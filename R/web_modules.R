web_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::fileInput(ns("files"),"Importer des fichiers",multiple=TRUE,accept=c(".csv",".tsv",".txt",".xls",".xlsx")),
    shiny::selectInput(ns("file"),"Fichier à analyser",choices=NULL),
    shiny::conditionalPanel("output.excel === 'true'",ns=ns,
      shiny::selectInput(ns("od"),"Feuille OD",NULL),
      shiny::selectInput(ns("mch"),"Feuille de signal facultative",choices=c("Aucun"=""),selectize=FALSE)),
    shiny::selectInput(ns("time"),"Colonne temps",choices=NULL),
    shiny::helpText("Les puits sont détectés automatiquement, sans les colonnes de température. Les temps Excel sont exprimés en minutes écoulées depuis la première mesure de chaque feuille."),
    shiny::actionButton(ns("pair"),"Importer les données",class="btn-primary"),
    shiny::actionButton(ns("reset"),"Nouvelle analyse"),shiny::textOutput(ns("status")),
    shiny::conditionalPanel("output.excel === 'true' && input.mch && input.mch !== input.od",ns=ns,
      shiny::radioButtons(ns("preview_signal"),"Afficher dans le tableau",choices=c("OD600"="od","Signal"="mch"),selected="od",inline=TRUE)),
    shiny::tableOutput(ns("preview")))
}
web_import_server <- function(id) shiny::moduleServer(id,function(input,output,session) {
  data <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$files, {
    shiny::updateSelectInput(session,"file",choices=stats::setNames(seq_len(nrow(input$files)),input$files$name))
  })
  file <- shiny::reactive({
    shiny::req(input$files,input$file)
    index <- as.integer(input$file)
    shiny::req(index %in% seq_len(nrow(input$files)))
    input$files[index,]
  })
  extension <- shiny::reactive(tolower(tools::file_ext(file()$name)))
  excel <- shiny::reactive(extension() %in% c("xls","xlsx"))
  output$excel <- shiny::renderText(if (shiny::isTruthy(input$file) && excel()) "true" else "false")
  shiny::outputOptions(output,"excel",suspendWhenHidden=FALSE)
  sheets <- shiny::reactive({
    if (excel()) readxl::excel_sheets(file()$datapath) else character()
  })
  shiny::observeEvent(file(), {
    available <- tryCatch(sheets(),error=function(e) character())
    shiny::updateSelectInput(session,"od",choices=available,
      selected=if ("OD600" %in% available) "OD600" else head(available,1))
    shiny::updateSelectInput(session,"mch",choices=c("Aucun"="",available),selected="")
  })
  raw <- shiny::reactive({
    f <- file()
    ext <- extension()
    shiny::validate(shiny::need(ext %in% c("csv","tsv","txt","xls","xlsx"),"Format non pris en charge."))
    d <- tryCatch({
      if (excel()) {
        shiny::req(input$od %in% sheets())
        readxl::read_excel(f$datapath,sheet=input$od)
      } else if (ext=="csv") readr::read_csv(f$datapath,show_col_types=FALSE) else
        readr::read_delim(f$datapath,delim=if(ext=="tsv") "\t" else NULL,show_col_types=FALSE)
    },shiny.silent.error=function(e) stop(e),
      error=function(e) shiny::validate(shiny::need(FALSE,paste("Lecture impossible :",conditionMessage(e)))))
    shiny::validate(shiny::need(nrow(d)>0 && ncol(d)>1,"Fichier vide ou séparateur incorrect."))
    d
  })
  secondary_sheet <- shiny::reactive({
    sheet <- input$mch %||% ""
    if (excel() && nzchar(sheet)) sheet else NULL
  })
  signal_raw <- shiny::reactive(readxl::read_excel(file()$datapath,sheet=secondary_sheet()))
  elapsed_time <- function(d,time) {
    if (inherits(d[[time]],c("POSIXt","Date")))
      d[[time]] <- as.numeric(difftime(d[[time]],d[[time]][1],units="mins"))
    d
  }
  well_columns <- function(d) names(d)[grepl("^[A-Za-z][0-9]+$",names(d))]
  shiny::observeEvent(raw(),{
    columns <- names(raw())
    shiny::updateSelectInput(session,"time",choices=columns,selected=columns[1])
  })
  output$preview <- shiny::renderTable({
    d <- raw(); shiny::req(input$time)
    sheet <- secondary_sheet()
    if (identical(input$preview_signal,"mch") && !is.null(sheet) && sheet %in% sheets() && sheet != input$od)
      d <- signal_raw()
    shiny::validate(shiny::need(input$time %in% names(d),"Colonne temps absente de cette feuille."))
    d <- elapsed_time(d,input$time)
    utils::head(d[c(input$time,well_columns(d))],8)
  }, digits=3)
  shiny::observeEvent(input$pair,{
    tryCatch({
      shiny::req(input$time)
      od <- raw()
      od_name <- if (excel()) input$od else file()$name
      sheet <- secondary_sheet()
      prepare <- function(d,signal) {
        if (!input$time %in% names(d)) stop(paste("Colonne temps absente :",signal))
        web_prepare(elapsed_time(d,input$time),input$time,well_columns(d),signal)
      }
      combined <- prepare(od,od_name)
      if (!is.null(sheet)) {
        if (sheet == od_name) stop("Choisissez deux feuilles distinctes.")
        combined <- dplyr::bind_rows(combined,prepare(signal_raw(),sheet))
      }
      attr(combined,"od_signal") <- od_name
      attr(combined,"fluorescence_signal") <- sheet
      data(combined)
      shiny::showNotification(if (is.null(sheet)) "OD importée. Vous pouvez définir les conditions."
        else "OD et signal importés. Vous pouvez définir les conditions.")
    },validation=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL),
      shiny.silent.error=function(e) NULL,
      error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  shiny::observeEvent(input$reset,data(NULL))
  output$status <- shiny::renderText(if(is.null(data())) "Aucun signal ajouté." else paste(length(unique(data()$signal)),"signal(s),",length(unique(data()$well)),"puits."))
  data
})

web_replicate_counts <- c(3L,4L,6L)

web_conditions_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths=c(3,9),
    bslib::card(
      shiny::radioButtons(ns("replicate_count"),"Nombre de réplicats techniques",
        choices=web_replicate_counts,selected=3,inline=TRUE),
      shiny::helpText("Changer ce nombre recrée les groupes par ligne et conserve le groupe blanc sélectionné."),
      shiny::selectInput(ns("global_blank"),"Groupe blanc commun",choices=c("Aucun"="")),
      shiny::helpText("Le groupe blanc est exclu des ajustements. Sa moyenne est soustraite à chaque condition pour chaque signal.")),
    shiny::tagList(shiny::selectInput(ns("selected"),"Condition",NULL),shiny::textInput(ns("name"),"Nom",""),
    shiny::selectizeInput(ns("reps"),"Réplicats",NULL,multiple=TRUE),
    shiny::helpText("Renommez le groupe et ajustez ses puits, puis enregistrez. Un puits ajouté est déplacé depuis son ancien groupe."),
    shiny::actionButton(ns("save"),"Enregistrer / renommer",class="btn-primary"),shiny::actionButton(ns("new"),"Nouvelle condition"),
    shiny::actionButton(ns("delete"),"Supprimer"),shiny::fileInput(ns("yaml"),"Importer une configuration YAML",accept=c(".yml",".yaml")),
    shiny::downloadButton(ns("download"),"Exporter YAML"),shiny::tableOutput(ns("summary"))))
}
web_conditions_server <- function(id,data) shiny::moduleServer(id,function(input,output,session){
  conditions <- shiny::reactiveVal(list())
  blank_name <- shiny::reactiveVal("")
  selection <- shiny::reactiveVal("")
  replicate_count <- shiny::reactiveVal(3L)
  effective <- shiny::reactive({
    blank <- conditions()[[blank_name()]]
    web_apply_global_blank(conditions(),blank$replicates %||% character())
  })
  replace_groups <- function(groups,blank="") {
    selection(if (length(groups)) names(groups)[1] else "")
    blank_name(blank)
    conditions(groups)
  }
  shiny::observeEvent(data(),{
    wells <- unique(data()$well)
    replace_groups(web_default_conditions(wells,replicate_count()))
    shiny::updateSelectizeInput(session,"reps",choices=wells)
  },ignoreNULL=FALSE,priority=1)
  shiny::observeEvent(input$replicate_count,{
    if (!input$replicate_count %in% web_replicate_counts) return()
    count <- as.integer(input$replicate_count)
    if (count == replicate_count()) return()
    replicate_count(count)
    blank <- blank_name()
    blank_group <- conditions()[[blank]]
    wells <- setdiff(unique(data()$well),blank_group$replicates)
    groups <- web_default_conditions(wells,count)
    if (!is.null(blank_group)) {
      names(groups) <- make.unique(c(blank,names(groups)),sep=" ")[-1L]
      groups[[blank]] <- blank_group
    }
    replace_groups(groups,blank)
  })
  shiny::observeEvent(list(conditions(),blank_name()),{
    names<-names(conditions())
    selected<-shiny::isolate(selection())
    if(nzchar(selected) && !selected %in% names) selected<-if(length(names))names[1] else ""
    shiny::updateSelectInput(session,"selected",choices=c(stats::setNames(names,names),"Nouvelle"=""),selected=selected)
    shiny::updateSelectInput(session,"global_blank",choices=c("Aucun"="",stats::setNames(names,names)),selected=blank_name())
    group<-conditions()[[selected]]
    shiny::updateTextInput(session,"name",value=selected)
    shiny::updateSelectizeInput(session,"reps",selected=group$replicates %||% character())
  })
  shiny::observeEvent(input$global_blank,{
    if(input$global_blank=="" || input$global_blank %in% names(conditions())) blank_name(input$global_blank)
  })
  shiny::observeEvent(input$selected,{
    selection(input$selected)
    group<-conditions()[[input$selected]]
    shiny::updateTextInput(session,"name",value=input$selected)
    shiny::updateSelectizeInput(session,"reps",selected=group$replicates %||% character())
  })
  shiny::observeEvent(input$new,{
    selection("");shiny::updateSelectInput(session,"selected",selected="")
    shiny::updateTextInput(session,"name",value="");shiny::updateSelectizeInput(session,"reps",selected=character())
  })
  shiny::observeEvent(input$save,{
    tryCatch({
      edit<-web_edit_group(conditions(),input$selected,trimws(input$name),input$reps,blank_name(),unique(data()$well))
      selection(trimws(input$name));blank_name(edit$blank);conditions(edit$groups)
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  })
  shiny::observeEvent(input$delete,{
    groups<-conditions();groups[[input$selected]]<-NULL
    if(blank_name()==input$selected)blank_name("")
    selection(if(length(groups))names(groups)[1] else "");conditions(groups)
  })
  shiny::observeEvent(input$yaml,{
    tryCatch({
      config<-web_read_group_config(yaml::read_yaml(input$yaml$datapath,eval.expr=FALSE),unique(data()$well))
      selection(names(config$groups)[1]);blank_name(config$blank);conditions(config$groups)
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error"))
  })
  output$download<-shiny::downloadHandler(filename=function()"conditions.yaml",content=function(file){
    yaml::write_yaml(list(version=2,blank=blank_name(),groups=conditions()),file)
  })
  output$summary<-shiny::renderTable({
    groups<-conditions();if(!length(groups))return(NULL)
    data.frame(Condition=names(groups),Replicates=vapply(groups,function(g)paste(g$replicates,collapse=", "),""),
      Blank=ifelse(names(groups)==blank_name(),"✓",""))
  })
  effective
})

web_windows_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::div(style="display:flex;flex-wrap:wrap;gap:24px;align-items:flex-start;",
    shiny::div(style="flex:1 1 240px;min-width:220px;",
      shiny::selectInput(ns("signal"),"Signal",NULL),shiny::selectInput(ns("well"),"Puits",NULL),
      shiny::checkboxInput(ns("all"),"Appliquer à tous les puits et signaux",FALSE),
      shiny::numericInput(ns("left"),"Début",0),shiny::numericInput(ns("right"),"Fin",24),
      shiny::helpText("La sélection graphique et les bornes saisies sont enregistrées automatiquement. Cocher la case applique immédiatement la fenêtre affichée à tous les puits et signaux.")),
    shiny::div(style="flex:3 1 400px;min-width:280px;height:500px;min-height:500px;",
      shiny::plotOutput(ns("plot"),height="500px",fill=FALSE,
        brush=shiny::brushOpts(ns("brush"),direction="x",resetOnNew=TRUE))))
}
web_windows_server <- function(id,data,conditions=NULL) shiny::moduleServer(id,function(input,output,session){
  windows<-shiny::reactiveVal(NULL)
  shiny::observeEvent(data(),{
    if(is.null(data())){windows(NULL);return()}
    d<-data();windows(dplyr::summarise(dplyr::group_by(d,signal,well),left=min(Time),right=max(Time),.groups="drop"))
    shiny::updateSelectInput(session,"signal",choices=unique(d$signal));shiny::updateSelectInput(session,"well",choices=unique(d$well))
  },ignoreNULL=FALSE)
  shiny::observeEvent(list(input$signal,input$well,windows()),{
    shiny::req(windows());w<-windows();w<-w[w$signal==input$signal & w$well==input$well,];shiny::req(nrow(w)==1)
    shiny::updateNumericInput(session,"left",value=w$left);shiny::updateNumericInput(session,"right",value=w$right)
  })
  shiny::observeEvent(list(input$signal,input$well),{
    # The brush belongs to the displayed series, never to the next selected well.
    session$resetBrush("brush")
  })
  shiny::observeEvent(input$brush,{
    shiny::req(input$signal,input$well,is.finite(input$brush$xmin),is.finite(input$brush$xmax))
    shiny::req(windows(),input$brush$xmin<input$brush$xmax)
    w<-windows()
    i<-if(isTRUE(input$all)) rep(TRUE,nrow(w)) else w$signal==input$signal & w$well==input$well
    w$left[i]<-input$brush$xmin;w$right[i]<-input$brush$xmax
    windows(w)
  })
  save_bounds <- function(left,right,signal,well,all=FALSE){
    shiny::req(windows(),signal,well,length(left)==1,length(right)==1,is.finite(left),is.finite(right))
    if(left>=right){shiny::showNotification("Le début doit précéder la fin.",type="error");return()}
    w<-windows();i<-if(isTRUE(all)) rep(TRUE,nrow(w)) else w$signal==signal & w$well==well
    if(!any(i))return()
    if(all(w$left[i]==left & w$right[i]==right))return()
    w$left[i]<-left;w$right[i]<-right;windows(w)
  }
  shiny::observeEvent(input$bounds_edit,{
    edit<-input$bounds_edit
    save_bounds(edit$left,edit$right,edit$signal,edit$well,isTRUE(edit$all))
  })
  shiny::observeEvent(input$all,{
    if(!isTRUE(input$all))return()
    shiny::req(windows(),input$signal,input$well)
    current<-windows();current<-current[current$signal==input$signal & current$well==input$well,]
    shiny::req(nrow(current)==1)
    save_bounds(current$left,current$right,input$signal,input$well,TRUE)
  })
  output$plot<-shiny::renderPlot({shiny::req(data(),windows(),input$signal,input$well)
    d<-data()
    if(!is.null(conditions)) d<-web_blank_correct(d,attr(conditions(),"global_blank"),windows())
    d<-d[d$signal==input$signal & d$well==input$well,]
    saved<-windows();saved<-saved[saved$signal==input$signal & saved$well==input$well,]
    ggplot2::ggplot(d,ggplot2::aes(Time,value))+
      ggplot2::annotate("rect",xmin=saved$left,xmax=saved$right,ymin=-Inf,ymax=Inf,fill="#00284B",alpha=.08)+
      ggplot2::geom_line(color="#00284B")+ggplot2::geom_point()+
      ggplot2::geom_vline(xintercept=c(saved$left,saved$right),linetype=2)+ggplot2::labs(x="Time (in min)")+ggplot2::theme_minimal()
  },height=500,res=96,execOnResize=TRUE,width=function(){
    width<-session$clientData$output_plot_width
    if(is.null(width) || !is.finite(width) || width<100) 800 else width
  })
  windows
})
