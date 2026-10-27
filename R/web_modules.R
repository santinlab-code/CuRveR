web_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::fileInput(ns("files"),"Importer des fichiers",multiple=TRUE,accept=c(".csv",".tsv",".txt",".xls",".xlsx")),
    shiny::selectInput(ns("file"),"Fichier à analyser",choices=NULL),shiny::selectInput(ns("od"),"Feuille OD",NULL), shiny::selectInput(ns("mch"),"Feuille signal",NULL),
    shiny::selectInput(ns("time"),"Colonne temps",choices=NULL),
    shiny::helpText("Les puits sont détectés automatiquement, sans les colonnes de température. Les temps Excel sont exprimés en minutes écoulées depuis la première mesure de chaque feuille."),
    shiny::actionButton(ns("pair"),"Importer OD + signal",class="btn-primary"),
    shiny::actionButton(ns("reset"),"Nouvelle analyse"),shiny::textOutput(ns("status")),
    shiny::radioButtons(ns("preview_signal"),"Afficher dans le tableau",choices=c("OD600"="od","Signal"="mch"),selected="od",inline=TRUE),shiny::tableOutput(ns("preview")))
}
web_import_server <- function(id) shiny::moduleServer(id,function(input,output,session) {
  data <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$files, {
    shiny::updateSelectInput(session,"file",choices=stats::setNames(seq_len(nrow(input$files)),input$files$name))
  })
  shiny::observeEvent(input$file, {
    shiny::req(input$files,input$file)
    f <- input$files[as.integer(input$file),]
    sheets <- tryCatch(if (tolower(tools::file_ext(f$name)) %in% c("xls","xlsx")) readxl::excel_sheets(f$datapath) else character(),error=function(e) character())
    shiny::updateSelectInput(session,"od",choices=sheets,selected=if("OD600" %in% sheets) "OD600" else sheets[1])
    shiny::updateSelectInput(session,"mch",choices=sheets,selected=if("mCh signal" %in% sheets) "mCh signal" else tail(sheets,1))
  })
  raw <- shiny::reactive({
    shiny::req(input$files,input$file)
    f <- input$files[as.integer(input$file),]
    ext <- tolower(tools::file_ext(f$name))
    shiny::validate(shiny::need(ext %in% c("csv","tsv","txt","xls","xlsx"),"Format non pris en charge."))
    tryCatch({
      d <- if (ext %in% c("xls","xlsx")) {
        shiny::req(input$od); readxl::read_excel(f$datapath,sheet=input$od)
      } else if (ext=="csv") readr::read_csv(f$datapath,show_col_types=FALSE) else
        readr::read_delim(f$datapath,delim=if(ext=="tsv") "\t" else NULL,show_col_types=FALSE)
      shiny::validate(shiny::need(nrow(d)>0 && ncol(d)>1,"Fichier vide ou séparateur incorrect.")); d
    },error=function(e) shiny::validate(shiny::need(FALSE,paste("Lecture impossible :",conditionMessage(e)))))
  })
  shiny::observeEvent(raw(),{
    columns <- names(raw())
    shiny::updateSelectInput(session,"time",choices=columns,selected=columns[1])
  })
  output$preview <- shiny::renderTable({
    d <- raw(); shiny::req(input$time)
    if (identical(input$preview_signal,"mch")) {
      shiny::req(input$files,input$file,input$mch)
      f <- input$files[as.integer(input$file),]
      if(tolower(tools::file_ext(f$name)) %in% c("xls","xlsx"))
        d <- readxl::read_excel(f$datapath,sheet=input$mch)
    }
    shiny::validate(shiny::need(input$time %in% names(d),"Colonne temps absente de cette feuille."))
    if (input$time %in% names(d) && inherits(d[[input$time]],c("POSIXt","Date")))
      d[[input$time]] <- as.numeric(difftime(d[[input$time]],d[[input$time]][1],units="mins"))
    utils::head(d[c(input$time,names(d)[grepl("^[A-Za-z][0-9]+$",names(d))])],8)
  }, digits=3)
  shiny::observeEvent(input$pair,{
    tryCatch({
      shiny::req(input$files,input$file,input$time)
      f<-input$files[as.integer(input$file),]
      excel<-tolower(tools::file_ext(f$name)) %in% c("xls","xlsx")
      if(excel) shiny::req(input$od,input$mch)
      if(excel && input$od==input$mch) stop("Choisissez deux feuilles distinctes.")
      f<-input$files[as.integer(input$file),]
      signals<-lapply(if(excel) c(input$od,input$mch) else f$name,function(sheet){
        d<-if(excel) readxl::read_excel(f$datapath,sheet=sheet) else raw()
        if(!input$time %in% names(d)) stop(paste("Colonne temps absente :",sheet))
        if(inherits(d[[input$time]],c("POSIXt","Date")))
          d[[input$time]]<-as.numeric(difftime(d[[input$time]],d[[input$time]][1],units="mins"))
        wells<-names(d)[grepl("^[A-Za-z][0-9]+$",names(d))]
        web_prepare(d,input$time,wells,sheet)
      })
      combined <- dplyr::bind_rows(signals)
      attr(combined,"od_signal") <- if(excel) input$od else f$name
      attr(combined,"fluorescence_signal") <- if(excel) input$mch else NULL
      data(combined)
      shiny::showNotification("OD et signal importés. Vous pouvez définir les conditions.")
    },error=function(e)shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  shiny::observeEvent(input$reset,data(NULL))
  output$status <- shiny::renderText(if(is.null(data())) "Aucun signal ajouté." else paste(length(unique(data()$signal)),"signal(s),",length(unique(data()$well)),"puits."))
  data
})

web_conditions_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths=c(3,9),
    bslib::card(shiny::selectInput(ns("global_blank"),"Groupe blanc commun",choices=c("Aucun"="")),shiny::helpText("Soustrait à toutes les conditions, séparément pour chaque signal. Le triplet blanc est exclu des ajustements.")),
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
  effective <- shiny::reactive({
    blank <- conditions()[[blank_name()]]
    web_apply_global_blank(conditions(),blank$replicates %||% character())
  })
  shiny::observeEvent(data(),{
    groups<-web_triplet_conditions(unique(data()$well))
    blank_name("");selection(if(length(groups))names(groups)[1] else "");conditions(groups)
    shiny::updateSelectizeInput(session,"reps",choices=unique(data()$well))
  },ignoreNULL=FALSE)
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
      shiny::actionButton(ns("apply"),"Appliquer la fenêtre")),
    shiny::div(style="flex:3 1 400px;min-width:280px;height:500px;min-height:500px;",
      shiny::plotOutput(ns("plot"),height="500px",fill=FALSE,
        brush=shiny::brushOpts(ns("brush"),direction="x"))))
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
  shiny::observeEvent(input$brush,{shiny::updateNumericInput(session,"left",value=input$brush$xmin);shiny::updateNumericInput(session,"right",value=input$brush$xmax)})
  shiny::observeEvent(input$apply,{
    shiny::req(windows(),is.finite(input$left),is.finite(input$right))
    if(input$left>=input$right){shiny::showNotification("Le début doit précéder la fin.",type="error");return()}
    w<-windows();i<-if(input$all) rep(TRUE,nrow(w)) else w$signal==input$signal & w$well==input$well
    w$left[i]<-input$left;w$right[i]<-input$right;windows(w)
  })
  output$plot<-shiny::renderPlot({shiny::req(data(),windows(),input$signal,input$well)
    d<-data()
    if(!is.null(conditions)) d<-web_blank_correct(d,attr(conditions(),"global_blank"),windows())
    d<-d[d$signal==input$signal & d$well==input$well,]
    ggplot2::ggplot(d,ggplot2::aes(Time,value))+ggplot2::geom_line(color="#00284B")+ggplot2::geom_point()+ggplot2::geom_vline(xintercept=c(input$left,input$right),linetype=2)+ggplot2::theme_minimal()
  },height=500,res=96,execOnResize=TRUE,width=function(){
    width<-session$clientData$output_plot_width
    if(is.null(width) || !is.finite(width) || width<100) 800 else width
  })
  windows
})
