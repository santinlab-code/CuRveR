web_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::fileInput(ns("files"),"Importer des fichiers",multiple=TRUE,accept=c(".csv",".tsv",".txt",".xls",".xlsx")),
    shiny::selectInput(ns("file"),"Fichier",choices=NULL),shiny::selectInput(ns("sheet"),"Feuille Excel",choices=NULL),
    shiny::textInput(ns("signal"),"Nom du signal","OD600"),
    shiny::selectInput(ns("time"),"Colonne temps",choices=NULL),
    shiny::selectizeInput(ns("wells"),"Colonnes de mesures",choices=NULL,multiple=TRUE),
    shiny::actionButton(ns("add"),"Ajouter ce signal",class="btn-primary"),
    shiny::actionButton(ns("reset"),"Nouvelle analyse"),shiny::textOutput(ns("status")),shiny::tableOutput(ns("preview")))
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
    shiny::updateSelectInput(session,"sheet",choices=sheets)
  })
  raw <- shiny::reactive({
    shiny::req(input$files,input$file)
    f <- input$files[as.integer(input$file),]
    ext <- tolower(tools::file_ext(f$name))
    shiny::validate(shiny::need(ext %in% c("csv","tsv","txt","xls","xlsx"),"Format non pris en charge."))
    tryCatch({
      d <- if (ext %in% c("xls","xlsx")) {
        shiny::req(input$sheet); readxl::read_excel(f$datapath,sheet=input$sheet)
      } else if (ext=="csv") readr::read_csv(f$datapath,show_col_types=FALSE) else
        readr::read_delim(f$datapath,delim=if(ext=="tsv") "\t" else NULL,show_col_types=FALSE)
      shiny::validate(shiny::need(nrow(d)>0 && ncol(d)>1,"Fichier vide ou séparateur incorrect.")); d
    },error=function(e) shiny::validate(shiny::need(FALSE,paste("Lecture impossible :",conditionMessage(e)))))
  })
  shiny::observeEvent(raw(),{
    columns <- names(raw())
    shiny::updateSelectInput(session,"time",choices=columns,selected=columns[1])
    shiny::updateSelectizeInput(session,"wells",choices=columns,selected=columns[grepl("^[A-Za-z][0-9]+$",columns)])
  })
  output$preview <- shiny::renderTable(utils::head(raw(),8))
  shiny::observeEvent(input$add,{
    tryCatch({
      d <- web_prepare(raw(),input$time,input$wells,input$signal)
      old <- data()
      if (!is.null(old) && input$signal %in% old$signal) stop("Ce signal existe déjà. Utilisez un autre nom.")
      data(dplyr::bind_rows(old,d))
    },error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  shiny::observeEvent(input$reset,data(NULL))
  output$status <- shiny::renderText(if(is.null(data())) "Aucun signal ajouté." else paste(length(unique(data()$signal)),"signal(s),",length(unique(data()$well)),"puits."))
  data
})

web_conditions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::selectInput(ns("selected"),"Condition",NULL),shiny::textInput(ns("name"),"Nom","Témoin"),
    shiny::selectizeInput(ns("reps"),"Réplicats",NULL,multiple=TRUE),shiny::selectizeInput(ns("blanks"),"Blancs",NULL,multiple=TRUE),
    shiny::actionButton(ns("save"),"Enregistrer / renommer",class="btn-primary"),shiny::actionButton(ns("new"),"Nouvelle condition"),
    shiny::actionButton(ns("delete"),"Supprimer"),shiny::fileInput(ns("yaml"),"Importer une configuration YAML",accept=c(".yml",".yaml")),
    shiny::downloadButton(ns("download"),"Exporter YAML"),shiny::tableOutput(ns("summary")))
}
web_conditions_server <- function(id,data) shiny::moduleServer(id,function(input,output,session){
  conditions <- shiny::reactiveVal(list())
  shiny::observeEvent(data(),{ conditions(list()); wells<-unique(data()$well)
    shiny::updateSelectizeInput(session,"reps",choices=wells);shiny::updateSelectizeInput(session,"blanks",choices=wells)
  },ignoreNULL=FALSE)
  shiny::observeEvent(conditions(),shiny::updateSelectInput(session,"selected",choices=c("Nouvelle"="",stats::setNames(names(conditions()),names(conditions())))))
  shiny::observeEvent(input$selected,{
    c <- conditions()[[input$selected]]
    if(!is.null(c)){shiny::updateTextInput(session,"name",value=input$selected);shiny::updateSelectizeInput(session,"reps",selected=unlist(c$replicates));shiny::updateSelectizeInput(session,"blanks",selected=unlist(c$blanks))}
  })
  shiny::observeEvent(input$new, {shiny::updateSelectInput(session,"selected",selected="");shiny::updateTextInput(session,"name",value="");shiny::updateSelectizeInput(session,"reps",selected=character());shiny::updateSelectizeInput(session,"blanks",selected=character())})
  shiny::observeEvent(input$save,{
    tryCatch({
      name <- trimws(input$name); c <- conditions()
      if(!nzchar(name)) stop("Nommez la condition.")
      if(name %in% names(c) && name != input$selected) stop("Ce nom existe déjà.")
      candidate <- stats::setNames(list(list(replicates=input$reps,blanks=input$blanks)),name)
      web_validate_conditions(candidate,unique(data()$well))
      if(nzchar(input$selected)) c[[input$selected]] <- NULL
      c[[name]] <- candidate[[1]];conditions(c)
    },error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })
  shiny::observeEvent(input$delete,{c<-conditions();c[[input$selected]]<-NULL;conditions(c)})
  shiny::observeEvent(input$yaml,{
    tryCatch({c<-yaml::read_yaml(input$yaml$datapath,eval.expr=FALSE);web_validate_conditions(c,unique(data()$well));conditions(c)},error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
  })
  output$download <- shiny::downloadHandler(filename=function()"conditions.yaml",content=function(file)yaml::write_yaml(conditions(),file))
  output$summary <- shiny::renderTable({c<-conditions();if(!length(c))return(NULL);data.frame(Condition=names(c),Réplicats=vapply(c,function(x)paste(unlist(x$replicates),collapse=", "),""),Blancs=vapply(c,function(x)paste(unlist(x$blanks),collapse=", "),""))})
  conditions
})

web_windows_ui <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(shiny::selectInput(ns("signal"),"Signal",NULL),shiny::selectInput(ns("well"),"Puits",NULL),
    shiny::checkboxInput(ns("all"),"Appliquer à tous les puits et signaux",FALSE),
    shiny::numericInput(ns("left"),"Début",0),shiny::numericInput(ns("right"),"Fin",24),
    shiny::actionButton(ns("apply"),"Appliquer la fenêtre"),
    shiny::plotOutput(ns("plot"),brush=shiny::brushOpts(ns("brush"),direction="x")),shiny::tableOutput(ns("windows")))
}
web_windows_server <- function(id,data) shiny::moduleServer(id,function(input,output,session){
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
    d<-data();d<-d[d$signal==input$signal & d$well==input$well,]
    ggplot2::ggplot(d,ggplot2::aes(Time,value))+ggplot2::geom_line(color="#137c8b")+ggplot2::geom_point()+ggplot2::geom_vline(xintercept=c(input$left,input$right),linetype=2)+ggplot2::theme_minimal()
  })
  output$windows<-shiny::renderTable(windows())
  windows
})
