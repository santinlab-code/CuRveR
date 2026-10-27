modern_ui <- function() bslib::page_navbar(
  title=shiny::tagList(shiny::div(style="display:inline-block;margin-right:14px",shiny::tags$select(id="language",class="shiny-input-select form-select",`aria-label`="Language / Langue",shiny::tags$option(value="en",selected="selected","EN"),shiny::tags$option(value="fr","FR"))),"CuRveR"),id="page",theme=bslib::bs_theme(version=5, bootswatch="flatly",primary="#137c8b",base_font="system-ui"),
  header=shiny::tagList(shiny::tags$script(src="curver/language.js"),shiny::tags$style(shiny::HTML("body{background:#f3f6f8}.bslib-card{border:0;box-shadow:0 2px 12px #1232}.navbar{border-bottom:3px solid #137c8b}.shiny-output-error-validation{color:#9c3c19}.form-control{border-radius:6px}table{font-variant-numeric:tabular-nums}"))),
  sidebar=bslib::sidebar(title="Votre analyse",width=290,
    shiny::p("Import → Conditions → Fenêtres → Résultats"),
    shiny::textOutput("counts"),shiny::hr(),
    shiny::selectInput("loss","Fonction de coût",c("LAD","OLS","Huber","Tukey","Andrew","Jaeckel")),
    shiny::numericInput("seed","Graine reproductible",42,min=0,max=.Machine$integer.max,step=1),
    shiny::p("Modèle : Richard · Optimisation : évolution différentielle"),
    shiny::actionButton("run","Lancer l’analyse",class="btn-primary"),shiny::actionButton("cancel","Annuler"),
    shiny::textOutput("status"),shiny::p("Excel times are converted to minutes. Blanks are subtracted using the historical calculation.")),
  bslib::nav_panel("Import",bslib::card(bslib::card_header("Fichiers et préparation"),web_import_ui("import"))),
  bslib::nav_panel("Aperçu",bslib::card(fill=FALSE,
    bslib::card_body(fillable=FALSE,shiny::div(style="height:900px;min-height:900px;flex-shrink:0;",shiny::plotOutput("overview",height="900px",fill=FALSE))),
    bslib::card_footer(shiny::downloadButton("overview_export","Exporter l’aperçu PDF")))),
  bslib::nav_panel("Conditions",bslib::card(bslib::card_header("Réplicats et blancs"),web_conditions_ui("conditions"))),
  bslib::nav_panel("Fenêtres",bslib::card(bslib::card_header("Sélectionnez une zone du graphique ou saisissez ses bornes"),web_windows_ui("windows"))),
  bslib::nav_panel("Résultats",bslib::card(
    shiny::selectInput("signal","Signal",NULL),shiny::checkboxGroupInput("conditions","Conditions",NULL,inline=TRUE),
    bslib::navset_card_tab(
      bslib::nav_panel("Courbes",shiny::selectInput("plot_type","Vue",c("Courbes"="curves","Paramètres"="metrics","Annotations"="annotations")),
        shiny::selectInput("metric","Paramètre",c("r_max","p_max","p_min","s")),
        shiny::checkboxGroupInput("features","Afficher",choices=c("Points"="points","Ajustement"="fit","Moyenne"="mean","Écart-type (halo)"="sd","Écart-type (barres)"="bars","p_max","p_min","s","r_max"),selected=c("points","fit","mean","sd"),inline=TRUE),
        shiny::plotOutput("result_plot",height="550px",brush=shiny::brushOpts("zoom",direction="x")),
        shiny::checkboxInput("use_zoom","Zoom sur la zone sélectionnée",FALSE)),
      bslib::nav_panel("Paramètres",shiny::tableOutput("parameters")),
      bslib::nav_panel("Qualité",shiny::tableOutput("performance")),
      bslib::nav_panel("Données",shiny::p("Aperçu limité aux 200 premières lignes ; export complet disponible."),shiny::tableOutput("data"))))),
  bslib::nav_panel("Export",bslib::card(bslib::card_header("Résultats et reproductibilité"),
    shiny::downloadButton("csv","Données CSV"),shiny::downloadButton("params","Paramètres CSV"),shiny::downloadButton("quality","Qualité CSV"),
    shiny::downloadButton("settings","Configuration YAML"),
    shiny::selectInput("format","Format de figure",c("pdf","svg","png","jpeg","tiff","bmp","eps","ps")),
    shiny::numericInput("width","Largeur (cm)",18,min=1,max=100),shiny::numericInput("height","Hauteur (cm)",12,min=1,max=100),
    shiny::numericInput("dpi","Résolution (dpi)",300,min=72,max=1200),shiny::downloadButton("figure","Exporter la figure"))),
  bslib::nav_panel("Aide",bslib::card(shiny::h3("Un parcours guidé"),
    shiny::p("Ajoutez chaque signal, affectez les puits aux conditions, puis choisissez les fenêtres avant de lancer le calcul. Une erreur ne remplace pas les résultats précédents."),
    shiny::p("Les paramètres de calcul et la graine sont exportables. Les fichiers restent dans la session ; aucune sauvegarde permanente n’est effectuée."),
    shiny::p("L’interface historique reste accessible avec CuRveR::run_curver(legacy = TRUE). Les options historiques incomplètes ne sont pas présentées comme des modèles validés dans cette interface.")))
)

modern_server <- function(input,output,session) {
  data <- web_import_server("import")
  conditions <- web_conditions_server("conditions",data)
  windows <- web_windows_server("windows",data)
  result <- shiny::reactiveVal(NULL); job <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("Prêt. Ajoutez un signal pour commencer.")
  revision <- shiny::reactiveVal(0L); completed <- shiny::reactiveVal(-1L); launched <- 0L
  shiny::observeEvent(list(data(),conditions(),windows(),input$loss,input$seed),{
    revision(shiny::isolate(revision())+1L)
  },ignoreNULL=FALSE)
  output$counts <- shiny::renderText({d<-data();if(is.null(d))"Aucune donnée" else paste(length(unique(d$signal)),if(identical(input$language,"fr")) "signaux ·" else "signals ·",length(unique(d$well)),if(identical(input$language,"fr")) "puits ·" else "wells ·",length(conditions()),"conditions")})
  output$status <- shiny::renderText({paste(status(),if(!is.null(result()) && revision()!=completed())"Paramètres modifiés : relancez l’analyse avant export." else "")})
  shiny::observeEvent(input$run,{
    if(!is.null(job())){shiny::showNotification("Une analyse est déjà en cours.");return()}
    tryCatch({
      shiny::req(data(),windows());web_validate_conditions(conditions(),unique(data()$well))
      if(is.null(input$seed) || !is.finite(input$seed) || input$seed<0 || input$seed>.Machine$integer.max)stop("Graine invalide.")
      launched <<- revision()
      job(callr::r_bg(function(data,conditions,windows,loss,seed){
        CuRveR:::web_analyze(data,conditions,windows,loss,seed)
      },args=list(data=data(),conditions=conditions(),windows=windows(),loss=input$loss,seed=as.integer(input$seed)),libpath=.libPaths(),supervise=TRUE))
      status("Analyse en cours dans un processus séparé…")
    },error=function(e){status("Analyse non lancée.");shiny::showNotification(conditionMessage(e),type="error",duration=NULL)})
  })
  shiny::observe({
    process<-job();if(is.null(process))return();shiny::invalidateLater(500,session)
    if(!process$is_alive()){
      tryCatch({r<-process$get_result();result(r);completed(launched);status("Analyse terminée.")
        shiny::updateSelectInput(session,"signal",choices=unique(r$data$signal))
        shiny::updateCheckboxGroupInput(session,"conditions",choices=unique(r$data$condition),selected=unique(r$data$condition))
      },error=function(e){status("Échec de l’analyse ; résultats précédents conservés.");shiny::showNotification(conditionMessage(e),type="error",duration=NULL)})
      job(NULL)
    }
  })
  shiny::observeEvent(input$cancel,{if(!is.null(job())){job()$kill();job(NULL);status("Analyse annulée.")}})
  session$onSessionEnded(function(){p<-shiny::isolate(job());if(!is.null(p))p$kill()})
  overview <- shiny::reactive({shiny::req(data());web_overview_plot(data(),input$language %||% "en")})
  output$overview <- shiny::renderPlot({
    shiny::validate(shiny::need(!is.null(data()), if(identical(input$language,"fr")) "Importez les données avec le bouton Importer OD + signal." else "Import data using the Import OD + signal button first."))
    tryCatch(print(overview()), error=function(e) {
      shiny::validate(shiny::need(FALSE,paste(if(identical(input$language,"fr")) "Impossible de tracer l’aperçu :" else "Cannot render overview:",conditionMessage(e))))
    })
  },res=96,height=900,width=function(){
    width <- session$clientData$output_overview_width
    if(is.null(width) || !is.finite(width) || width<100) 1200 else width
  },execOnResize=TRUE)
  shiny::observeEvent(input$page,{
    if(identical(input$page,"Aperçu")) session$sendCustomMessage("curver-overview-resize",list())
  })
  plot <- shiny::reactive({shiny::req(result(),input$signal,input$conditions)
    p<-web_plot(result(),input$signal,input$conditions,input$plot_type,input$features,input$metric)
    if(isTRUE(input$use_zoom) && !is.null(input$zoom) && input$plot_type!="metrics")p<-p+ggplot2::coord_cartesian(xlim=c(input$zoom$xmin,input$zoom$xmax))
    p
  })
  output$result_plot<-shiny::renderPlot(plot())
  output$parameters<-shiny::renderTable({shiny::req(result());result()$parameters},digits=6)
  output$performance<-shiny::renderTable({shiny::req(result());result()$performance},digits=4)
  output$data<-shiny::renderTable({shiny::req(result());utils::head(result()$data,200)})
  ready<-function(){shiny::req(result(),is.null(job()));if(revision()!=completed())stop("Résultats obsolètes : relancez l’analyse.")}
  output$csv<-shiny::downloadHandler(function()"curver_data.csv",function(file){ready();readr::write_csv(result()$data,file)})
  output$params<-shiny::downloadHandler(function()"curver_parameters.csv",function(file){ready();readr::write_csv(result()$parameters,file)})
  output$quality<-shiny::downloadHandler(function()"curver_quality.csv",function(file){ready();readr::write_csv(result()$performance,file)})
  output$settings<-shiny::downloadHandler(function()"curver_settings.yaml",function(file){ready();yaml::write_yaml(result()$settings,file)})
  output$figure<-shiny::downloadHandler(function()paste0("curver.",input$format),function(file){ready();ggplot2::ggsave(file,plot(),device=input$format,width=input$width,height=input$height,units="cm",dpi=input$dpi,limitsize=TRUE)})
  output$overview_export<-shiny::downloadHandler(function()"overview.pdf",function(file)ggplot2::ggsave(file,overview(),device="pdf",width=18,height=12,units="cm"))
}
