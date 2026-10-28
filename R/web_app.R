modern_ui <- function() bslib::page_navbar(
  title=shiny::tagList(shiny::div(style="display:inline-block;margin-right:14px",shiny::tags$select(id="language",class="shiny-input-select form-select",`aria-label`="Language / Langue",shiny::tags$option(value="en",selected="selected","EN"),shiny::tags$option(value="fr","FR"))),"CuRveR 2.0"),id="page",theme=bslib::bs_theme(version=5, bootswatch="flatly",primary="#00284B",success="#00284B",base_font="system-ui"),
  header=shiny::tagList(shiny::tags$script(src="curver/language.js"),shiny::tags$style(shiny::HTML("body{background:#f3f6f8}.bslib-card{border:0;box-shadow:0 2px 12px #1232}.navbar{border-bottom:3px solid #00284B}.navbar .nav-link{color:#fff!important}.navbar .nav-link.active,.navbar .nav-link[aria-selected=true]{color:#00284B!important;background:#fff!important;border-radius:6px;font-weight:600}.navbar .nav-link:focus-visible{outline:2px solid #fff;outline-offset:3px}.shiny-output-error-validation{color:#9c3c19}.form-control{border-radius:6px}table{font-variant-numeric:tabular-nums}#conditions .shiny-options-group{display:flex;flex-wrap:wrap;gap:8px 18px}#conditions .form-check{margin:0}"))),
  sidebar=bslib::sidebar(title="Votre analyse",width=290,
    shiny::p("Import → Conditions → Fenêtres → Résultats"),
    shiny::textOutput("counts"),shiny::hr(),
    shiny::textOutput("status"),shiny::uiOutput("correction_description")),
  bslib::nav_panel("Import",bslib::card(bslib::card_header("Fichiers et préparation"),web_import_ui("import"))),
  bslib::nav_panel("Aperçu",bslib::card(fill=FALSE,
    bslib::card_body(fillable=FALSE,shiny::uiOutput("overview_container")),
    bslib::card_footer(shiny::downloadButton("overview_export","Exporter l’aperçu PDF")))),
  bslib::nav_panel("Conditions",bslib::card(bslib::card_header("Réplicats et blancs"),web_conditions_ui("conditions"))),
  bslib::nav_panel("Fenêtres",bslib::card(fill=FALSE,bslib::card_header("Sélectionnez une zone du graphique ou saisissez ses bornes"),bslib::card_body(fillable=FALSE,web_windows_ui("windows")))),
  bslib::nav_panel("Résultats",bslib::card(fill=FALSE,
    bslib::card_header("Analyse — Modèle de Richard (4 paramètres)"),
    bslib::card_body(fillable=FALSE,
      shiny::p("Modèle : Richard · Optimisation : évolution différentielle et raffinement borné · Coût : LAD"),
      shiny::div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:16px;",
        shiny::actionButton("run","Lancer l’analyse",class="btn-primary"),shiny::actionButton("cancel","Annuler")),
      shiny::div(style="display:flex;gap:24px;flex-wrap:wrap;align-items:center;",
        shiny::div(style="flex:1 1 220px;",shiny::selectInput("signal","Signal",NULL,width="100%")),
        shiny::div(style="flex:2 1 360px;",shiny::radioButtons("plot_type","Vue",choices=c("Courbes"="curves","Paramètres"="metrics","Annotations"="annotations"),selected="curves",inline=TRUE))),
      shiny::checkboxGroupInput("conditions","Conditions",choices=NULL,inline=TRUE),
      shiny::conditionalPanel("input.plot_type == 'metrics'",shiny::selectInput("metric","Paramètre",c("r_max","p_max","p_min","s"))),
      shiny::tags$details(shiny::tags$summary("Options du graphique"),
        shiny::checkboxGroupInput("features","Afficher",choices=c("Points"="points","Ajustement"="fit","Moyenne"="mean","Écart-type (halo)"="sd","Écart-type (barres)"="bars","p_max","p_min","s","r_max"),selected=c("points","fit","mean","sd"),inline=TRUE),
        shiny::checkboxInput("use_zoom","Zoom sur la zone sélectionnée",FALSE),
        shiny::checkboxInput("show_negative","Afficher les valeurs corrigées négatives",FALSE)),
      shiny::div(style="height:550px;min-height:550px;flex-shrink:0;",shiny::plotOutput("result_plot",height="550px",fill=FALSE,brush=shiny::brushOpts("zoom",direction="x"))),
      shiny::div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin:8px 0 16px;",
        shiny::selectInput("format",NULL,c("PNG"="png","PDF"="pdf","SVG"="svg","JPEG"="jpeg","TIFF"="tiff","EPS"="eps","PS"="ps","BMP"="bmp"),selected="png",width="110px"),
        shiny::downloadButton("figure","Enregistrer le graphique",class="btn-sm"),
        shiny::tags$details(shiny::tags$summary("Options de l’image"),
          shiny::numericInput("width","Largeur (cm)",18,min=1,max=100),shiny::numericInput("height","Hauteur (cm)",12,min=1,max=100),shiny::numericInput("dpi","Résolution (dpi)",300,min=72,max=1200))),
      shiny::textOutput("negative_note"),
      shiny::tags$details(shiny::tags$summary("Valeurs résumées et qualité de l’ajustement"),
      shiny::div(style="overflow:auto;max-height:420px;",shiny::tableOutput("parameters"))),
      shiny::tags$details(shiny::tags$summary("Données complètes"),
      shiny::div(style="display:flex;gap:20px;flex-wrap:wrap;",
        shiny::selectInput("data_size","Lignes par page",c(50,100,200),selected=50),
        shiny::numericInput("data_page","Page",1,min=1,step=1)),
      shiny::textOutput("data_count"),
      shiny::div(style="overflow:auto;max-height:500px;",shiny::tableOutput("data")))))),
  bslib::nav_panel("Export",bslib::card(fill=FALSE,bslib::card_header("Tableaux de résultats"),
    bslib::card_body(fillable=FALSE,
      shiny::selectInput("table_format","Format",c("CSV"="csv","Excel (.xlsx)"="xlsx","PDF"="pdf")),
      shiny::downloadButton("params","Valeurs résumées et qualité de l’ajustement"),
      shiny::downloadButton("csv","Données complètes")))),
  bslib::nav_panel("Aide",bslib::card(fill=FALSE,bslib::card_body(fillable=FALSE,shiny::uiOutput("section_help"))))
)

modern_server <- function(input,output,session) {
  data <- web_import_server("import")
  conditions <- web_conditions_server("conditions",data)
  windows <- web_windows_server("windows",data,conditions)
  result <- shiny::reactiveVal(NULL); job <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("Prêt. Ajoutez un signal pour commencer.")
  revision <- shiny::reactiveVal(0L); completed <- shiny::reactiveVal(-1L); launched <- 0L
  shiny::observeEvent(list(data(),conditions(),windows()),{
    revision(shiny::isolate(revision())+1L)
  },ignoreNULL=FALSE)
  output$section_help<-shiny::renderUI(web_section_help(input$language %||% "en"))
  output$correction_description<-shiny::renderUI(shiny::p(if(identical(input$language,"fr"))
    "Signal corrigé = mesure moins la moyenne du groupe blanc, calculée séparément pour l’OD et chaque signal dans les fenêtres sélectionnées. Sans blanc, la mesure est conservée. Une valeur négative signifie que la mesure est inférieure au blanc."
    else "Corrected signal = measurement minus the blank-group mean, calculated separately for OD and each signal within the selected windows. Without a blank, the measurement is unchanged. Negative values mean the measurement is below the blank."))
  output$counts <- shiny::renderText({d<-data();if(is.null(d))"Aucune donnée" else paste(length(unique(d$signal)),if(identical(input$language,"fr")) "signaux ·" else "signals ·",length(unique(d$well)),if(identical(input$language,"fr")) "puits ·" else "wells ·",length(conditions()),"conditions")})
  output$status <- shiny::renderText({paste(status(),if(!is.null(result()) && revision()!=completed())"Paramètres modifiés : relancez l’analyse avant export." else "")})
  shiny::observeEvent(input$run,{
    if(!is.null(job())){shiny::showNotification("Une analyse est déjà en cours.");return()}
    tryCatch({
      shiny::req(data(),windows());web_validate_conditions(conditions(),unique(data()$well))
      launched <<- revision()
      job(callr::r_bg(function(data,conditions,windows,loss,seed){
        CuRveR:::web_analyze(data,conditions,windows,loss,seed)
      },args=list(data=data(),conditions=conditions(),windows=windows(),loss="LAD",seed=42L),libpath=.libPaths(),supervise=TRUE))
      status("Analyse en cours dans un processus séparé…")
    },error=function(e){status("Analyse non lancée.");shiny::showNotification(conditionMessage(e),type="error",duration=NULL)})
  })
  shiny::observe({
    process<-job();if(is.null(process))return();shiny::invalidateLater(500,session)
    if(!process$is_alive()){
      tryCatch({r<-process$get_result();result(r);completed(launched);status("Analyse terminée.")
        shiny::updateSelectInput(session,"signal",choices=unique(r$data$signal),selected=attr(data(),"od_signal") %||% unique(r$data$signal)[1])
        shiny::updateCheckboxGroupInput(session,"conditions",choices=unique(r$data$condition),selected=unique(r$data$condition))
      },error=function(e){status("Échec de l’analyse ; résultats précédents conservés.");shiny::showNotification(conditionMessage(e),type="error",duration=NULL)})
      job(NULL)
    }
  })
  shiny::observeEvent(input$cancel,{if(!is.null(job())){job()$kill();job(NULL);status("Analyse annulée.")}})
  session$onSessionEnded(function(){p<-shiny::isolate(job());if(!is.null(p))p$kill()})
  output$overview_container <- shiny::renderUI({
    height <- if(is.null(data())) 300L else web_overview_height(data())
    shiny::div(style=paste0("height:",height,"px;min-height:",height,"px;flex-shrink:0;"),
      shiny::plotOutput("overview",height=paste0(height,"px"),fill=FALSE))
  })
  overview <- shiny::reactive({shiny::req(data());web_overview_grid(data(),input$language %||% "en")})
  output$overview <- shiny::renderPlot({
    shiny::validate(shiny::need(!is.null(data()), if(identical(input$language,"fr")) "Importez les données avec le bouton Importer OD + signal." else "Import data using the Import OD + signal button first."))
    tryCatch({grid::grid.newpage();grid::grid.draw(overview())}, error=function(e) {
      shiny::validate(shiny::need(FALSE,paste(if(identical(input$language,"fr")) "Impossible de tracer l’aperçu :" else "Cannot render overview:",conditionMessage(e))))
    })
  },res=96,height=function(){if(is.null(data())) 300L else web_overview_height(data())},width=function(){
    width <- session$clientData$output_overview_width
    if(is.null(width) || !is.finite(width) || width<100) 1200 else width
  },execOnResize=TRUE)
  shiny::observeEvent(input$page,{
    if(identical(input$page,"Aperçu")) session$sendCustomMessage("curver-overview-resize",list())
  })
  plot <- shiny::reactive({shiny::req(result(),input$signal,input$conditions)
    xlim<-if(isTRUE(input$use_zoom) && !is.null(input$zoom)) c(input$zoom$xmin,input$zoom$xmax) else NULL
    p<-web_plot(result(),input$signal,input$conditions,input$plot_type,input$features,input$metric,positive=!isTRUE(input$show_negative),xlim=xlim)
    if(input$plot_type!="metrics") p<-p+ggplot2::labs(x=if(identical(input$language,"fr")) "Temps (in min)" else "Time (in min)",y=if(identical(input$language,"fr")) "Signal corrigé" else "Corrected signal",color="Condition",fill="Condition")
    p
  })
  output$negative_note<-shiny::renderText({
    shiny::req(result(),input$signal)
    d<-result()$data;d<-d[d$signal==input$signal & d$condition %in% input$conditions,]
    n<-sum(d$value<0)
    if(!n || isTRUE(input$show_negative) || input$plot_type=="metrics")return("")
    paste(n,if(identical(input$language,"fr")) "valeurs corrigées négatives hors du cadre positif. Elles restent dans les calculs et exports ; activez leur affichage dans Options du graphique pour les voir." else "negative corrected values are outside the positive view. They remain in calculations and exports; enable their display in Plot options to view them.")
  })
  output$result_plot<-shiny::renderPlot({
    shiny::validate(shiny::need(!is.null(result()),if(identical(input$language,"fr")) "Lancez l’analyse pour afficher les résultats." else "Run the analysis to display results."),
      shiny::need(length(input$conditions)>0,if(identical(input$language,"fr")) "Sélectionnez une condition." else "Select a condition."))
    print(plot())
  },height=550,res=96,execOnResize=TRUE,width=function(){
    width<-session$clientData$output_result_plot_width
    if(is.null(width)||!is.finite(width)||width<100) 900 else width
  })
  output$parameters<-shiny::renderTable({shiny::req(result());web_summary(result())},digits=6)
  output$performance<-shiny::renderTable({shiny::req(result());result()$performance},digits=4)
  output$data<-shiny::renderTable({shiny::req(result());web_data_page(result()$data,input$data_page,input$data_size)})
  shiny::observeEvent(list(result(),input$data_size),{
    shiny::req(result());size<-as.integer(input$data_size %||% 50)
    shiny::updateNumericInput(session,"data_page",value=1,min=1,max=max(1,ceiling(nrow(result()$data)/size)))
  })
  output$data_count<-shiny::renderText({
    shiny::req(result());total<-nrow(result()$data);size<-as.integer(input$data_size %||% 50)
    paste(total,if(identical(input$language,"fr")) "lignes au total · Pages :" else "total rows · Pages:",max(1,ceiling(total/size)))
  })
  ready<-function(){shiny::req(result(),is.null(job()));if(revision()!=completed())stop("Résultats obsolètes : relancez l’analyse.")}
  output$csv<-shiny::downloadHandler(function()paste0(if(identical(input$language,"fr")) "donnees_completes" else "complete_data",".",input$table_format),function(file){
    ready();web_export_table(result()$data,file,input$table_format,if(identical(input$language,"fr")) "Données complètes" else "Complete data")
  })
  output$params<-shiny::downloadHandler(function()paste0(if(identical(input$language,"fr")) "valeurs_resumees_et_qualite" else "summarized_values_and_fit_quality",".",input$table_format),function(file){
    ready();web_export_table(web_summary(result()),file,input$table_format,if(identical(input$language,"fr")) "Valeurs résumées et qualité de l’ajustement" else "Summarized values and fit quality")
  })
  output$figure<-shiny::downloadHandler(function()paste0("CuRveR_",input$plot_type,".",input$format),function(file){ready();ggplot2::ggsave(file,plot(),device=input$format,width=input$width,height=input$height,units="cm",dpi=input$dpi,limitsize=TRUE)})
  output$overview_export<-shiny::downloadHandler(function()"overview.pdf",function(file)ggplot2::ggsave(file,overview(),device="pdf",width=27,height=web_overview_height(data())/96*2.54,units="cm"))
}

web_section_help <- function(language="en") {
  fr<-identical(language,"fr")
  sections<-if(fr)list(
    Import=c("Charger les mesures OD et un autre signal.","Choisissez le fichier, les deux feuilles et la colonne de temps, puis importez. Les puits sont détectés automatiquement ; les temps Excel deviennent des minutes écoulées. Pour CSV/TSV, fournissez le temps en minutes."),
    Aperçu=c("Inspecter les courbes brutes de chaque puits avant toute correction.","Il suffit de regarder. OD en noir à gauche, signal en rouge à droite : les axes sont alignés séparément par puits. Leurs croisements dépendent des échelles. Le bouton tout en bas permet d’enregistrer un PDF."),
    Conditions=c("Définir les réplicats techniques et le blanc commun.","Les triplets sont préremplis. Sélectionnez un groupe, renommez-le, retirez un puits contaminé ou ajoutez un réplicat, puis enregistrez. Un puits déplacé quitte son groupe précédent. Désignez le groupe blanc dans le menu latéral ; il reste modifiable et est exclu des ajustements."),
    Fenêtres=c("Choisir les points de temps utilisés pour l’ajustement et la moyenne du blanc.","Choisissez un signal et un puits, puis sélectionnez une zone du graphique ou saisissez ses bornes. Tout est enregistré automatiquement dans la session. Cocher le partage applique immédiatement la fenêtre affichée à tous les puits/signaux ; décocher permet les modifications individuelles."),
    Résultats=c("Ajuster le modèle de Richard à quatre paramètres et examiner les résultats.","Lancez l’analyse puis choisissez le signal, les conditions et la vue : courbes, comparaison des paramètres ou annotations. Les options règlent points, ajustement, moyenne, écart-type et zoom. Les tableaux sont dépliables. Le signal corrigé est la mesure moins la moyenne du blanc pour ce signal. Les valeurs négatives restent dans les calculs et exports ; leur affichage peut être activé. Enregistrez la figure visible avec le petit bouton sous le graphique."),
    Export=c("Télécharger les deux tableaux complets de Results.","Choisissez CSV, Excel ou PDF puis le tableau : valeurs résumées avec qualité de l’ajustement, ou données complètes. La pagination d’affichage ne limite pas l’export. Le PDF répartit lignes et colonnes sur plusieurs pages. Si les conditions ou fenêtres ont changé, relancez l’analyse avant export.")) else list(
    Import=c("Load OD measurements and another signal.","Choose the file, two sheets and time column, then import. Wells are detected automatically; Excel times become elapsed minutes. For CSV/TSV, supply time in minutes."),
    Overview=c("Inspect the raw curves of each well before correction.","Simply view the plots. OD is black on the left, the signal red on the right; axes are aligned independently per well. Crossings depend on these scales. Use the button at the bottom to save a PDF if desired."),
    Conditions=c("Define technical replicates and the shared blank.","Triplets are prefilled. Select a group, rename it, remove a contaminated well or add a replicate, then save. Moving a well removes it from its previous group. Select the blank group in the side menu; it remains editable and is excluded from fitting."),
    `Time windows`=c("Choose time points used for fitting and the blank mean.","Select a signal and well, then brush a region or type its limits. Changes are saved automatically within the session. Checking global application immediately copies the current window to all wells/signals; uncheck it for individual edits."),
    Results=c("Fit the four-parameter Richard model and review results.","Run the analysis, then choose the signal, conditions and view: curves, parameter comparison or annotations. Plot options control points, fit, mean, standard deviation and zoom. Expand the tables as needed. Corrected signal is measurement minus the blank mean for that signal. Negative values remain in calculations and exports; enable their display if needed. Save the visible figure using the small button below it."),
    Export=c("Download the two complete Results tables.","Choose CSV, Excel or PDF, then the table: summarized values with fit quality, or complete data. Display pagination does not limit exports. PDF splits rows and columns across pages. Rerun analysis before exporting if conditions or windows have changed."))
  shiny::tagList(lapply(names(sections),function(name)shiny::tags$section(shiny::h4(name),shiny::p(shiny::strong(if(fr)"À quoi ça sert : " else "Purpose: "),sections[[name]][1]),shiny::p(shiny::strong(if(fr)"Comment l’utiliser : " else "How to use it: "),sections[[name]][2]))),
    shiny::hr(),shiny::p(if(fr)"La documentation utilisateur détaillée sera complétée ensemble sur GitHub." else "We will expand the detailed user documentation together on GitHub."),
    shiny::tags$a(href="https://github.com/santinlab-code/CuRveR/blob/feature/modern-web-app/docs/modern-web-app.md",target="_blank",rel="noopener noreferrer",if(fr)"Documentation sur GitHub" else "Documentation on GitHub"))
}
