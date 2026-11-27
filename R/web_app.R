modern_ui <- function() bslib::page_navbar(
  title=shiny::tagList(shiny::div(style="display:inline-block;margin-right:14px",shiny::tags$select(id="language",class="shiny-input-select form-select",`aria-label`="Language / Langue",shiny::tags$option(value="en",selected="selected","EN"),shiny::tags$option(value="fr","FR"))),"CuRveR 2.0"),id="page",theme=bslib::bs_theme(version=5, bootswatch="flatly",primary="#00284B",success="#00284B",base_font="system-ui"),
  header=shiny::tagList(shiny::tags$link(rel="stylesheet",href="curver/app.css"),shiny::tags$script(src="curver/language.js"),shiny::tags$style(shiny::HTML("body{background:#f3f6f8}.bslib-card{border:0;box-shadow:0 2px 12px #1232}.navbar{border-bottom:3px solid #00284B}.navbar .nav-link{color:#fff!important}.navbar .nav-link.active,.navbar .nav-link[aria-selected=true]{color:#00284B!important;background:#fff!important;border-radius:6px;font-weight:600}.navbar .nav-link:focus-visible{outline:2px solid #fff;outline-offset:3px}.shiny-output-error-validation{color:#9c3c19}.form-control{border-radius:6px}table{font-variant-numeric:tabular-nums}#conditions .shiny-options-group{display:flex;flex-wrap:wrap;gap:8px 18px}#conditions .form-check{margin:0}"))),
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
    bslib::card_header("Analyse — Richard, régression segmentée et statistiques"),
    bslib::card_body(fillable=FALSE,
      shiny::p("Modèle : Richard · Optimisation : évolution différentielle et raffinement borné · Coût : LAD"),
      shiny::tags$details(class="segment-options",shiny::tags$summary("Options de la régression segmentée"),
        shiny::div(style="display:flex;gap:18px;flex-wrap:wrap;align-items:flex-end;",
          shiny::selectInput("seg_scale","Échelle",c("Auto : log pour l’OD, linéaire pour les autres signaux"="auto","Logarithmique pour tous les signaux"="log","Linéaire pour tous les signaux"="linear"),width="360px"),
          shiny::selectInput("seg_mode","Points de rupture",c("Automatique (BIC)"="auto","Nombre fixe"="fixed","Guidé (temps initiaux)"="guided"),width="220px"),
          shiny::conditionalPanel("input.seg_mode == 'auto'",shiny::numericInput("seg_max","Maximum de ruptures",2,min=1,max=4,step=1,width="170px")),
          shiny::conditionalPanel("input.seg_mode == 'fixed'",shiny::numericInput("seg_k","Nombre de ruptures",2,min=0,max=4,step=1,width="170px")),
          shiny::conditionalPanel("input.seg_mode == 'guided'",shiny::textInput("seg_guesses","Temps initiaux (min, séparés par des virgules)","",placeholder="180, 480",width="280px"))),
        shiny::checkboxInput("per_well_richard","Ajuster aussi Richard puits par puits (nécessaire aux statistiques sur Richard, plus long)",TRUE)),
      shiny::div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:16px;",
        shiny::actionButton("run","Lancer l’analyse",class="btn-primary"),shiny::actionButton("cancel","Annuler")),
      shiny::div(style="display:flex;gap:24px;flex-wrap:wrap;align-items:center;",
        shiny::div(style="flex:1 1 220px;",shiny::selectInput("signal","Signal",NULL,width="100%")),
        shiny::div(style="flex:2 1 360px;",shiny::radioButtons("plot_type","Vue",choices=c("Courbes"="curves","Paramètres"="metrics","Annotations"="annotations","Segments"="segments","Statistiques"="stats"),selected="curves",inline=TRUE))),
      shiny::checkboxGroupInput("conditions","Conditions",choices=NULL,inline=TRUE),
      shiny::conditionalPanel("input.plot_type == 'metrics'",shiny::selectInput("metric","Paramètre",c("r_max","p_max","p_min","s"))),
      shiny::conditionalPanel("input.plot_type == 'stats'",shiny::div(style="display:flex;gap:18px;flex-wrap:wrap;",
        shiny::selectInput("stat_param","Paramètre testé",NULL,width="300px"),
        shiny::selectInput("stat_reference","Condition de référence",NULL,width="220px"),
        shiny::radioButtons("stat_mode","Comparaisons",c("Contre la référence"="reference","Toutes les paires"="pairwise"),inline=TRUE))),
      shiny::tags$details(shiny::tags$summary("Options du graphique"),
        shiny::checkboxGroupInput("features","Afficher",choices=c("Points"="points","Ajustement"="fit","Moyenne"="mean","Écart-type (halo)"="sd","Écart-type (barres)"="bars","p_max","p_min","s","r_max"),selected=c("points","fit","mean","sd"),inline=TRUE),
        shiny::checkboxInput("use_zoom","Zoom sur la zone sélectionnée",FALSE),
        shiny::checkboxInput("show_negative","Afficher les valeurs corrigées négatives",FALSE)),
      shiny::div(style="height:550px;min-height:550px;flex-shrink:0;",shiny::plotOutput("result_plot",height="550px",fill=FALSE,brush=shiny::brushOpts("zoom",direction="x"))),
      shiny::div(class="plot-save-panel",
        shiny::div(class="plot-save-toolbar",
          shiny::div(class="plot-save-format",shiny::tags$label(class="visually-hidden",`for`="format","Image format"),
            shiny::selectInput("format",NULL,c("PNG"="png","PDF"="pdf","SVG"="svg","JPEG"="jpeg","TIFF"="tiff","EPS"="eps","PS"="ps","BMP"="bmp"),selected="png",width="110px")),
          shiny::downloadButton("figure","Enregistrer le graphique",class="btn-sm btn-primary")),
        shiny::tags$details(class="plot-save-options",shiny::tags$summary("Options de l’image"),
          shiny::div(class="plot-save-dimensions",
            shiny::numericInput("width","Largeur (cm)",18,min=1,max=100),shiny::numericInput("height","Hauteur (cm)",12,min=1,max=100),shiny::numericInput("dpi","Résolution (dpi)",300,min=72,max=1200)))),
      shiny::textOutput("negative_note"),
      shiny::uiOutput("model_alert"),
      shiny::tags$details(shiny::tags$summary("Comparaison des modèles (Richard vs segments)"),
        shiny::div(style="overflow:auto;max-height:420px;",shiny::tableOutput("comparison")),
        shiny::p(class="small text-muted","ΔAICc = AICc(Richard) − AICc(segments) : > 10 favorise les segments, < −10 favorise Richard. Résidus structurés : test des runs p < 0,01 et écart systématique maximal (résidu moyen par temps) > 5 % de l’amplitude du signal, ce qui indique que le modèle rate une phase. Les deux modèles sont évalués sur les mêmes points, en unités du signal.")),
      shiny::tags$details(shiny::tags$summary("Phases de la régression segmentée"),
        shiny::div(style="overflow:auto;max-height:420px;",shiny::tableOutput("phases"))),
      shiny::tags$details(shiny::tags$summary("Statistiques entre conditions"),
        shiny::p(class="small text-muted","n = nombre de puits. Des puits d’une même plaque sont des réplicats techniques : les tests décrivent alors la variabilité technique, pas la reproductibilité biologique. IC à 95 % (loi de Student), test de Welch, correction de Holm, g de Hedges."),
        shiny::div(style="overflow:auto;max-height:420px;",shiny::tableOutput("stat_summary"),shiny::tableOutput("stat_global"),shiny::tableOutput("stat_tests"))),
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
      shiny::downloadButton("csv","Données complètes"),
      shiny::downloadButton("comparison_export","Comparaison des modèles"),
      shiny::downloadButton("phases_export","Phases segmentées"),
      shiny::downloadButton("wells_export","Paramètres par puits"),
      shiny::downloadButton("stats_export","Tests statistiques")))),
  bslib::nav_panel("Aide",bslib::card(fill=FALSE,bslib::card_body(fillable=FALSE,shiny::uiOutput("section_help"))))
)

modern_server <- function(input,output,session) {
  data <- web_import_server("import")
  conditions <- web_conditions_server("conditions",data)
  windows <- web_windows_server("windows",data,conditions)
  result <- shiny::reactiveVal(NULL); job <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("Prêt. Ajoutez un signal pour commencer.")
  revision <- shiny::reactiveVal(0L); completed <- shiny::reactiveVal(-1L); launched <- 0L
  shiny::observeEvent(list(data(),conditions(),windows(),input$seg_scale,input$seg_mode,input$seg_max,input$seg_k,input$seg_guesses,input$per_well_richard),{
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
      segments<-list(scale=input$seg_scale %||% "auto",mode=input$seg_mode %||% "auto",
        max_breaks=as.integer(input$seg_max %||% 2),breaks=as.integer(input$seg_k %||% 2),
        guesses=if(identical(input$seg_mode,"guided")) web_parse_breaks(input$seg_guesses) else numeric())
      job(callr::r_bg(function(data,conditions,windows,loss,seed,segments,per_well){
        CuRveR:::web_analyze_advanced(data,conditions,windows,loss,seed,segments,per_well)
      },args=list(data=data(),conditions=conditions(),windows=windows(),loss="LAD",seed=42L,segments=segments,per_well=isTRUE(input$per_well_richard)),libpath=.libPaths(),supervise=TRUE))
      status("Analyse en cours dans un processus séparé…")
    },error=function(e){status("Analyse non lancée.");shiny::showNotification(conditionMessage(e),type="error",duration=NULL)})
  })
  shiny::observe({
    process<-job();if(is.null(process))return();shiny::invalidateLater(500,session)
    if(!process$is_alive()){
      tryCatch({r<-process$get_result();result(r);completed(launched);status("Analyse terminée.")
        shiny::updateSelectInput(session,"signal",choices=unique(r$data$signal),selected=attr(data(),"od_signal") %||% unique(r$data$signal)[1])
        shiny::updateCheckboxGroupInput(session,"conditions",choices=unique(r$data$condition),selected=unique(r$data$condition))
        conds<-unique(r$data$condition);shiny::updateSelectInput(session,"stat_reference",choices=conds,selected=conds[1])
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
    shiny::validate(shiny::need(!is.null(data()), if(identical(input$language,"fr")) "Importez d’abord les données dans l’onglet Import." else "Import data from the Import tab first."))
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
    language<-input$language %||% "en"
    if(input$plot_type=="segments")return(web_segment_plot(result(),input$signal,input$conditions,language,xlim))
    if(input$plot_type=="stats")return(web_stats_plot(stats(),language))
    p<-web_plot(result(),input$signal,input$conditions,input$plot_type,input$features,input$metric,positive=!isTRUE(input$show_negative),xlim=xlim)
    if(input$plot_type!="metrics") p<-p+ggplot2::labs(x=if(identical(input$language,"fr")) "Temps (in min)" else "Time (in min)",y=if(identical(input$language,"fr")) "Signal corrigé" else "Corrected signal",color="Condition",fill="Condition")
    p
  })
  output$negative_note<-shiny::renderText({
    shiny::req(result(),input$signal)
    d<-result()$data;d<-d[d$signal==input$signal & d$condition %in% input$conditions,]
    n<-sum(d$value<0)
    if(!n || isTRUE(input$show_negative) || input$plot_type %in% c("metrics","segments","stats"))return("")
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
  # Statistics are recomputed instantly from per-well parameters: no refit needed.
  shiny::observeEvent(list(result(),input$signal),{
    shiny::req(result(),input$signal);w<-result()$wells;shiny::req(!is.null(w))
    w<-unique(w[w$signal==input$signal,c("method","parameter")])
    choices<-stats::setNames(paste(w$method,w$parameter,sep="::"),paste(w$method,"·",w$parameter))
    current<-shiny::isolate(input$stat_param)
    shiny::updateSelectInput(session,"stat_param",choices=choices,selected=if(!is.null(current) && current %in% choices) current else choices[1])
  })
  stats<-shiny::reactive({
    shiny::req(result(),input$signal,input$stat_param);key<-strsplit(input$stat_param,"::",fixed=TRUE)[[1]]
    w<-result()$wells;w<-w[w$condition %in% input$conditions,]
    web_parameter_stats(w,input$signal,key[1],key[2],input$stat_reference,input$stat_mode %||% "reference")
  })
  output$stat_summary<-shiny::renderTable({shiny::req(result()$wells);stats()$summary},digits=5,caption="Résumé par condition / Summary by condition",caption.placement="top")
  output$stat_global<-shiny::renderTable({shiny::req(result()$wells);g<-stats()$global;shiny::req(nrow(g));g},digits=5,caption="Test global / Global test",caption.placement="top")
  output$stat_tests<-shiny::renderTable({shiny::req(result()$wells);t<-stats()$tests;shiny::req(nrow(t));t},digits=5,caption="Comparaisons / Comparisons",caption.placement="top")
  output$comparison<-shiny::renderTable({
    shiny::req(result()$comparison);cmp<-result()$comparison
    cmp$verdict<-mapply(web_verdict_text,cmp$verdict,cmp$richard_structured_residuals,MoreArgs=list(language=input$language %||% "en"))
    cmp},digits=4)
  output$phases<-shiny::renderTable({shiny::req(result()$segments);p<-result()$segments$phases;p[p$signal==input$signal,]},digits=5)
  output$model_alert<-shiny::renderUI({
    shiny::req(result()$comparison);cmp<-result()$comparison
    bad<-cmp[cmp$verdict=="segmented" | cmp$richard_structured_residuals,]
    if(!nrow(bad))return(NULL)
    fr<-identical(input$language,"fr")
    shiny::div(class="alert alert-warning",style="margin-top:12px;",
      shiny::strong(if(fr)"Richard est peut-être inadapté pour : " else "Richard may be inappropriate for: "),
      paste(unique(paste(bad$signal,bad$condition,sep=" / ")),collapse=", "),
      shiny::br(),if(fr)"Consultez la vue Segments et la comparaison des modèles." else "Check the Segments view and the model comparison.")
  })
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
  output$comparison_export<-shiny::downloadHandler(function()paste0("model_comparison.",input$table_format),function(file){ready();web_export_table(result()$comparison,file,input$table_format,"Model comparison")})
  output$phases_export<-shiny::downloadHandler(function()paste0("segmented_phases.",input$table_format),function(file){ready();web_export_table(result()$segments$phases,file,input$table_format,"Segmented phases")})
  output$wells_export<-shiny::downloadHandler(function()paste0("parameters_by_well.",input$table_format),function(file){ready();web_export_table(result()$wells,file,input$table_format,"Parameters by well")})
  output$stats_export<-shiny::downloadHandler(function()paste0("statistical_tests.",input$table_format),function(file){ready();web_export_table(web_stats_table(result(),input$stat_reference,input$stat_mode %||% "reference"),file,input$table_format,"Statistical tests")})
  output$figure<-shiny::downloadHandler(function()paste0("CuRveR_",input$plot_type,".",input$format),function(file){ready();ggplot2::ggsave(file,plot(),device=input$format,width=input$width,height=input$height,units="cm",dpi=input$dpi,limitsize=TRUE)})
  output$overview_export<-shiny::downloadHandler(function()"overview.pdf",function(file)ggplot2::ggsave(file,overview(),device="pdf",width=27,height=web_overview_height(data())/96*2.54,units="cm"))
}

web_section_help <- function(language="en") {
  fr<-identical(language,"fr")
  sections<-if(fr)list(
    Import=c("Charger les mesures OD seules ou accompagnées d’un second signal.","Choisissez le fichier et la colonne de temps. Pour Excel, choisissez la feuille OD et, si nécessaire, une seconde feuille de signal. Les puits sont détectés automatiquement. Les temps Excel sont convertis en minutes écoulées. Pour CSV/TSV, fournissez le temps en minutes."),
    Aperçu=c("Inspecter les courbes brutes de chaque puits avant toute correction.","L’OD est en noir sur l’axe de gauche. Si vous avez importé un second signal, il apparaît en rouge sur l’axe de droite. Les axes sont alignés séparément par puits. Le bouton du bas permet d’enregistrer un PDF."),
    Conditions=c("Définir les réplicats techniques et le blanc commun.","Choisissez 3, 4 ou 6 réplicats techniques pour générer les groupes par ligne. Le choix par défaut est 3. Changer le nombre recrée les groupes tout en conservant le blanc sélectionné. Sélectionnez un groupe pour le renommer ou modifier ses puits, puis enregistrez. Un puits déplacé quitte son groupe précédent. Désignez le groupe blanc dans le menu latéral pour l’exclure des ajustements."),
    Fenêtres=c("Choisir les points de temps utilisés pour l’ajustement et la moyenne du blanc.","Choisissez un signal et un puits, puis sélectionnez une zone du graphique ou saisissez ses bornes. Tout est enregistré automatiquement dans la session. Cocher le partage applique immédiatement la fenêtre affichée à tous les puits/signaux ; décocher permet les modifications individuelles."),
    Résultats=c("Ajuster Richard, la régression segmentée, comparer les modèles et tester les différences entre conditions.","Réglez si besoin les options de la régression segmentée : échelle automatique (log pour l’OD, linéaire pour la fluorescence) ou imposée ; nombre de ruptures automatique (BIC commun à tous les puits d’un signal), fixe ou guidé par des temps initiaux. La vue Segments montre les phases et leurs débuts. La vue Statistiques montre chaque puits, la moyenne et l’IC à 95 %, avec les tests de Welch contre la référence. Un bandeau signale les conditions où Richard décrit mal la courbe. Lancez l’analyse puis choisissez le signal, les conditions et la vue : courbes, comparaison des paramètres ou annotations. Les options règlent points, ajustement, moyenne, écart-type et zoom. Les tableaux sont dépliables. Le signal corrigé est la mesure moins la moyenne du blanc pour ce signal. Les valeurs négatives restent dans les calculs et exports ; leur affichage peut être activé. Enregistrez la figure visible avec le petit bouton sous le graphique."),
    Export=c("Télécharger les deux tableaux complets de Results.","Choisissez CSV, Excel ou PDF puis le tableau : valeurs résumées avec qualité de l’ajustement, ou données complètes. La pagination d’affichage ne limite pas l’export. Le PDF répartit lignes et colonnes sur plusieurs pages. Si les conditions ou fenêtres ont changé, relancez l’analyse avant export.")) else list(
    Import=c("Load OD measurements alone or with a second signal.","Choose the file and time column. For Excel, choose the OD sheet and optionally a second signal sheet. Wells are detected automatically. Excel times are converted to elapsed minutes. For CSV/TSV, supply time in minutes."),
    Overview=c("Inspect the raw curves of each well before correction.","OD is shown in black on the left axis. If you imported a second signal, it appears in red on the right axis. The axes are aligned independently for each well. Use the button at the bottom to save a PDF."),
    Conditions=c("Define technical replicates and the shared blank.","Choose 3, 4 or 6 technical replicates to generate groups by row. The default is 3. Changing the count regenerates the groups and keeps the selected blank. Select a group to rename it or change its wells, then save. Moving a well removes it from its previous group. Select the blank group in the side menu to exclude it from fitting."),
    `Time windows`=c("Choose time points used for fitting and the blank mean.","Select a signal and well, then brush a region or type its limits. Changes are saved automatically within the session. Checking global application immediately copies the current window to all wells/signals; uncheck it for individual edits."),
    Results=c("Fit Richard and segmented regression, compare models and test differences between conditions.","Adjust the segmented regression options if needed: automatic scale (log for OD, linear for fluorescence) or forced; breakpoints automatic (one BIC-selected number shared by all wells of a signal), fixed, or guided by initial times. The Segments view shows phases and their onsets. The Statistics view shows each well, the mean and its 95% CI, with Welch tests against the reference. A banner flags conditions where Richard describes the curve poorly. Run the analysis, then choose the signal, conditions and view: curves, parameter comparison or annotations. Plot options control points, fit, mean, standard deviation and zoom. Expand the tables as needed. Corrected signal is measurement minus the blank mean for that signal. Negative values remain in calculations and exports; enable their display if needed. Save the visible figure using the small button below it."),
    Export=c("Download the two complete Results tables.","Choose CSV, Excel or PDF, then the table: summarized values with fit quality, or complete data. Display pagination does not limit exports. PDF splits rows and columns across pages. Rerun analysis before exporting if conditions or windows have changed."))
  shiny::tags$article(class="help-markdown",
    shiny::h2(if(fr)"Guide d’utilisation" else "User guide"),
    shiny::p(class="help-intro",if(fr)"Suivez le parcours de l’import des mesures à l’export des résultats." else "Follow the workflow from importing measurements to exporting results."),
    shiny::tags$nav(class="help-contents",`aria-label`=if(fr)"Sommaire" else "Contents",
      lapply(seq_along(sections),function(i)shiny::tags$a(href=paste0("#help-section-",i),names(sections)[i]))),
    lapply(seq_along(sections),function(i){
      name<-names(sections)[i]
      instructions<-strsplit(sections[[i]][2],"(?<=[.!?])\\s+(?=[[:upper:]])",perl=TRUE)[[1]]
      list_tag<-if(i==2)shiny::tags$ul else shiny::tags$ol
      shiny::tags$section(id=paste0("help-section-",i),
        shiny::h3(name),
        shiny::h4(if(fr)"À quoi ça sert" else "Purpose"),shiny::p(sections[[i]][1]),
        shiny::h4(if(fr)"Comment l’utiliser" else "How to use it"),
        list_tag(lapply(instructions,shiny::tags$li)))
    }),
    shiny::tags$aside(class="help-documentation",
      shiny::h3(if(fr)"Pour aller plus loin" else "Further reading"),
      shiny::p(if(fr)"La documentation utilisateur détaillée sera complétée ensemble sur GitHub." else "We will expand the detailed user documentation together on GitHub."),
      shiny::tags$a(href="https://github.com/santinlab-code/CuRveR/blob/CuRveR-2.1-modules/docs/modern-web-app.md",target="_blank",rel="noopener noreferrer",if(fr)"Documentation sur GitHub →" else "Documentation on GitHub →")))
}
