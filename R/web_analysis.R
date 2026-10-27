# Pure adapters: scientific equations, bounds, losses and optimizers remain unchanged.
web_prepare <- function(data, time, wells, signal) {
  if (!nzchar(signal) || !length(wells) || time %in% wells)
    stop("Choisissez un signal, une colonne temps et des mesures distinctes.")
  selected <- c(time, wells)
  if (!all(selected %in% names(data))) stop("Colonnes absentes du fichier.")
  if (!all(vapply(data[selected], is.numeric, logical(1))))
    stop("Temps et mesures doivent être numériques. Vérifiez le séparateur et les décimales.")
  if (any(!is.finite(data[[time]]))) stop("Le temps contient des valeurs manquantes ou infinies.")
  if (is.unsorted(data[[time]])) stop("Le temps doit être trié dans l’ordre croissant.")
  if (any(!is.finite(as.matrix(data[wells])))) stop("Les mesures contiennent des valeurs manquantes ou infinies.")
  data <- data[selected]
  names(data)[1] <- "Time"
  data$signal <- signal
  data <- clean_time(data, "Time")
  format_wellplate_long(data, wells = tidyselect::all_of(wells))
}

web_validate_conditions <- function(conditions, wells) {
  if (!is.list(conditions) || !length(conditions) || is.null(names(conditions)) ||
      any(!nzchar(names(conditions))) || anyDuplicated(names(conditions)))
    stop("La configuration doit contenir des conditions nommées et uniques.")
  for (condition in conditions) {
    if (!is.list(condition) || !all(c("replicates", "blanks") %in% names(condition)))
      stop("Chaque condition doit définir replicates et blanks.")
    reps <- unlist(condition$replicates, use.names = FALSE)
    blanks <- unlist(condition$blanks, use.names = FALSE)
    if (!length(reps) || !all(c(reps, blanks) %in% wells) || length(intersect(reps, blanks)))
      stop("Chaque condition nécessite des réplicats valides, distincts de ses blancs.")
  }
  conditions
}

web_analyze <- function(data, conditions, windows, loss = "LAD", seed = 42L) {
  web_validate_conditions(conditions, unique(data$well))
  losses <- list(LAD = least_absolute_deviation, OLS = ordinary_least_squares,
                 Huber = huber, Tukey = tukey_bisquare, Andrew = andrew, Jaeckel = jaeckel)
  if (!loss %in% names(losses)) stop("Fonction de coût inconnue.")
  # Preserve the caller's random state while making each run reproducible.
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv) else
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  rows <- parameters <- performance <- list()
  for (signal in sort(unique(data$signal))) for (name in sort(names(conditions))) {
    cond <- conditions[[name]]
    part <- data[data$signal == signal, ]
    win <- windows[windows$signal == signal, ]
    part <- dplyr::left_join(part, win, by = c("signal", "well"))
    if (anyNA(part$left) || anyNA(part$right)) stop("Fenêtres manquantes.")
    part <- part[part$Time >= part$left & part$Time <= part$right, ]
    blank_values <- part$value[part$well %in% unlist(cond$blanks)]
    blank <- if (length(blank_values)) mean(blank_values) else NA_real_
    part <- part[part$well %in% unlist(cond$replicates), ]
    if (!is.na(blank)) part$value <- part$value - blank
    if (length(unique(part$Time)) < 5 || !nrow(part) || diff(range(part$value)) == 0)
      stop(paste(name, signal, ": au moins cinq temps distincts et un signal variable sont nécessaires."))
    model <- RichardModel$new(part$Time, part$value)
    model$optimize(DeOptimizer$new(losses[[loss]]))
    part$fit <- model$predict(part$Time)
    if (any(!is.finite(part$fit))) stop(paste(name, signal, ": ajustement non fini."))
    part$condition <- name
    for (p in names(model$estimated)) part[[p]] <- model$estimated[[p]]
    rows[[length(rows)+1L]] <- part
    parameters[[length(parameters)+1L]] <- data.frame(signal=signal, condition=name, as.list(model$estimated))
    performance[[length(performance)+1L]] <- data.frame(signal=signal, condition=name,
      VEcv=variance_explained(part$value,part$fit), d_r=willmott_index_agreement(part$value,part$fit))
  }
  list(data=dplyr::bind_rows(rows), parameters=dplyr::bind_rows(parameters),
       performance=dplyr::bind_rows(performance), settings=list(loss=loss, seed=seed,
       conditions=conditions, windows=windows, R=R.version.string, package=as.character(utils::packageVersion("CuRveR"))))
}

web_plot <- function(result, signal, conditions, type="curves", features=c("points","fit","mean","sd"), metric="r_max") {
  d <- result$data[result$data$signal == signal & result$data$condition %in% conditions, ]
  if (type == "metrics") return(ggplot2::ggplot(unique(d[c("condition",metric)]),
    ggplot2::aes(x=.data[[metric]], y=condition, color=condition)) + ggplot2::geom_point(size=4) + ggplot2::theme_minimal())
  p <- ggplot2::ggplot(d, ggplot2::aes(Time, value, color=condition, fill=condition))
  if ("points" %in% features) p <- p + ggplot2::geom_point(alpha=.4)
  if ("fit" %in% features) p <- p + ggplot2::geom_line(ggplot2::aes(y=fit, group=interaction(condition,well)))
  summary <- dplyr::summarise(dplyr::group_by(d, condition, Time), mean=mean(value), sd=stats::sd(value), .groups="drop")
  if ("sd" %in% features) p <- p + ggplot2::geom_ribbon(data=summary, ggplot2::aes(y=NULL,ymin=mean-sd,ymax=mean+sd),alpha=.12,colour=NA)
  if ("bars" %in% features) p <- p + ggplot2::geom_errorbar(data=summary,ggplot2::aes(y=NULL,ymin=mean-sd,ymax=mean+sd),alpha=.4)
  if ("mean" %in% features) p <- p + ggplot2::geom_line(data=summary,ggplot2::aes(y=mean),linetype="dashed")
  if (type == "annotations") {
    p <- p + ggplot2::facet_wrap(~condition)
    if ("p_max" %in% features) p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=p_max),linetype=2)
    if ("p_min" %in% features) p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=p_min),linetype=2)
    if ("s" %in% features) p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept=s),linetype=2)
    if ("r_max" %in% features) {
      at_s <- with(d, p_min+(p_max-p_min)/(1+exp(4*r_max*(s-s)/(p_min-p_max))))
      d$intercept <- at_s-d$r_max*d$s
      p <- p + ggplot2::geom_abline(data=d,ggplot2::aes(slope=r_max,intercept=intercept),linetype=2)
    }
  }
  p + ggplot2::theme_minimal(base_size=13) + ggplot2::labs(x="Temps (unité du fichier)",y="Signal corrigé",color="Condition",fill="Condition")
}

# A single invertible display transform, shared by all well panels.
# Original values and the scientific pipeline are never rescaled.
web_overview_plot <- function(data, language="en") {
  od <- attr(data,"od_signal")
  fluorescence <- attr(data,"fluorescence_signal")
  if(is.null(od)) od <- unique(data$signal)[1]
  if(is.null(fluorescence)) {
    return(ggplot2::ggplot(data,ggplot2::aes(Time,value,group=signal))+
      ggplot2::geom_line(color="black")+ggplot2::facet_wrap(~well)+
      ggplot2::labs(x="Temps (unité du fichier)",y=od)+ggplot2::theme_minimal())
  }
  left <- data[data$signal==od,]
  right <- data[data$signal==fluorescence,]
  amplitude <- function(x) {m<-max(abs(x),na.rm=TRUE);if(!is.finite(m)||m==0) 1 else m}
  factor <- amplitude(right$value)/amplitude(left$value)
  right$display_value <- right$value/factor
  ggplot2::ggplot(left,ggplot2::aes(Time,value))+
    ggplot2::geom_line(color="black")+
    ggplot2::geom_line(data=right,ggplot2::aes(y=display_value),color="#c62828")+
    ggplot2::facet_wrap(~well)+
    ggplot2::scale_y_continuous(name=paste0("OD600 — ",od),
      sec.axis=ggplot2::sec_axis(function(x) x * factor,name=paste0("Signal — ",fluorescence)))+
    ggplot2::labs(x=if(language=="fr") "Temps (minutes pour Excel)" else "Time (minutes for Excel)",
      caption=if(language=="fr") "Noir : OD600 (axe gauche) · Rouge : signal (axe droit)" else "Black: OD600 (left axis) · Red: signal (right axis)")+
    ggplot2::theme_minimal(base_size=12)+
    ggplot2::theme(plot.background=ggplot2::element_rect(fill="white",color=NA),
      panel.background=ggplot2::element_rect(fill="white",color=NA),
      axis.title.y.left=ggplot2::element_text(color="black"),
      axis.text.y.left=ggplot2::element_text(color="black"),
      axis.title.y.right=ggplot2::element_text(color="#c62828"),
      axis.text.y.right=ggplot2::element_text(color="#c62828"))
}
