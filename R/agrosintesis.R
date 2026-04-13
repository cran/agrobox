#' Sintetiza resultados estadisticos para multiples variables
#'
#' Aplica \code{agrobox()} a una o varias variables y consolida
#' las tablas resumen. Genera ademas una sintesis decisional
#' orientada a interpretacion rapida.
#'
#' @param data data.frame con los datos experimentales.
#' @param variables vector de nombres de variables respuesta.
#' @param estructura formula en texto tipo "Factor1~Factor2" para clusters.
#' @param report logical, imprime reporte ejecutivo en consola.
#' @param color logical, usa color en el reporte (si esta disponible).
#' @param verbose Logical. If TRUE, prints progress messages.
#' @param ... argumentos adicionales pasados a \code{agrobox()}.
#'
#' @return
#' - Sin clusters: lista con \code{tabla} y \code{decision}
#' - Con clusters: lista de dichas listas por cluster
#'
#' @importFrom dplyr bind_cols distinct semi_join across all_of bind_rows
#' @importFrom stats as.formula
#'
#' @export
agrosintesis <- function(
    data,
    variables,
    estructura = NULL,
    verbose = FALSE,
    report  = TRUE,
    color   = TRUE,
    ...
) {

  .get_decision_status <- function(p, power, cv) {

    # ---- Normalizar entradas ----
    if (length(p) == 0 || is.null(p))      p <- NA_real_
    if (length(power) == 0 || is.null(power)) power <- NA_real_
    if (length(cv) == 0 || is.null(cv))    cv <- NA_real_

    # ---- Reglas agronomicas ----

    #  Datos insuficientes
    if (is.na(p) && is.na(power)) {
      return("Datos insuficientes para decision")
    }

    #  No significativo
    if (!is.na(p) && p >= 0.05) {
      return("Sin diferencias estadisticas")
    }

    # Significativo pero con baja potencia
    if (!is.na(p) && p < 0.05 && !is.na(power) && power < 0.6) {
      return("Resultado significativo pero poco confiable")
    }

    # Significativo y confiable
    if (!is.na(p) && p < 0.05 && !is.na(power) && power >= 0.8) {
      return("Resultado confiable y significativo")
    }

    #  Significativo con potencia intermedia
    if (!is.na(p) && p < 0.05 && !is.na(power)) {
      return("Resultado significativo (potencia moderada)")
    }

    #  Mucha variabilidad experimental
    if (!is.na(cv) && cv > 25) {
      return("Alta variabilidad experimental")
    }

    # fallback seguro
    "Resultado no concluyente"
  }

  # -------------------------------------------------------------------------
  # HELPER: apply color formatting to decision labels for cli output
  # -------------------------------------------------------------------------
  .color_status <- function(estado) {
    dplyr::case_when(
      estado == "Resultado confiable y significativo"        ~
        cli::col_green(estado),
      estado == "Resultado significativo (potencia moderada)" ~
        cli::col_yellow(estado),
      estado == "Resultado significativo pero poco confiable" ~
        cli::col_red(estado),
      estado == "Sin diferencias estadisticas"               ~
        cli::col_cyan(estado),
      estado == "Alta variabilidad experimental"             ~
        cli::col_magenta(estado),
      estado == "Datos insuficientes para decision"          ~
        cli::col_silver(estado),
      TRUE ~
        cli::col_white(estado)
    )
  }
  # --------------------------------------------------
  # Logger interno (no depende de cli)
  # --------------------------------------------------
  log_msg <- function(...) {
    if (isTRUE(verbose)) {
      message("[AGROSINTESIS] ", sprintf(...))
    }
  }

  # --------------------------------------------------
  # Funcion interna: sintesis por subconjunto
  # --------------------------------------------------
  sintetizar <- function(data_sub, titulo = NULL) {

    tablas     <- list()
    decisiones <- list()

    log_msg("Iniciando analisis de %d variables.", length(variables))

    for (v in variables) {

      log_msg("Analizando variable: %s", v)

      # ---- evitar que verbose llegue a agrobox ----
      args_agrobox <- list(...)
      args_agrobox$verbose <- NULL

      res <- do.call(
        agrobox,
        c(
          list(
            data       = data_sub,
            variable   = v,
            estructura = NULL
          ),
          args_agrobox
        )
      )

      # ---- tabla ----
      tab <- res$tabla
      colnames(tab)[1]  <- "trt"
      colnames(tab)[-1] <- v
      tablas[[v]] <- tab

      # ---- estadisticos globales (1 fila por cluster) ----
      st <- res$stats[1, ]

      decision <- .get_decision_status(
        p     = st$anova_p,
        power = st$Power,
        cv    = st$CV
      )

      log_msg(
        "%s | p=%.3f | Power=%.2f | CV=%.1f%% --> %s",
        v, st$anova_p, st$Power, st$CV, decision
      )

      decisiones[[v]] <- data.frame(
        variable = v,
        p_value  = st$anova_p,
        power    = st$Power,
        CV       = st$CV,
        decision = decision,
        stringsAsFactors = FALSE
      )
    }

    # ---- unir tablas ----
    tabla_final <- Reduce(
      function(x, y) dplyr::bind_cols(x, y[-1]),
      tablas
    )

    decision_df <- dplyr::bind_rows(decisiones)

    # ---- reporte ejecutivo ----
    if (report && requireNamespace("cli", quietly = TRUE)) {

      if (!is.null(titulo)) {
        cli::cli_h2(titulo)
      } else {
        cli::cli_h1("AGROSINTESIS - RESUMEN DE DECISION")
      }

      cli::cli_rule()

      for (i in seq_len(nrow(decision_df))) {

        d <- decision_df[i, ]

        estado <- d$decision
        if (color) estado <- .color_status(estado)

        cli::cli_text("{d$variable}: {estado}")
        cli::cli_text(
          "  p = {format(d$p_value, digits = 3)} | power = {round(d$power, 2)} | CV = {round(d$CV, 1)}%"
        )
        cli::cli_rule()
      }
    }

    list(
      tabla    = tabla_final,
      decision = decision_df
    )
  }

  # --------------------------------------------------
  # CASO 1: SIN CLUSTERS
  # --------------------------------------------------
  if (is.null(estructura)) {
    log_msg("Ejecutando sintesis global (sin clusters).")
    return(sintetizar(data))
  }

  # --------------------------------------------------
  # CASO 2: CON CLUSTERS
  # --------------------------------------------------
  estructura_formula <- stats::as.formula(estructura)
  vars_cluster       <- all.vars(estructura_formula)

  # -------------------------------------------------------------------------
  # Detectar que orden corresponde a que variable de cluster.
  # La logica replica exactamente como agrobox parsea 'estructura':
  #   estructura = "grupe1~grupe2"  -> vars_cluster[1]=grupe1, vars_cluster[2]=grupe2
  #   estructura = "~grupe2"        -> vars_cluster[1]=grupe2  (solo columnas)
  #   estructura = "grupe1~"        -> vars_cluster[1]=grupe1  (solo filas)
  #
  # Para saber si hay grupe1 o grupe2 parseamos igual que agrobox:
  # -------------------------------------------------------------------------
  args_extra <- list(...)

  partes_estructura <- stringr::str_split_fixed(estructura, "~", 2)
  grupe1_nombre     <- stringr::str_trim(partes_estructura[, 1])
  grupe2_nombre     <- stringr::str_trim(partes_estructura[, 2])

  tiene_grupe1 <- nzchar(grupe1_nombre)
  tiene_grupe2 <- nzchar(grupe2_nombre)

  # Extraer los ordenes que el usuario paso
  orden_g1 <- args_extra[["grupo1_orden"]]
  orden_g2 <- args_extra[["grupo2_orden"]]

  # Funcion interna: normalizar vector de orden (igual que aplicar_orden_labels)
  normalizar_orden <- function(orden_vec) {
    if (is.null(orden_vec)) return(NULL)
    nms <- names(orden_vec)
    if (is.null(nms)) {
      names(orden_vec) <- orden_vec
    } else {
      faltantes <- nms == ""
      names(orden_vec)[faltantes] <- orden_vec[faltantes]
    }
    orden_vec
  }

  orden_g1 <- normalizar_orden(orden_g1)
  orden_g2 <- normalizar_orden(orden_g2)

  # Funcion interna: aplicar orden a una columna de cluster en data_local
  aplicar_orden_cluster <- function(data_local, nombre_var, orden_vec) {
    if (is.null(orden_vec) || !nombre_var %in% names(data_local))
      return(data_local)

    niveles_ordenados <- names(orden_vec)
    vals_en_data      <- as.character(unique(data_local[[nombre_var]]))

    # Separar los que estan en el orden de los que no
    niveles_validos <- niveles_ordenados[niveles_ordenados %in% vals_en_data]
    niveles_extra   <- vals_en_data[!vals_en_data %in% niveles_validos]

    # Ordenar los extras numericamente si es posible, sino alfabeticamente
    nums <- suppressWarnings(as.numeric(niveles_extra))
    if (length(niveles_extra) > 0 && !any(is.na(nums))) {
      niveles_extra <- niveles_extra[order(nums)]
    } else {
      niveles_extra <- sort(niveles_extra)
    }

    data_local[[nombre_var]] <- factor(
      data_local[[nombre_var]],
      levels = c(niveles_validos, niveles_extra)
    )
    data_local
  }

  data_local <- data

  # Aplicar orden a grupe1 si corresponde
  if (tiene_grupe1 && grupe1_nombre %in% names(data_local)) {
    data_local <- aplicar_orden_cluster(data_local, grupe1_nombre, orden_g1)
  }

  # Aplicar orden a grupe2 si corresponde
  if (tiene_grupe2 && grupe2_nombre %in% names(data_local)) {
    data_local <- aplicar_orden_cluster(data_local, grupe2_nombre, orden_g2)
  }

  # Asegurar factor en todas las vars de cluster
  for (v in vars_cluster) {
    if (v %in% names(data_local) && !is.factor(data_local[[v]])) {
      data_local[[v]] <- factor(data_local[[v]], levels = unique(data_local[[v]]))
    }
  }

  # Construir tabla de clusters respetando el orden de los factores
  clusters <- dplyr::distinct(
    data_local,
    dplyr::across(dplyr::all_of(vars_cluster))
  )

  # Ordenar clusters segun los niveles ya definidos
  if (length(vars_cluster) == 2) {
    clusters <- clusters[order(
      match(as.character(clusters[[vars_cluster[1]]]),
            levels(data_local[[vars_cluster[1]]])),
      match(as.character(clusters[[vars_cluster[2]]]),
            levels(data_local[[vars_cluster[2]]]))
    ), , drop = FALSE]
  } else if (length(vars_cluster) == 1) {
    clusters <- clusters[order(
      match(as.character(clusters[[vars_cluster[1]]]),
            levels(data_local[[vars_cluster[1]]]))
    ), , drop = FALSE]
  }

  # Filtrar clusters: solo procesar los que estan en el orden especificado
  # Si se especifico orden para grupe2 (caso ~grupe2), filtrar por esos niveles
  if (tiene_grupe2 && !is.null(orden_g2) && grupe2_nombre %in% names(clusters)) {
    niveles_g2_validos <- names(orden_g2)
    clusters <- clusters[
      as.character(clusters[[grupe2_nombre]]) %in% niveles_g2_validos,
      , drop = FALSE
    ]
  }

  # Si se especifico orden para grupe1 (caso grupe1~), filtrar por esos niveles
  if (tiene_grupe1 && !is.null(orden_g1) && grupe1_nombre %in% names(clusters)) {
    niveles_g1_validos <- names(orden_g1)
    clusters <- clusters[
      as.character(clusters[[grupe1_nombre]]) %in% niveles_g1_validos,
      , drop = FALSE
    ]
  }

  resultados <- list()
  log_msg("Detectados %d clusters.", nrow(clusters))

  for (i in seq_len(nrow(clusters))) {

    filtro       <- clusters[i, , drop = FALSE]
    data_cluster <- dplyr::semi_join(data_local, filtro, by = vars_cluster)

    nombre <- paste(
      mapply(
        function(var, val) paste0(var, "=", as.character(val)),
        vars_cluster,
        filtro,
        SIMPLIFY = TRUE
      ),
      collapse = " | "
    )

    log_msg("Procesando cluster: %s", nombre)

    resultados[[nombre]] <- sintetizar(
      data_cluster,
      titulo = paste("AGROSINTESIS -", nombre)
    )
  }

  resultados
}
