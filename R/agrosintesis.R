#' Sintetiza resultados estadísticos para múltiples variables
#'
#' Aplica \code{agrobox()} a una o varias variables y consolida
#' las tablas resumen. Permite análisis por clusters cuando se
#' define una estructura.
#'
#' @param data data.frame con los datos experimentales.
#' @param variables vector de nombres de variables respuesta.
#' @param estructura fórmula en texto tipo "Factor1~Factor2" para clusters.
#' @param ... argumentos adicionales pasados a \code{agrobox()}.
#'
#' @return
#' Un data.frame si no hay clusters, o una lista de data.frames
#' si se define \code{estructura}.
#'
#' @importFrom dplyr bind_cols distinct semi_join across all_of
#' @importFrom stats as.formula
#'
#' @export
agrosintesis <- function(data, variables, estructura = NULL, ...) {

  # --------------------------------------------------
  # CASO 1: SIN CLUSTERS
  # --------------------------------------------------
  if (is.null(estructura)) {

    tablas <- lapply(variables, function(v) {

      res <- agrobox(
        data       = data,
        variable   = v,
        estructura = NULL,
        ...
      )

      tab <- res$tabla
      colnames(tab)[1]    <- "trt"
      colnames(tab)[-1]   <- v
      tab
    })

    tabla_final <- Reduce(
      function(x, y) dplyr::bind_cols(x, y[-1]),
      tablas
    )

    return(tabla_final)
  }

  # --------------------------------------------------
  # CASO 2: CON CLUSTERS
  # --------------------------------------------------

  estructura_formula <- stats::as.formula(estructura)
  vars_cluster <- all.vars(estructura_formula)

  # Copia local de data (CRAN-friendly)
  data_local <- data

  # Asegurar factores
  data_local[vars_cluster] <- lapply(
    data_local[vars_cluster],
    function(x) factor(x, levels = unique(x))
  )

  clusters <- dplyr::distinct(
    data_local,
    dplyr::across(dplyr::all_of(vars_cluster))
  )

  resultados <- list()

  for (i in seq_len(nrow(clusters))) {

    filtro <- clusters[i, , drop = FALSE]

    data_cluster <- dplyr::semi_join(
      data_local,
      filtro,
      by = vars_cluster
    )

    tablas <- lapply(variables, function(v) {

      res <- agrobox(
        data       = data_cluster,
        variable   = v,
        estructura = NULL,
        ...
      )

      tab <- res$tabla
      colnames(tab)[1]  <- "trt"
      colnames(tab)[-1] <- v
      tab
    })

    tabla_final <- Reduce(
      function(x, y) dplyr::bind_cols(x, y[-1]),
      tablas
    )

    nombre <- paste(
      mapply(
        function(var, val) paste0(var, "=", as.character(val)),
        vars_cluster,
        filtro,
        SIMPLIFY = TRUE
      ),
      collapse = " | "
    )

    resultados[[nombre]] <- tabla_final
  }

  resultados
}
