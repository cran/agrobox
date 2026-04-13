#' Boxplot con anotaciones post-hoc estadísticas para experimentos agronómicos
#' / Boxplot with Statistical Post-hoc Annotations for Agronomic Experiments
#'
#' Genera boxplots con puntos jitter, etiquetas de medias y letras de comparación
#' múltiple para experimentos agronómicos. Soporta facetas con hasta dos variables
#' de agrupación y selecciona automáticamente el método estadístico en función de
#' la normalidad (Shapiro-Wilk) y la homogeneidad de varianzas (Fligner-Killeen).
#'
#' Generates boxplots with jittered points, mean labels, and post-hoc letter
#' annotations for agronomic experiments. Supports faceting by up to two grouping
#' variables and automatic selection of the statistical method based on normality
#' (Shapiro-Wilk) and homoscedasticity (Fligner-Killeen) tests.
#'
#' @details
#' La función aplica la siguiente lógica de decisión para cada panel (cluster):
#'
#' The function applies the following decision logic for each facet panel (cluster):
#'
#' \enumerate{
#'   \item Prueba de Shapiro-Wilk sobre los residuos del ANOVA. Si la normalidad falla
#'         (p <= 0.05), solo se muestran las medias sin letras.
#'
#'         Shapiro-Wilk test on ANOVA residuals. If normality fails (p <= 0.05),
#'         only means are shown with no letters.
#'
#'   \item Si \code{var.equal = TRUE} (ANOVA clásico): se aplica la prueba de
#'         Fligner-Killeen. Si falla la homogeneidad, solo se muestran medias.
#'         Si pasa, se usa ANOVA + post-hoc Duncan o Tukey.
#'
#'         If \code{var.equal = TRUE} (classic ANOVA path): Fligner-Killeen test is
#'         applied. If homoscedasticity fails, only means are shown. Otherwise,
#'         ANOVA + Duncan or Tukey post-hoc is used.
#'
#'   \item Si \code{var.equal = FALSE} (ruta Welch):
#'     \itemize{
#'       \item Si Fligner pasa (p > 0.05): se usa ANOVA estándar + Duncan o Tukey.
#'       \item Si Fligner falla: se usa Welch + Games-Howell.
#'       \item CV y Power solo se reportan cuando ff_p > 0.01.
#'       \item Si alguna comparación tiene p-valor NA o falta algún grupo en las
#'             letras, se eliminan las letras para todo el cluster.
#'     }
#'
#'     If \code{var.equal = FALSE} (Welch path):
#'     \itemize{
#'       \item If Fligner passes (p > 0.05): standard ANOVA + Duncan or Tukey is used.
#'       \item If Fligner fails: Welch test + Games-Howell post-hoc is used.
#'       \item CV and Power are reported only when ff_p > 0.01.
#'       \item If any pairwise comparison has NA p-value or any group is missing
#'             from the letter display, letters are suppressed for the entire cluster.
#'     }
#' }
#'
#' Los argumentos \code{orden_factor}, \code{grupo1_orden} y \code{grupo2_orden}
#' aceptan vectores nombrados o no:
#'
#' The \code{orden_factor}, \code{grupo1_orden}, and \code{grupo2_orden}
#' arguments accept either unnamed or named vectors:
#'
#' \itemize{
#'   \item Vector sin nombre: solo cambia el orden.
#'   \item Vector nombrado: cambia orden y etiquetas.
#' }
#'
#' \itemize{
#'   \item Unnamed vector: order only.
#'   \item Named vector: order + relabeling.
#' }
#'
#' Para vectores nombrados, el nombre corresponde al nivel original y el valor
#' es la etiqueta mostrada. Elementos sin nombre conservan su valor original.
#'
#' For named vectors, names correspond to original levels and values to display
#' labels. Elements without a name keep their original value.
#'
#' Si algún tratamiento tiene todas sus observaciones como NA dentro de un cluster,
#' la función se detiene mostrando un error informativo.
#'
#' If any treatment has all observations as NA within any cluster, the function
#' stops with an informative error.
#'
#' @param data Data frame con los datos experimentales /
#'   A data frame containing the experimental data.
#'
#' @param test Método post-hoc: \code{"Duncan"} (default) o \code{"Tukey"} /
#'   Post-hoc test: \code{"Duncan"} (default) or \code{"Tukey"}.
#'
#' @param var.equal Si TRUE usa ANOVA clásico; si FALSE activa Welch /
#'   Logical. If TRUE uses classic ANOVA; if FALSE enables Welch path.
#'
#' @param factor Nombre de la variable categórica principal /
#'   Name of the main categorical variable (treatment).
#'
#' @param factor2 Segundo factor opcional para interacción /
#'   Optional second factor for interaction.
#'
#' @param orden_factor Vector para ordenar/renombrar tratamientos /
#'   Vector to reorder/relabel factor levels.
#'
#' @param grupo1_orden Igual que orden_factor pero para filas de facetas /
#'   Same as orden_factor for row facets.
#'
#' @param grupo2_orden Igual que orden_factor pero para columnas de facetas /
#'   Same as orden_factor for column facets.
#'
#' @param bloque Variable de bloque opcional /
#'   Optional blocking variable.
#'
#' @param variable Variable numérica de respuesta /
#'   Numeric response variable.
#'
#' @param titulo Etiqueta del eje Y /
#'   Y-axis label.
#'
#' @param estructura Especificación de facetas ("fila~columna") /
#'   Faceting structure ("row~col").
#'
#' @param lim_sup Límite superior del eje Y /
#'   Upper y-axis limit.
#'
#' @param lim_inf Límite inferior del eje Y /
#'   Lower y-axis limit.
#'
#' @param colores Vector de colores /
#'   Color vector for factor levels.
#'
#' @return Lista con cuatro elementos /
#'   A list with four elements:
#' \describe{
#'   \item{\code{plot}}{Objeto ggplot2 con boxplots y anotaciones /
#'     ggplot2 object with boxplots and annotations}
#'   \item{\code{tabla}}{Tabla resumen con medias, letras, ANOVA, CV y Power /
#'     Summary table with means, letters, ANOVA, CV and Power}
#'   \item{\code{levels}}{Niveles del factor mostrados /
#'     Displayed factor levels}
#'   \item{\code{data}}{Datos procesados usados en el análisis /
#'     Processed data used in the analysis}
#' }
#'
#' #'
#' @importFrom dplyr as_tibble filter mutate group_by summarise ungroup
#'   distinct left_join bind_rows select pull across all_of any_of
#'   relocate slice case_when if_else n_distinct recode
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_text labs
#'   theme_bw theme element_text element_blank scale_color_manual
#'   scale_y_continuous facet_grid
#' @importFrom stringr str_split_fixed str_trim str_split
#' @importFrom tidyr separate pivot_wider unite
#' @importFrom rlang sym set_names
#' @importFrom stats reformulate lm aov shapiro.test fligner.test
#'   df.residual deviance setNames sd
#' @importFrom grDevices hcl.colors
#' @importFrom utils tail head
#' @importFrom agricolae HSD.test duncan.test
#' @importFrom pwr pwr.anova.test
#' @importFrom rstatix games_howell_test
#' @importFrom multcompView multcompLetters
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1: Single panel, classic ANOVA with Duncan test
#' set.seed(42)
#' df1 <- data.frame(
#'   trat = rep(c("T0", "T1", "T2", "T3"), each = 5),
#'   resp = c(
#'     rnorm(5, 10, 1),
#'     rnorm(5, 14, 1),
#'     rnorm(5, 18, 1),
#'     rnorm(5, 22, 1)
#'   )
#' )
#'
#' result1 <- agrobox(
#'   data         = df1,
#'   factor       = "trat",
#'   variable     = "resp",
#'   titulo       = "Response variable",
#'   orden_factor = c("T0" = "Control", "T1" = "Low",
#'                    "T2" = "Medium",  "T3" = "High"),
#'   var.equal    = TRUE,
#'   test         = "Duncan"
#' )
#' result1$plot
#' result1$tabla
#'
#' # Example 2: One grouping variable (column facets), Welch + Games-Howell
#' set.seed(99)
#' df2 <- data.frame(
#'   trat  = rep(c("A", "B", "C", "D"), each = 12),
#'   epoca = rep(rep(c("Dry", "Rainy", "Transition"), each = 4), 4),
#'   resp  = c(
#'     rnorm(12, 5,  0.3),
#'     rnorm(12, 9,  3.5),
#'     rnorm(12, 7,  2.0),
#'     rnorm(12, 12, 4.8)
#'   )
#' )
#'
#' result2 <- agrobox(
#'   data         = df2,
#'   factor       = "trat",
#'   variable     = "resp",
#'   estructura   = "~epoca",
#'   titulo       = "Response by season",
#'   grupo2_orden = c("Dry", "Transition", "Rainy"),
#'   orden_factor = c("A" = "Control",     "B" = "Treatment 1",
#'                    "C" = "Treatment 2", "D" = "Treatment 3"),
#'   var.equal    = FALSE,
#'   colores      = c("gray40", "steelblue", "orange", "red3")
#' )
#' result2$plot
#' result2$tabla
#'
#' # Example 3: Two grouping variables (row x column facets), with NAs
#' set.seed(7)
#' df3 <- expand.grid(
#'   trat  = c("T1", "T2", "T3"),
#'   suelo = c("Clay", "Sand"),
#'   riego = c("Drip", "Sprinkler", "Rainfed"),
#'   rep   = 1:5
#' )
#' df3$yield <- with(df3, {
#'   base  <- c(T1 = 8,    T2 = 14,   T3 = 20)[as.character(trat)]
#'   s     <- c(Clay = 0,  Sand = 3)[as.character(suelo)]
#'   r     <- c(Drip = 2,  Sprinkler = 0, Rainfed = -4)[as.character(riego)]
#'   rnorm(nrow(df3), base + s + r, 2)
#' })
#' set.seed(15)
#' df3$yield[sample(nrow(df3), size = round(nrow(df3) * 0.08))] <- NA
#'
#' result3 <- agrobox(
#'   data         = df3,
#'   factor       = "trat",
#'   variable     = "yield",
#'   estructura   = "suelo~riego",
#'   titulo       = "Yield (t/ha)",
#'   orden_factor = c("T1" = "Variety 1", "T2" = "Variety 2",
#'                    "T3" = "Variety 3"),
#'   grupo1_orden = c("Clay" = "Clay soil", "Sand" = "Sandy soil"),
#'   grupo2_orden = c("Rainfed", "Drip", "Sprinkler"),
#'   var.equal    = FALSE,
#'   colores      = c("darkgreen", "royalblue", "firebrick")
#' )
#' result3$plot
#' result3$tabla
#' result3$data
#'
#' # Example 4: Real Data nitrogeno_liberacion
#' data(nitrogeno_liberacion)
#'
#' agrobox(
#' data = nitrogeno_liberacion,
#' test = "Tukey",
#' factor = "trata",
#' variable = "nh4_mg_lt",
#' orden_factor = c( "T1" = "Fertilizante (14-4-4)",
#'                   "T2" = "Pellet (14-4-4)",
#'                   "T3" = "Fertilizante (7-7-19)",
#'                   "T4" = "Pellet (7-7-19)"),
#' grupo1_orden = c("14.4.4" = "Ley 14 N - 4 P2O5 - 4 K2O",
#'                  "7.7.19" = "Ley 7 N - 7 P2O5 - 19 K2O"),
#' grupo2_orden = c("0" = "Día 0",
#'                  "1" = "Día 1",
#'                  "8" = "Día 8",
#'                  "16" = "Día 16",
#'                  "41" = "Día 41"),
#' estructura = "formu~dias",
#' colores = c("purple3", "green4","black", "red3")
#' )
#'
#'
#' # Example 5: Real Data pimiento_hibridacion
#'
#' data(pimiento_hibridacion)
#'
#'
#' agrobox(
#' data = pimiento_hibridacion,
#' test = "Tukey",
#' factor = "trata",
#' bloque = "bloque",
#' variable = "g_pla",
#' titulo = "Rendimiento (g/pla)",
#' orden_factor = c( "T1" = "5",
#'                   "T2" = "4",
#'                   "T3" = "3"),
#' grupo2_orden = c("1" = "Día 0",
#'                  "4"= "Día 4",
#'                  "8" = "Día 8",
#'                  "12" = "Día 12",
#'                  "16" = "Día 16",
#'                  "20" = "Día 20"),
#' estructura = "~dia",
#' colores = c("purple3", "green4","black", "red3")
#' )$plot +
#'   ggplot2::labs(col = "Semanas de hibridación")
#'
#'
#' @export
agrobox <- function(data,
                    test      = c("Duncan", "Tukey"),
                    var.equal = TRUE,
                    factor,
                    factor2   = NULL,
                    orden_factor  = NULL,
                    grupo1_orden  = NULL,
                    grupo2_orden  = NULL,
                    bloque    = NULL,
                    variable,
                    titulo    = NULL,
                    estructura = NULL,
                    lim_sup   = NULL,
                    lim_inf   = NULL,
                    colores   = NULL) {

  # =========================================================================
  # agrobox()
  #
  # Generates boxplots with statistical post-hoc letter annotations for
  # agronomic experiments. Supports faceting by up to two grouping variables,
  # factor reordering and relabeling, and automatic selection between ANOVA
  # (Duncan / Tukey) and Welch + Games-Howell depending on normality and
  # homoscedasticity tests.
  #
  # Returns a list with:
  #   $plot   - ggplot2 object
  #   $tabla  - summary table (tibble) with means, letters, ANOVA sig, CV, Power
  #   $levels - character vector of factor levels as used in the plot
  #   $data   - summary of the processed data used for analysis
  # =========================================================================

  test <- match.arg(test)

  # -------------------------------------------------------------------------
  # HELPER: apply order and optional relabeling to a factor column.
  #
  # orden_vec can be:
  #   - NULL              : convert to factor with alphabetical levels
  #   - unnamed vector    : c("T2", "T0", "T1")  -> order only
  #   - named vector      : c("T2" = "Medio", "T0" = "Testigo", "T1")
  #                         names = original levels, values = display labels
  #                         elements without a name keep their original value
  # -------------------------------------------------------------------------
  aplicar_orden_labels <- function(x, orden_vec) {

    x <- as.character(x)

    if (is.null(orden_vec)) {
      return(factor(x))
    }

    # Normalize: fill empty names with the element value itself
    # so every element follows the pattern  original_level = display_label
    nms <- names(orden_vec)
    if (is.null(nms)) {
      names(orden_vec) <- orden_vec
    } else {
      faltantes <- nms == ""
      names(orden_vec)[faltantes] <- orden_vec[faltantes]
    }

    niveles_originales <- names(orden_vec)
    labels_deseados    <- unname(orden_vec)

    factor(x, levels = niveles_originales, labels = labels_deseados)
  }

  # -------------------------------------------------------------------------
  # Coerce data to tibble and apply factor ordering / labeling
  # -------------------------------------------------------------------------
  df <- dplyr::as_tibble(data)
  df[[factor]] <- aplicar_orden_labels(df[[factor]], orden_factor)

  # -------------------------------------------------------------------------
  # Parse the 'estructura' string into grupe1 (rows) and grupe2 (columns)
  # Format expected: "row_var~col_var", "~col_var", or "row_var~"
  # -------------------------------------------------------------------------
  grupe1 <- ""
  grupe2 <- ""
  if (!is.null(estructura) && nzchar(estructura)) {
    parts  <- stringr::str_split_fixed(estructura, "~", 2)
    grupe1 <- stringr::str_trim(parts[, 1])
    grupe2 <- stringr::str_trim(parts[, 2])
  }

  # Helper: TRUE when a string is non-empty and non-NA
  has_name <- function(x) nzchar(x) && !is.na(x)

  # -------------------------------------------------------------------------
  # Pre-check on raw data: if any treatment has ALL observations as NA within
  # any cluster combination, stop before any processing begins.
  # The check runs on the original data before NA rows are dropped.
  # -------------------------------------------------------------------------
  df_check <- dplyr::as_tibble(data)

  check_groups <- c(
    factor,
    if (has_name(grupe1) && grupe1 %in% names(df_check)) grupe1 else NULL,
    if (has_name(grupe2) && grupe2 %in% names(df_check)) grupe2 else NULL
  )

  conteos_check <- df_check %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(check_groups))) %>%
    dplyr::summarise(
      n_validos = sum(!is.na(.data[[variable]])),
      .groups   = "drop"
    )

  vacios_check <- conteos_check %>%
    dplyr::filter(n_validos == 0)

  if (nrow(vacios_check) > 0) {
    detalle <- vacios_check %>%
      tidyr::unite("combinacion", dplyr::all_of(check_groups), sep = " | ") %>%
      dplyr::pull(combinacion)

    stop(
      "Analysis cannot proceed: the following treatment x group combinations ",
      "have no valid observations (all NA):\n  ",
      paste(detalle, collapse = "\n  "),
      "\nPlease remove or impute these rows before calling agrobox()."
    )
  }

  # -------------------------------------------------------------------------
  # Build the 'cluster' column that identifies each facet panel.
  # The cluster drives the loop over ANOVA groups.
  #
  # Cases:
  #   1. grupe1 + grupe2 -> interaction(grupe1, grupe2, sep = "_")
  #   2. grupe1 only     -> cluster = grupe1
  #   3. grupe2 only     -> cluster = grupe2
  #   4. no groups       -> cluster = "A" (single panel)
  # -------------------------------------------------------------------------
  make_cluster_col <- function(df) {

    if (has_name(grupe1) && grupe1 %in% names(df)) {
      df[[grupe1]] <- aplicar_orden_labels(df[[grupe1]], grupo1_orden)
    }

    if (has_name(grupe2) && grupe2 %in% names(df)) {
      df[[grupe2]] <- aplicar_orden_labels(df[[grupe2]], grupo2_orden)
    }

    df2 <- df

    if (has_name(grupe1) && has_name(grupe2) &&
        all(c(grupe1, grupe2) %in% names(df2))) {

      # Case 1: both grouping variables present
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = interaction(.data[[grupe1]], .data[[grupe2]],
                                sep = "_", drop = TRUE)
        )

    } else if (has_name(grupe1) && grupe1 %in% names(df2)) {

      # Case 2: only grupe1
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = factor(.data[[grupe1]],
                           levels = levels(.data[[grupe1]]))
        )

    } else if (has_name(grupe2) && grupe2 %in% names(df2)) {

      # Case 3: only grupe2
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = factor(.data[[grupe2]],
                           levels = levels(.data[[grupe2]]))
        )

    } else {

      # Case 4: no grouping - single panel
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = factor("A", levels = "A"))
    }

    return(df2)
  }

  data2 <- make_cluster_col(df)

  # -------------------------------------------------------------------------
  # Games-Howell post-hoc test with compact letter display.
  # Used when var.equal = FALSE and homoscedasticity fails.
  # Returns NULL if any pairwise p-value is NA or any group is missing
  # from the letter display, suppressing all letters for that cluster.
  # -------------------------------------------------------------------------
  games_howell_letras <- function(datis2, formula_term, factor_name, variable_name) {

    df_games <- tryCatch(
      rstatix::games_howell_test(datis2, formula_term),
      error = function(e) NULL
    )
    if (is.null(df_games)) return(NULL)

    pvals <- dplyr::select(df_games, group1, group2, p.adj)

    # If any pairwise comparison has NA p-value, the test is unreliable.
    # Suppress all letters for this cluster rather than showing partial results.
    if (any(is.na(pvals$p.adj))) return(NULL)

    grupos <- unique(c(df_games$group1, df_games$group2))

    # Build full symmetric p-value matrix
    mat_full <- matrix(1,
                       nrow     = length(grupos),
                       ncol     = length(grupos),
                       dimnames = list(grupos, grupos))

    for (i in seq_len(nrow(pvals))) {
      mat_full[pvals$group1[i], pvals$group2[i]] <- pvals$p.adj[i]
      mat_full[pvals$group2[i], pvals$group1[i]] <- pvals$p.adj[i]
    }

    letras <- multcompView::multcompLetters(mat_full < 0.05)

    means <- datis2 %>%
      dplyr::group_by(.data[[factor_name]]) %>%
      dplyr::summarise(
        medias = mean(.data[[variable_name]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(medias))

    letras_vec <- letras$Letters

    # If any group is missing from the letter display, the comparison is
    # incomplete and statistically invalid. Suppress all letters.
    grupos_faltantes <- setdiff(means[[factor_name]], names(letras_vec))
    if (length(grupos_faltantes) > 0) return(NULL)

    niveles_letras <- unique(letras_vec)
    nuevas_letras  <- stats::setNames(letters[seq_along(niveles_letras)],
                                      niveles_letras)
    letras_final   <- nuevas_letras[letras_vec[means[[factor_name]]]]

    means %>%
      dplyr::mutate(groups = letras_final) %>%
      dplyr::select(!!rlang::sym(factor_name), medias, groups)
  }

  # -------------------------------------------------------------------------
  # Core statistical analysis for one cluster (facet panel).
  #
  # Decision logic:
  #   1. Shapiro-Wilk normality test on residuals
  #      -> fails (p <= 0.05): return means only, no letters
  #   2. var.equal = TRUE  (classic ANOVA path)
  #      -> Fligner homoscedasticity test
  #         fails: return means only, no letters
  #         passes: ANOVA + Duncan or Tukey
  #   3. var.equal = FALSE (Welch path)
  #      -> Fligner passes: use ANOVA + Duncan / Tukey anyway
  #      -> Fligner fails:  use Welch + Games-Howell letters
  #         CV / Power reported only when ff_p > 0.01
  # -------------------------------------------------------------------------
  run_anova_for_group <- function(datis, formula_term, factor_name,
                                  variable_name, test_method) {

    res <- list(oti       = NULL,
                cv        = NA_real_,
                power     = NA_real_,
                shapiro_p = NA_real_,
                fligner_p = NA_real_,
                anova_p   = NA_real_,
                metodo    = NA_character_)

    tryCatch({

      # Count valid observations per treatment
      qq    <- ifelse(is.na(datis[[variable_name]]), 0L, 1L)
      datis2 <- datis %>%
        dplyr::mutate(qq = qq) %>%
        dplyr::group_by(cluster, .data[[factor_name]]) %>%
        dplyr::mutate(n = sum(qq, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sum_n = min(n, na.rm = TRUE))

      # Means table (always computed regardless of ANOVA outcome)
      means_tbl <- datis2 %>%
        dplyr::group_by(.data[[factor_name]]) %>%
        dplyr::summarise(
          medias = mean(.data[[variable_name]], na.rm = TRUE),
          n      = sum(!is.na(.data[[variable_name]])),
          .groups = "drop"
        ) %>%
        dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))

      # Minimum requirements to run ANOVA
      cond_valid <- all(!is.infinite(datis2$sum_n)) &&
        all(!is.na(datis2$sum_n))                   &&
        all(datis2$sum_n >= 3)                       &&
        suppressWarnings(
          max(abs(datis2[[variable_name]]), na.rm = TRUE) > 0
        )                                            &&
        length(unique(datis2[[factor_name]])) >= 2

      if (!cond_valid) {
        warning("ANOVA skipped for cluster '", unique(datis$cluster),
                "': insufficient data.")
        res$shapiro_p <- tryCatch(
          stats::shapiro.test(
            stats::residuals(
              stats::lm(
                stats::reformulate(factor_name, response = variable_name),
                datis2
              )
            )
          )$p.value,
          error = function(e) NA_real_
        )
        res$fligner_p <- tryCatch(
          stats::fligner.test(
            stats::reformulate(factor_name, response = variable_name),
            data = datis2
          )$p.value,
          error = function(e) NA_real_
        )
        res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
        return(res)
      }

      # Fit model and run diagnostic tests
      lm_fit  <- stats::lm(formula_term, datis2)
      aov_fit <- stats::aov(lm_fit)

      ss_p    <- tryCatch(
        stats::shapiro.test(stats::residuals(aov_fit))$p.value,
        error = function(e) NA_real_
      )
      ff_p    <- tryCatch(
        stats::fligner.test(
          stats::reformulate(factor_name, response = variable_name),
          data = datis2
        )$p.value,
        error = function(e) NA_real_
      )
      anova_p <- tryCatch(
        summary(aov_fit)[[1]][["Pr(>F)"]][1],
        error = function(e) NA_real_
      )

      res$shapiro_p <- ss_p
      res$fligner_p <- ff_p
      res$anova_p   <- anova_p

      # Helper: extract post-hoc groups from agricolae output
      extract_ph_groups <- function(ph, factor_name, means_tbl) {
        if (!is.null(ph) && !is.null(ph$groups)) {
          gdf <- as.data.frame(ph$groups)
          names(gdf)[1] <- "medias"
          if (!"groups" %in% names(gdf)) names(gdf)[ncol(gdf)] <- "groups"
          gdf %>%
            dplyr::mutate(!!factor_name := rownames(gdf)) %>%
            dplyr::select(!!rlang::sym(factor_name), medias, groups) %>%
            dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))
        } else {
          dplyr::mutate(means_tbl, groups = NA_character_)
        }
      }

      # Helper: compute CV and power from ANOVA fit
      compute_cv_power <- function(aov_fit, datis2, variable_name, factor_name) {
        df_res      <- stats::df.residual(aov_fit)
        MSerror     <- stats::deviance(aov_fit) / df_res
        cv_val      <- sqrt(MSerror) /
          mean(datis2[[variable_name]], na.rm = TRUE) * 100

        ss_table    <- summary(aov_fit)[[1]]
        eta2        <- ss_table$`Sum Sq`[1] /
          sum(ss_table$`Sum Sq`, na.rm = TRUE)
        effect_size <- sqrt(eta2 / (1 - eta2))
        k           <- length(unique(datis2[[factor_name]]))
        n_per_group <- nrow(datis2) / k

        pwr_res <- tryCatch(
          pwr::pwr.anova.test(k         = k,
                              n         = n_per_group,
                              f         = effect_size,
                              sig.level = 0.05),
          error = function(e) NULL
        )
        power_val <- if (!is.null(pwr_res)) pwr_res$power else NA_real_
        list(cv = cv_val, power = power_val)
      }

      # --- Step 1: normality gate ---
      if (is.na(ss_p) || ss_p <= 0.05) {
        res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
        return(res)
      }

      # --- Step 2: var.equal = TRUE (classic ANOVA) ---
      if (var.equal) {

        if (is.na(ff_p) || ff_p <= 0.05) {
          res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
          return(res)
        }

        ph <- tryCatch(
          if (test_method == "Tukey") {
            agricolae::HSD.test(lm_fit, factor_name, group = TRUE)
          } else {
            agricolae::duncan.test(lm_fit, factor_name, group = TRUE)
          },
          error = function(e) NULL
        )

        stats_out  <- compute_cv_power(aov_fit, datis2, variable_name, factor_name)
        res$cv     <- stats_out$cv
        res$power  <- stats_out$power
        res$oti    <- extract_ph_groups(ph, factor_name, means_tbl)
        res$metodo <- "ANOVA"
        return(res)
      }

      # --- Step 3: var.equal = FALSE ---

      # Scenario A: homoscedasticity holds -> use standard ANOVA anyway
      if (!is.na(ff_p) && ff_p > 0.05) {

        ph <- tryCatch(
          if (test_method == "Tukey") {
            agricolae::HSD.test(lm_fit, factor_name, group = TRUE)
          } else {
            agricolae::duncan.test(lm_fit, factor_name, group = TRUE)
          },
          error = function(e) NULL
        )

        stats_out  <- compute_cv_power(aov_fit, datis2, variable_name, factor_name)
        res$cv     <- stats_out$cv
        res$power  <- stats_out$power
        res$oti    <- extract_ph_groups(ph, factor_name, means_tbl)
        res$metodo <- "ANOVA"
        return(res)
      }

      # Scenario B: heteroscedasticity -> Welch + Games-Howell
      groups_df <- games_howell_letras(datis2, formula_term,
                                       factor_name, variable_name)

      if (is.null(groups_df)) {
        res$oti <- dplyr::mutate(means_tbl, groups = NA_character_)
        return(res)
      }

      # Report CV / Power only when ff_p is not extremely small (> 0.01)
      if (!is.na(ff_p) && ff_p > 0.01) {
        stats_out <- compute_cv_power(aov_fit, datis2, variable_name, factor_name)
        res$cv    <- stats_out$cv
        res$power <- stats_out$power
      }

      res$oti    <- groups_df
      res$metodo <- "Welch"
      return(res)

    }, error = function(e) {

      # Fallback: return means without letters on unexpected error
      means_tbl2 <- tryCatch(
        datis %>%
          dplyr::group_by(.data[[factor_name]]) %>%
          dplyr::summarise(
            medias = mean(.data[[variable_name]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(!!factor_name := as.character(.data[[factor_name]])),
        error = function(e2) NULL
      )

      res$oti    <- if (!is.null(means_tbl2))
        dplyr::mutate(means_tbl2, groups = NA_character_) else NULL
      res$metodo <- "-"
      return(res)
    })
  }

  # -------------------------------------------------------------------------
  # Build the model formula from factor, optional factor2, and optional block
  # -------------------------------------------------------------------------
  build_formula <- function(factor_name, factor2_name, bloque_name, response_name) {
    if (is.null(factor2_name) && is.null(bloque_name)) {
      stats::reformulate(factor_name, response = response_name)
    } else if (!is.null(bloque_name) && is.null(factor2_name)) {
      stats::reformulate(c(factor_name, bloque_name), response = response_name)
    } else if (is.null(bloque_name) && !is.null(factor2_name)) {
      stats::reformulate(paste0(factor_name, "*", factor2_name),
                         response = response_name)
    } else {
      stats::reformulate(c(paste0(factor_name, "*", factor2_name), bloque_name),
                         response = response_name)
    }
  }

  # -------------------------------------------------------------------------
  # Loop over clusters: run ANOVA / Welch for each facet panel
  # -------------------------------------------------------------------------
  clusters   <- unique(data2$cluster)
  oti_list   <- list()
  cv_list    <- list()
  power_list <- list()

  for (grp in clusters) {
    datis        <- dplyr::filter(data2, cluster == grp)
    if (nrow(datis) == 0) next

    formula_curr <- build_formula(factor, factor2, bloque, variable)
    res_anova    <- run_anova_for_group(datis, formula_curr,
                                        factor, variable, test)

    if (!is.null(res_anova$oti)) {
      oti_list[[length(oti_list) + 1]] <- res_anova$oti %>%
        dplyr::mutate(
          cluster   = grp,
          shapiro_p = res_anova$shapiro_p,
          fligner_p = res_anova$fligner_p,
          anova_p   = res_anova$anova_p,
          metodo    = res_anova$metodo
        )
    } else {
      empty_cols <- c("groups", factor)
      oti_list[[length(oti_list) + 1]] <- dplyr::tibble(
        !!!rlang::set_names(rep(list(character(0)), length(empty_cols)),
                            empty_cols),
        cluster = character(0)
      )
    }

    cv_list[[length(cv_list) + 1]] <- dplyr::tibble(
      cluster = grp,
      CV      = res_anova$cv
    )
    power_list[[length(power_list) + 1]] <- dplyr::tibble(
      cluster = grp,
      Power   = res_anova$power
    )
  }

  # -------------------------------------------------------------------------
  # Merge all results into a single table
  # -------------------------------------------------------------------------
  oti_all   <- dplyr::bind_rows(oti_list)
  cv_all    <- dplyr::bind_rows(cv_list)
  power_all <- dplyr::bind_rows(power_list)

  oti_merged <- oti_all %>%
    dplyr::left_join(cv_all,    by = "cluster") %>%
    dplyr::left_join(power_all, by = "cluster") %>%
    dplyr::distinct()

  # -------------------------------------------------------------------------
  # Factor levels and color palette
  # -------------------------------------------------------------------------
  dosis.a      <- levels(as.factor(data2[[factor]]))
  labels_union <- dosis.a   # display labels (equal to levels after relabeling)

  max_val <- suppressWarnings(max(data2[[variable]], na.rm = TRUE))
  min_val <- suppressWarnings(min(data2[[variable]], na.rm = TRUE))

  if (is.null(lim_sup))
    lim_sup <- ifelse(is.finite(max_val) && !is.na(max_val),
                      max_val * 1.2, NA_real_)
  if (is.null(lim_inf))
    lim_inf <- ifelse(is.finite(min_val) && !is.na(min_val),
                      ifelse(min_val <= 0, min_val * 2, min_val * 0.7),
                      NA_real_)

  if (is.null(colores))
    colores <- grDevices::hcl.colors(length(dosis.a), "Dynamic")

  # Single-panel flag: no faceting when the only cluster is "A"
  clusters_unique <- unique(as.character(data2$cluster))
  single_A <- length(clusters_unique) == 1 && clusters_unique == "A"

  # -------------------------------------------------------------------------
  # Ensure facet variables are proper factors in data2 so that facet_grid
  # respects the level order defined by grupo1_orden / grupo2_orden
  # -------------------------------------------------------------------------
  if (has_name(grupe1) && grupe1 %in% names(data2))
    data2[[grupe1]] <- factor(data2[[grupe1]], levels = levels(data2[[grupe1]]))

  if (has_name(grupe2) && grupe2 %in% names(data2))
    data2[[grupe2]] <- factor(data2[[grupe2]], levels = levels(data2[[grupe2]]))

  # -------------------------------------------------------------------------
  # Build base ggplot
  # -------------------------------------------------------------------------
  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p_base <- ggplot2::ggplot(
    data2,
    ggplot2::aes(y     = .data[[variable]],
                 x     = .data[[factor]],
                 color = .data[[factor]])
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(alpha = 0.4, size = 1) +
    ggplot2::labs(y = titulo, x = NULL, col = NULL) +
    base_theme +
    ggplot2::scale_color_manual(values = colores,
                                breaks = dosis.a,
                                labels = labels_union)

  # Legend and x-axis text depend on whether there is more than one panel
  if (single_A) {
    p_base <- p_base + ggplot2::theme(legend.position = "none")
  } else {
    p_base <- p_base + ggplot2::theme(
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_blank()
    )
  }

  # Add faceting when estructura is provided
  if (!is.null(estructura) && nzchar(estructura))
    p_base <- p_base +
    ggplot2::facet_grid(estructura, switch = "y", space = "free")

  # -------------------------------------------------------------------------
  # Reconstruct grouping columns in oti_merged so that geom_text can be
  # placed inside the correct facet panel.
  # tidyr::separate() outputs character columns; we restore factor levels
  # from data2 so the facet assignment is correct.
  # -------------------------------------------------------------------------
  g1 <- if (has_name(grupe1)) grupe1 else NA_character_
  g2 <- if (has_name(grupe2)) grupe2 else NA_character_

  restore_levels <- function(df_target, g, data_src) {
    if (!is.na(g) && g %in% names(df_target) && g %in% names(data_src)) {
      df_target[[g]] <- factor(df_target[[g]], levels = levels(data_src[[g]]))
    }
    df_target
  }

  if (!"cluster" %in% names(oti_merged)) {
    oti_merged2 <- oti_merged

  } else if (!is.na(g1) && !is.na(g2)) {
    oti_merged2 <- oti_merged %>%
      tidyr::separate(cluster,
                      into   = c(g1, g2),
                      sep    = "_",
                      remove = FALSE,
                      extra  = "merge",
                      fill   = "right") %>%
      dplyr::mutate(!!g1 := as.character(.data[[g1]]),
                    !!g2 := as.character(.data[[g2]]))
    oti_merged2 <- restore_levels(oti_merged2, g1, data2)
    oti_merged2 <- restore_levels(oti_merged2, g2, data2)

  } else if (!is.na(g1) && is.na(g2)) {
    oti_merged2 <- oti_merged %>%
      tidyr::separate(cluster,
                      into   = c(g1, "rest"),
                      sep    = "_",
                      remove = FALSE,
                      extra  = "merge",
                      fill   = "right") %>%
      dplyr::select(-dplyr::any_of("rest")) %>%
      dplyr::mutate(!!g1 := as.character(.data[[g1]]))
    oti_merged2 <- restore_levels(oti_merged2, g1, data2)

  } else if (is.na(g1) && !is.na(g2)) {
    oti_merged2 <- oti_merged %>%
      dplyr::mutate(
        .tmp = stringr::str_split(cluster, "_"),
        !!g2 := vapply(.tmp, function(x) utils::tail(x, 1),
                       FUN.VALUE = character(1))
      ) %>%
      dplyr::select(-.tmp) %>%
      dplyr::mutate(!!g2 := as.character(.data[[g2]]))
    oti_merged2 <- restore_levels(oti_merged2, g2, data2)

  } else {
    oti_merged2 <- oti_merged
  }

  # Safety: create empty grouping columns if still missing
  if (!is.na(g1) && !(g1 %in% names(oti_merged2))) oti_merged2[[g1]] <- NA_character_
  if (!is.na(g2) && !(g2 %in% names(oti_merged2))) oti_merged2[[g2]] <- NA_character_

  # -------------------------------------------------------------------------
  # Prepare corner annotation label (CV, Power, or diagnostic p-values)
  # One label per cluster, placed at top-right of each facet panel
  # -------------------------------------------------------------------------
  labels_corner <- oti_merged2 %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      shapiro_lbl = ifelse(!is.na(shapiro_p),
                           formatC(shapiro_p, digits = 3, format = "f"),
                           NA_character_),
      fligner_lbl = ifelse(!is.na(fligner_p),
                           formatC(fligner_p, digits = 3, format = "f"),
                           NA_character_),
      corner_label = dplyr::case_when(
        !is.na(CV) | !is.na(Power) ~ paste0(
          "Metodo: ", ifelse(is.na(metodo), "-", metodo),
          "\nCV: ",    ifelse(is.na(CV),    "N/A", round(CV, 2)), "%",
          "\nPower: ", ifelse(is.na(Power), "N/A", round(Power, 2))
        ),
        !is.na(shapiro_lbl) | !is.na(fligner_lbl) ~ paste0(
          "Metodo: ",     ifelse(is.na(metodo),      "-",   metodo),
          "\nShapiro p: ", ifelse(is.na(shapiro_lbl), "N/A", shapiro_lbl),
          "\nFligner p: ", ifelse(is.na(fligner_lbl), "N/A", fligner_lbl)
        ),
        TRUE ~ paste0("Metodo: ", ifelse(is.na(metodo), "-", metodo))
      )
    )

  # -------------------------------------------------------------------------
  # Assemble final plot: add mean values, post-hoc letters, and corner labels
  # -------------------------------------------------------------------------
  p1 <- p_base +
    ggplot2::geom_text(
      data    = oti_merged2,
      mapping = ggplot2::aes(x     = .data[[factor]],
                             y     = medias,
                             label = round(medias, 2)),
      color   = "black",
      size    = 3.3,
      vjust   = -3.5
    ) +
    ggplot2::geom_text(
      data    = oti_merged2,
      mapping = ggplot2::aes(x     = .data[[factor]],
                             y     = medias,
                             label = groups),
      color    = "red",
      size     = 3,
      vjust    = -2.8,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data        = dplyr::filter(labels_corner, !is.na(corner_label)),
      mapping     = ggplot2::aes(x = Inf, y = Inf, label = corner_label),
      hjust       = 1.1,
      vjust       = 1.1,
      size        = 3,
      color       = "blue",
      inherit.aes = FALSE
    )

  if (!is.na(lim_inf) && !is.na(lim_sup))
    p1 <- p1 + ggplot2::scale_y_continuous(limits = c(lim_inf, lim_sup))

  # -------------------------------------------------------------------------
  # Summary table: means + letters + ANOVA significance + CV + Power
  # Rows: one per factor level, plus ANOVA / CV / Power footer rows
  # Columns: one per cluster (facet panel)
  # -------------------------------------------------------------------------
  tabla_resumen_anova <- function(data_in, factor_col) {

    df_t <- dplyr::as_tibble(data_in)

    if (!factor_col %in% names(df_t))
      stop("Column '", factor_col, "' not found in data.")

    # Apply display labels to factor column
    df_t <- df_t %>%
      dplyr::mutate(
        !!factor_col := factor(
          dplyr::recode(.data[[factor_col]],
                        !!!stats::setNames(labels_union, dosis.a)),
          levels = labels_union
        )
      )

    oti_clean <- df_t %>%
      dplyr::mutate(
        celda = dplyr::if_else(
          is.na(groups),
          sprintf("%.2f", medias),
          sprintf("%.2f (%s)", medias, groups)
        )
      )

    cluster_map <- oti_clean %>%
      dplyr::distinct(cluster) %>%
      dplyr::arrange(cluster) %>%
      dplyr::mutate(col_name = as.character(cluster))

    oti_labeled <- dplyr::left_join(oti_clean, cluster_map, by = "cluster")

    wide_main <- oti_labeled %>%
      dplyr::select(dplyr::all_of(factor_col), col_name, celda) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = col_name, values_from = celda)

    wide_main <- wide_main[order(wide_main[[factor_col]]), ]
    n_trat    <- dplyr::n_distinct(wide_main[[factor_col]])

    # ANOVA significance row
    if (!"anova_p" %in% names(oti_labeled)) {
      sig_row <- wide_main[1, ]
      sig_row[] <- ""
      sig_row[[factor_col]] <- "ANOVA"
    } else {
      sig_row <- oti_labeled %>%
        dplyr::select(cluster, CV, col_name, anova_p, metodo) %>%
        dplyr::distinct() %>%
        dplyr::mutate(sig = dplyr::case_when(
          is.na(CV)         ~ "-",
          is.na(anova_p)    ~ "",
          metodo == "Welch" ~ "-",
          anova_p < 0.001   ~ "***",
          anova_p < 0.01    ~ "**",
          anova_p < 0.05    ~ "*",
          TRUE              ~ "n.s."
        )) %>%
        dplyr::select(col_name, sig) %>%
        tidyr::pivot_wider(names_from = col_name, values_from = sig)
      sig_row[[factor_col]] <- "ANOVA"
      sig_row <- dplyr::relocate(sig_row, dplyr::all_of(factor_col), .before = 1)
    }

    # Helper for numeric footer rows (CV, Power)
    build_numeric_row <- function(var_name, label) {
      row <- oti_labeled %>%
        dplyr::select(col_name, !!rlang::sym(var_name)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          value = dplyr::if_else(is.na(.data[[var_name]]),
                                 "",
                                 sprintf("%.2f", .data[[var_name]]))
        ) %>%
        dplyr::select(col_name, value) %>%
        tidyr::pivot_wider(names_from = col_name, values_from = value)
      row[[factor_col]] <- label
      dplyr::relocate(row, dplyr::all_of(factor_col), .before = 1)
    }

    cv_row    <- build_numeric_row("CV",    "CV")
    power_row <- build_numeric_row("Power", "Power")

    tabla_final <- dplyr::bind_rows(wide_main, sig_row, cv_row, power_row)

    # Add a header row showing the factor column name
    fila_extra <- as.list(rep("", ncol(tabla_final)))
    names(fila_extra) <- names(tabla_final)
    fila_extra[[factor_col]] <- factor_col

    tabla_final2 <- dplyr::bind_rows(fila_extra, tabla_final)
    names(tabla_final2)[names(tabla_final2) == factor_col] <- ""

    # Format multi-part column names (grupe1_grupe2) for LaTeX makecell
    enc <- names(tabla_final2)
    enc[-1] <- vapply(enc[-1], function(x) {
      if (!grepl("_", x)) return(x)
      partes <- strsplit(x, "_", fixed = TRUE)[[1]]
      sprintf("\\makecell{%s \\\\ %s}",
              partes[1],
              paste(partes[-1], collapse = "_"))
    }, character(1))
    names(tabla_final2) <- enc

    tabla_final2
  }

  tabla <- tabla_resumen_anova(data_in    = oti_merged,
                               factor_col = factor)

  # -------------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------------

  # Build a clean summary of the processed data used in the analysis.
  # Contains only the columns involved: factor, variable, grouping variables,
  # and cluster. Means are computed per factor x cluster combination.
  cols_keep <- c(factor, variable,
                 if (has_name(grupe1)) grupe1 else NULL,
                 if (has_name(grupe2)) grupe2 else NULL,
                 "cluster")

  data_summary <- data2 %>%
    dplyr::select(dplyr::all_of(cols_keep)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      setdiff(cols_keep, variable)
    ))) %>%
    dplyr::summarise(
      mean    = round(mean(.data[[variable]], na.rm = TRUE), 3),
      sd      = round(stats::sd(.data[[variable]], na.rm = TRUE), 3),
      n       = sum(!is.na(.data[[variable]])),
      .groups = "drop"
    )

  anova_summary <- oti_merged %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      anova_p   = dplyr::first(anova_p),
      shapiro_p = dplyr::first(shapiro_p),
      fligner_p = dplyr::first(fligner_p),
      CV        = dplyr::first(CV),
      Power     = dplyr::first(Power),
      .groups = "drop"
    )

  list(
    plot   = p1,
    tabla  = tabla,
    levels = labels_union,
    data   = data_summary,
    stats  = anova_summary
  )
}

