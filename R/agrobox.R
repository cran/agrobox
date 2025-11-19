#' Genera un grafico y analisis estadistico por grupo
#'
#'
#' Esta funcion realiza analisis estadisticos por grupos definidos (clusteres)
#' usando ANOVA y pruebas post-hoc (Duncan o Tukey),
#' y genera un grafico tipo boxplot con etiquetas de medias, letras de significancia,
#' coeficiente de variacion (CV) y potencia estadistica (Power).
#'
#'
#' @param data Un data frame que contiene los datos.
#' @param test Tipo de prueba post-hoc a usar: `"Duncan"` (por defecto) o `"Tukey"`.
#' @param factor Variable categorica principal (factor 1 o tratamiento).
#' @param factor2 (Opcional) Segundo factor para interaccion (usado en ANOVA).
#' @param orden_factor (Opcional) Vector con el orden deseado para los niveles del `factor`.
#' @param bloque (Opcional) Variable de bloque para disenos con bloques completos.
#' @param variable Variable numerica dependiente.
#' @param niveles_factor (Opcional) Etiquetas personalizadas para los niveles del `factor`.
#' @param titulo Etiqueta para el eje Y del grafico (por ejemplo: "Altura (cm)").
#' @param estructura (Opcional) Formula tipo `Grupo1 ~ Grupo2` para definir los clusteres.
#' @param lim_sup (Opcional) Limite superior para el eje Y.
#' @param lim_inf (Opcional) Limite inferior para el eje Y.
#' @param colores Vector de colores para los niveles del `factor`.
#'
#' @return Una lista con:
#' \describe{
#'   \item{plot}{Objeto `ggplot` con el grafico generado.}
#'   \item{levels}{Niveles del factor utilizados.}
#' }
#'
#' @import dplyr ggplot2 agricolae pwr stringr
#' @examples
#' # Ejemplo de un experimento factorial con bloques y estructura bifactorial
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' set.seed(123)
#'
#' # Diseno factorial con bloques (ej. 4 bloques por Localidad-Variedad)
#' df_experimento <- expand.grid(
#'   Fertilizante = c("A", "B", "C"),
#'   Dosis = c("Baja", "Media", "Alta"),
#'   Localidad = c("Loc1", "Loc2"),
#'   Variedad = c("Var1", "Var2", "Var3"),
#'   Bloque = paste0("B", 1:4)  # Bloques, no repeticiones
#' )
#'
#' # Simular rendimiento (tn/ha) con efectos reales
#' df_experimento$tn_ha <- 20 +
#'   ifelse(df_experimento$Fertilizante == "B", 2,
#'          ifelse(df_experimento$Fertilizante == "C", 4, 0)) +
#'   ifelse(df_experimento$Dosis == "Media", 1,
#'          ifelse(df_experimento$Dosis == "Alta", 2, 0)) +
#'   ifelse(df_experimento$Localidad == "Loc2", 0.5, 0) +
#'   ifelse(df_experimento$Variedad == "Var2", 0.5,
#'          ifelse(df_experimento$Variedad == "Var3", -0.5, 0)) +
#'   rnorm(nrow(df_experimento), mean = 0, sd = 1.2)
#'
#' # Ejecutar la funcion agrobox
#' agrobox(
#'   data = df_experimento,     # La data que usaremos
#'   test = "Duncan",           # Seleccionamos el test que deseamos utilizar,
#'   #puede ser Tukey o Duncan
#'   factor = "Fertilizante",   # Si tenemos un factorial aca colocamos el factor
#'   #que nos interesa mostrar,
#'   #suponiendo que previamente ya demostramos que no hay interaccion.
#'   factor2 = "Dosis",         # Si tenemos un factorial este es el segundo factor,
#'   #sino es un factorial solo se coloca NULL
#'   orden_factor = c("A", "C", "B"),   # Ordenamos los niveles del factor principal
#'   #si fuera necesario, sino solo colocamos NULL
#'   bloque = "Bloque",                 # En campo siempre se deben tener bloques.
#'   variable = "tn_ha",               # Es la variable respuesta
#'   niveles_factor = c("A" = "Nitrato de amonio",
#'             "B" = "Fosfato diamonico",
#'             "C" = "Sulfato de amonio"),  # Si deseamos cambiar de nombre a los niveles del
#'   #factor principal aca podemos hacerlo, de lo contrario colocar NULL
#'   titulo = "Rendimiento (tn/ha)",        # El titulo del grafico.
#'   estructura = "Localidad~Variedad",     # Aca se colocan los grupo que deseamos evaluar,
#'   #se realizara un ANOVA y un TEST POST-HOC por cada grupo.
#'   lim_sup = NULL, lim_inf = NULL,        # podemos colocar los limites del eje Y si deseamos.
#'   colores = c("A" = "#1b9e77",
#'               "B" = "#d95f02",
#'               "C" = "#7570b3")           # Se coloca los colores de cada nivel del factor principal.
#' )$plot                                   # Recordemos que la funcion nos da una lista,
#' #de la cual si deseamos obtener el plot debemos seleccionarlo de esta forma.
#' @export



agrobox <- function(data,
                    test = c("Duncan", "Tukey"),
                    factor,
                    factor2 = NULL,
                    orden_factor = NULL,
                    bloque = NULL,
                    variable,
                    niveles_factor = NULL,
                    titulo = NULL,
                    estructura = NULL,
                    lim_sup = NULL,
                    lim_inf = NULL,
                    colores = NULL) {
  # Dependencias
  #requireNamespace("dplyr")
  #requireNamespace("stringr")
  #requireNamespace("rlang")
  #requireNamespace("ggplot2")
  #requireNamespace("lubridate")
  #requireNamespace("janitor")
  #requireNamespace("pwr")
  #requireNamespace("agricolae")

  test <- match.arg(test)

  # --- Preparar datos basicos ----------------------------------------------
  df <- dplyr::as_tibble(data)

  # Forzar factor
  df[[factor]] <- as.factor(df[[factor]])

  # Reordenar niveles si viene orden_factor
  if (!is.null(orden_factor)) {
    df[[factor]] <- factor(df[[factor]], levels = orden_factor)
  }

  # --- Parsear 'estructura' en grupos (grupe1, grupe2) ----------------------
  grupe1 <- ""
  grupe2 <- ""
  if (!is.null(estructura) && nzchar(estructura)) {
    parts <- stringr::str_split_fixed(estructura, "~", 2)
    grupe1 <- stringr::str_trim(parts[, 1])
    grupe2 <- stringr::str_trim(parts[, 2])
  }

  # Helper para chequear nombre valido (nchar > 0)
  has_name <- function(x) nzchar(x) && !is.na(x)

  # --- Crear columna cluster y calcular meds2 (promedios por cluster) -------
  # Funcion auxiliar que filtra columnas existentes y crea cluster segun grupos
  make_cluster_col <- function(df) {
    df2 <- df

    if (has_name(grupe1) && has_name(grupe2) &&
        all(c(grupe1, grupe2) %in% names(df2))) {
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = paste(.data[[grupe1]], .data[[grupe2]], sep = "_"))
    } else if (has_name(grupe1) && (grupe1 %in% names(df2))) {
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe1]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = paste(.data[[grupe1]], sep = "_"))
    } else if (has_name(grupe2) && (grupe2 %in% names(df2))) {
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]]),
                      !is.na(.data[[grupe2]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = paste(.data[[grupe2]], sep = "_"))
    } else {
      df2 <- df2 %>%
        dplyr::filter(!is.na(.data[[factor]]),
                      !is.na(.data[[variable]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(cluster = "A")
    }
    df2
  }

  data2 <- make_cluster_col(df)

  # meds2: promedio por factor x cluster (robusto ante la presencia/ausencia de grupe1/grupe2)
  #meds2 <- data2 %>%
  #  dplyr::group_by(.data[[factor]], cluster) %>%
  # dplyr::summarise(meds2 = mean(.data[[variable]], na.rm = TRUE), .groups = "drop") %>%
  #dplyr::distinct()

  # --- Preparar bucle por cluster (evitar rbind en cada iteracion) ----------
  clusters <- unique(data2$cluster)
  oti_list <- list()
  cv_list <- list()
  power_list <- list()

  run_anova_for_group <- function(datis, formula_term, factor_name, variable_name, test_method) {
    res <- list(oti = NULL, cv = NA_real_, power = NA_real_, shapiro_p = NA_real_, fligner_p = NA_real_)
    tryCatch({
      ## Preparar conteos y medias por tratamiento (siempre)
      qq <- ifelse(is.na(datis[[variable_name]]), 0, 1)
      datis2 <- datis %>%
        dplyr::mutate(qq = qq) %>%
        dplyr::group_by(cluster, .data[[factor_name]]) %>%
        dplyr::mutate(n = sum(qq, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sum_n = min(n, na.rm = TRUE))

      # tabla de medias siempre (una fila por nivel del factor)
      means_tbl <- datis2 %>%
        dplyr::group_by(.data[[factor_name]]) %>%
        dplyr::summarise(medias = mean(.data[[variable_name]], na.rm = TRUE),
                         n = sum(!is.na(.data[[variable_name]])),
                         .groups = "drop") %>%
        dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))

      ## Validaciones para ANOVA/post-hoc
      cond_valid <- all(!is.infinite(datis2$sum_n)) &&
        all(!is.na(datis2$sum_n)) &&
        all(datis2$sum_n >= 3) &&
        suppressWarnings(max(abs(datis2[[variable_name]]), na.rm = TRUE) > 0) &&
        length(unique(datis2[[factor_name]])) >= 2

      # Si no cumple condiciones, devolver solo medias (groups = NA) y P-values si posibles
      if (!cond_valid) {
        # intentamos calcular shapiro/fligner si es posible (protegido)
        sh_p <- tryCatch(stats::shapiro.test(stats::residuals(stats::lm(stats::reformulate(factor_name, response = variable_name), datis2)))$p.value,
                         error = function(e) NA_real_)
        fl_p <- tryCatch(stats::fligner.test(stats::reformulate(factor_name, response = variable_name), data = datis2)$p.value,
                         error = function(e) NA_real_)
        res$shapiro_p <- sh_p
        res$fligner_p <- fl_p

        res$oti <- means_tbl %>% dplyr::mutate(groups = NA_character_)

        if (ss_p <= 0.05 || ff_p <= 0.05) message(sprintf("Cluster %s: shapiro p=%.3g fligner p=%.3g  no post-hoc", grp, ss_p, ff_p))

        return(res)
      }

      ## Intentar ANOVA y post-hoc
      lm_fit <- stats::lm(formula_term, datis2)
      aov_fit <- stats::aov(lm_fit)

      ss_p <- tryCatch(stats::shapiro.test(stats::residuals(aov_fit))$p.value, error = function(e) NA_real_)
      ff_p <- tryCatch(stats::fligner.test(stats::reformulate(factor_name, response = variable_name), data = datis2)$p.value, error = function(e) NA_real_)

      # Guardamos p-values en el resultado (siempre)
      res$shapiro_p <- ss_p
      res$fligner_p <- ff_p


      # Si las pruebas no pasan, devolvemos medias con groups = NA
      if (is.na(ss_p) || is.na(ff_p) || ss_p <= 0.05 || ff_p <= 0.05) {
        res$oti <- means_tbl %>% dplyr::mutate(groups = NA_character_)
        return(res)
      }


      # Ejecutar post-hoc (protegido)
      ph <- tryCatch({
        if (test_method == "Tukey") {
          agricolae::HSD.test(lm_fit, factor_name, group = TRUE)
        } else {
          agricolae::duncan.test(lm_fit, factor_name, group = TRUE)
        }
      }, error = function(e) NULL)

      # Construir groups_df de forma robusta si ph existe
      if (!is.null(ph) && !is.null(ph$groups)) {
        groups_df <- as.data.frame(ph$groups)
        if (ncol(groups_df) >= 1) names(groups_df)[1] <- "medias"
        if (!("groups" %in% names(groups_df))) names(groups_df)[ncol(groups_df)] <- "groups"
        groups_df <- groups_df %>%
          dplyr::mutate(!!factor_name := rownames(groups_df)) %>%
          dplyr::select(!!rlang::sym(factor_name), medias, groups) %>%
          dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))
      } else {
        groups_df <- means_tbl %>% dplyr::mutate(groups = NA_character_) %>% dplyr::select(!!rlang::sym(factor_name), medias, groups)
      }

      # Calcular CV
      df_res <- stats::df.residual(aov_fit)
      MSerror <- tryCatch(stats::deviance(aov_fit) / df_res, error = function(e) NA_real_)
      cv_val <- if (!is.na(MSerror) && mean(datis2[[variable_name]], na.rm = TRUE) != 0) {
        sqrt(MSerror) / mean(datis2[[variable_name]], na.rm = TRUE) * 100
      } else NA_real_

      # Calcular potencia aproximada (protegido)
      power_val <- NA_real_
      anova_res <- tryCatch(summary(aov_fit), error = function(e) NULL)
      if (!is.null(anova_res) && "Sum Sq" %in% colnames(anova_res[[1]])) {
        ss_table <- anova_res[[1]]
        eta2 <- ss_table$`Sum Sq`[1] / sum(ss_table$`Sum Sq`, na.rm = TRUE)
        effect_size <- sqrt(eta2 / (1 - eta2))
        k <- length(unique(datis2[[factor_name]]))
        n_per_group <- nrow(datis2) / k
        pwr_res <- tryCatch(pwr::pwr.anova.test(k = k, n = n_per_group, f = effect_size, sig.level = 0.05), error = function(e) NULL)
        power_val <- if (!is.null(pwr_res)) pwr_res$power else NA_real_
      }

      res$oti <- groups_df
      res$cv <- cv_val
      res$power <- power_val
      return(res)

    }, error = function(e) {
      # En caso de error inesperado, devolver medias sin groups
      means_tbl2 <- tryCatch({
        datis %>%
          dplyr::group_by(.data[[factor_name]]) %>%
          dplyr::summarise(medias = mean(.data[[variable_name]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(!!factor_name := as.character(.data[[factor_name]]))
      }, error = function(e2) NULL)
      if (!is.null(means_tbl2)) {
        res$oti <- means_tbl2 %>% dplyr::mutate(groups = NA_character_)
      } else {
        res$oti <- NULL
      }
      res$cv <- NA_real_
      res$power <- NA_real_
      res$shapiro_p <- NA_real_
      res$fligner_p <- NA_real_
      return(res)
    })
  }



  # Construir formula base segun combinaciones de factor/factor2/bloque
  # Nota: reformulate espera vector de terms en RHS
  build_formula <- function(factor_name, factor2_name, bloque_name, response_name) {
    if (is.null(factor2_name) && is.null(bloque_name)) {
      frm <- stats::reformulate(factor_name, response = response_name)
    } else if (!is.null(bloque_name) && is.null(factor2_name)) {
      frm <- stats::reformulate(c(factor_name, bloque_name), response = response_name)
    } else if (is.null(bloque_name) && !is.null(factor2_name)) {
      frm <- stats::reformulate(paste0(factor_name, "*", factor2_name), response = response_name)
    } else {
      frm <- stats::reformulate(c(paste0(factor_name, "*", factor2_name), bloque_name), response = response_name)
    }
    frm
  }


  # Iterar clusters
  for (grp in clusters) {
    datis <- data2 %>% dplyr::filter(cluster == grp)
    if (nrow(datis) == 0) next
    formula_curr <- build_formula(factor, factor2, bloque, variable)
    res_anova <- run_anova_for_group(datis, formula_curr, factor, variable, test)
    if (!is.null(res_anova$oti)) {
      oti_df <- res_anova$oti %>% dplyr::mutate(cluster = grp,
                                                shapiro_p = res_anova$shapiro_p,
                                                fligner_p = res_anova$fligner_p)
      oti_list[[length(oti_list) + 1]] <- oti_df
    } else {
      # guardar estructura vacia con columnas esperadas para mantener consistencia
      empty_cols <- c("groups", factor)
      oti_list[[length(oti_list) + 1]] <- dplyr::tibble(!!!rlang::set_names(rep(list(character(0)), length(empty_cols)), empty_cols), cluster = character(0))
    }
    cv_list[[length(cv_list) + 1]] <- dplyr::tibble(cluster = grp, CV = res_anova$cv)
    power_list[[length(power_list) + 1]] <- dplyr::tibble(cluster = grp, Power = res_anova$power)
  }

  # Unir resultados
  oti_all <- dplyr::bind_rows(oti_list)
  cv_all <- dplyr::bind_rows(cv_list)
  power_all <- dplyr::bind_rows(power_list)

  # Merge con meds2
  # Asegurar types
  #meds2 <- meds2 %>%
  # dplyr::mutate(!!factor  as.character(.data[[factor]]),
  #              cluster = as.character(cluster))

  oti_merged <- oti_all %>%
    #dplyr::mutate(!!factor  as.character(factor)) %>%
    #dplyr::left_join(meds2, by = c(factor, "cluster")) %>%
    dplyr::left_join(cv_all, by = "cluster") %>%
    dplyr::left_join(power_all, by = "cluster") %>%
    dplyr::distinct()

  # Vector de niveles del factor
  dosis.a <- levels(as.factor(data2[[factor]]))
  if (is.null(niveles_factor)) niveles_factor <- dosis.a

  # Limites y colores
  max_val <- suppressWarnings(max(data2[[variable]], na.rm = TRUE))
  min_val <- suppressWarnings(min(data2[[variable]], na.rm = TRUE))
  if (is.null(lim_sup)) lim_sup <- ifelse(is.finite(max_val) && !is.na(max_val), max_val * 1.2, NA_real_)
  if (is.null(lim_inf)) lim_inf <- ifelse(is.finite(min_val) && !is.na(min_val),
                                          ifelse(min_val <= 0, min_val * 2, min_val * 0.7),
                                          NA_real_)

  if (is.null(colores)) {
    # palette por defecto si no se provee
    colores <- grDevices::hcl.colors(length(dosis.a), "Dynamic")
  }

  # --- Generar grafico ----------------------------------------------------
  p_base <- ggplot2::ggplot(data2, ggplot2::aes(y = .data[[variable]], x = .data[[factor]], color = .data[[factor]])) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(alpha = 0.4, size = 1) +
    ggplot2::labs(y = titulo, x = NULL, col = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colores, breaks = dosis.a, labels = niveles_factor)

  if (!is.null(estructura) && nzchar(estructura)) {
    p_base <- p_base + ggplot2::facet_grid(estructura, switch = "y", space = "free")
  }

  # Anadir textos si hay CV info en oti_merged
  # --- Anadir textos si hay CV info en oti_merged (parche robusto según grupe1/grupe2) ---
  if ("CV" %in% names(oti_merged)) {
    # determinar nombres de grupos que definiste al inicio (pueden ser "" si no aplica)
    g1 <- if (exists("grupe1") && nzchar(grupe1)) grupe1 else NA_character_
    g2 <- if (exists("grupe2") && nzchar(grupe2)) grupe2 else NA_character_

    # si no hay cluster, no hacemos separacion
    if (!("cluster" %in% names(oti_merged))) {
      oti_merged2 <- oti_merged
    } else if (!is.na(g1) && !is.na(g2)) {
      # ambos grupos presentes: separar en dos columnas con los nombres correspondientes
      oti_merged2 <- oti_merged %>%
        tidyr::separate(cluster,
                        into = c(g1, g2),
                        sep = "_",
                        remove = FALSE,
                        extra = "merge",
                        fill = "right") %>%
        dplyr::mutate(
          !!g1 := as.character(.data[[g1]]),
          !!g2 := as.character(.data[[g2]])
        )
    } else if (!is.na(g1) && is.na(g2)) {
      # solamente grupe1: extraer la primera parte antes del "_" (si existe)
      oti_merged2 <- oti_merged %>%
        tidyr::separate(cluster,
                        into = c(g1, "rest"),
                        sep = "_",
                        remove = FALSE,
                        extra = "merge",
                        fill = "right") %>%
        dplyr::select(-dplyr::any_of("rest")) %>%
        dplyr::mutate(!!g1 := as.character(.data[[g1]]))
    } else if (is.na(g1) && !is.na(g2)) {
      # solamente grupe2: extraer la ultima parte (lo hacemos invirtiendo)
      oti_merged2 <- oti_merged %>%
        dplyr::mutate(.tmp_split = stringr::str_split(cluster, "_")) %>%
        dplyr::mutate(!!g2 := vapply(.tmp_split, function(x) utils::tail(x, 1), FUN.VALUE = character(1))) %>%
        dplyr::select(-.tmp_split) %>%
        dplyr::mutate(!!g2 := as.character(.data[[g2]]))
    } else {
      # ni grupe1 ni grupe2 definidos: no modificar
      oti_merged2 <- oti_merged
    }

    # seguridad: si las nuevas columnas no existen, crear vacias (evita errores siguientes)
    if (exists("oti_merged2")) {
      if (!is.na(g1) && !(g1 %in% names(oti_merged2))) oti_merged2[[g1]] <- NA_character_
      if (!is.na(g2) && !(g2 %in% names(oti_merged2))) oti_merged2[[g2]] <- NA_character_
    } else {
      oti_merged2 <- oti_merged
    }
  }


  # preparar labels: una fila por cluster (primera por cluster)
  labels_corner <- oti_merged2 %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # formatear p-values con 3 decimales; si NA, dejar vacio
      shapiro_lbl = ifelse(!is.na(shapiro_p), formatC(shapiro_p, digits = 3, format = "f"), NA_character_),
      fligner_lbl = ifelse(!is.na(fligner_p), formatC(fligner_p, digits = 3, format = "f"), NA_character_),
      # construir etiqueta priorizando CV/Power si existen, si no usar p-values
      corner_label = dplyr::case_when(
        !is.na(CV) | !is.na(Power) ~ paste0("CV: ", ifelse(is.na(CV), "N/A", round(CV,2)), "%\nPower: ", ifelse(is.na(Power), "N/A", round(Power,2))),
        !is.na(shapiro_lbl) | !is.na(fligner_lbl) ~ paste0("Shapiro p: ", ifelse(is.na(shapiro_lbl), "N/A", shapiro_lbl),
                                                           "\nFligner p: ", ifelse(is.na(fligner_lbl), "N/A", fligner_lbl)),
        TRUE ~ NA_character_
      )
    )

  # --- Construir grafico final usando text_labels ---
  p1 <- p_base +
    # medias (numeros)
    ggplot2::geom_text(
      data = oti_merged2,
      ggplot2::aes(x = .data[[factor]], y = medias, label = round(medias, 2)),
      color = "black", size = 3.3, vjust = -3.5
    ) +
    # letras posthoc (grupos)
    ggplot2::geom_text(
      data = oti_merged2,
      ggplot2::aes(x = .data[[factor]], y = medias, label = groups),
      color = "red", size = 3 ,vjust = -2.8, fontface = "bold"
    ) +
    # CV y Power por cluster (esquina)
    ggplot2::geom_text(
      data = labels_corner %>% dplyr::filter(!is.na(corner_label)),
      ggplot2::aes(x = Inf, y = Inf, label = corner_label),
      hjust = 1.1, vjust = 1.1, size = 3, color = "blue",
      inherit.aes = FALSE
    )

  # aplicar limites y retornar
  if (!is.na(lim_inf) && !is.na(lim_sup)) {
    p1 <- p1 + ggplot2::scale_y_continuous(limits = c(lim_inf, lim_sup))
  }

  return(list(plot = p1, levels = dosis.a))
}

