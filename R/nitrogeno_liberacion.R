#' Curva de liberacion de nitrogeno
#'
#' Datos experimentales de liberación de nitrógeno (NH4, NO3 y K)
#' en función del tiempo bajo diferentes formulaciones y estados
#' (fertilizante vs pellet).
#'
#' Experimental data of nitrogen release (NH4, NO3 and K)
#' over time under different formulations and physical states
#' (fertilizer vs pellet).
#'
#' @format A data frame with 60 rows and 13 variables:
#' \describe{
#'   \item{formu}{Formulacion (ej. "14.4.4", "7.7.19")}
#'   \item{estado}{Tipo de producto (fertilizante o pellet)}
#'   \item{trata}{Tratamiento experimental (T1–T4)}
#'   \item{fecha}{Fecha de evaluacion}
#'   \item{dias}{Dias desde el inicio del experimento}
#'   \item{rep}{Repeticion}
#'   \item{vol_extractante}{Volumen del extractante (mL)}
#'   \item{ml_lixiviado}{Volumen lixiviado (mL)}
#'   \item{ph_lixiviado}{pH del lixiviado}
#'   \item{ce_lixiviado}{Conductividad electrica del lixiviado}
#'   \item{nh4_mg_lt}{Amonio (mg/L)}
#'   \item{no3_mg_lt}{Nitrato (mg/L)}
#'   \item{k_mg_lt}{Potasio (mg/L)}
#' }
#'
#' @source Datos de tesis del autor
"nitrogeno_liberacion"
