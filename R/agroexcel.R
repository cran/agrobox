#' Exporta tablas agrobox o agrosintesis a Excel
#'
#' Escribe una tabla individual o una lista de tablas
#' en un archivo Excel (.xlsx). Cuando se trata de una
#' lista, cada elemento se exporta como una hoja distinta.
#'
#' @param x data.frame o lista de data.frames.
#' @param file nombre del archivo Excel de salida.
#'
#' @return Invisiblemente TRUE si el archivo se genera correctamente.
#'
#' @examples
#' \dontrun{
#' # Ejemplo 1: tabla simple desde agrobox
#' aa <- agrobox(
#'   data = antigua2,
#'   variable = "harvwt",
#'   factor = "trt",
#'   test = "Duncan"
#' )
#'
#' write_agro_excel(aa$tabla, file = "tabla_simple.xlsx")
#'
#' # Ejemplo 2: lista de tablas desde agrosintesis
#' res <- agrosintesis(
#'   data = df_multi,
#'   variables = c("tn_ha", "peso_fruto"),
#'   estructura = "Variedad~Localidad",
#'   factor = "Fertilizante",
#'   factor2 = "Dosis",
#'   bloque = "Bloque",
#'   test = "Tukey"
#' )
#'
#' write_agro_excel(res, file = "resultados_clusters.xlsx")
#' }
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @export
agroexcel <- function(x, file = "resultados_agro.xlsx") {

  wb <- openxlsx::createWorkbook()

  # --------------------------------------------------
  # CASO 1: tabla simple (aa$tabla)
  # --------------------------------------------------
  if (inherits(x, c("data.frame", "tbl_df"))) {

    openxlsx::addWorksheet(wb, "Resultados")

    openxlsx::writeData(
      wb,
      sheet     = "Resultados",
      x         = x,
      withFilter = TRUE
    )

    # --------------------------------------------------
    # CASO 2: lista de tablas (agrosintesis)
    # --------------------------------------------------
  } else if (is.list(x)) {

    for (nombre in names(x)) {

      # Limpiar nombre de hoja (Excel + Windows safe)
      sheet_name <- gsub("[^A-Za-z0-9_ ]", "_", nombre)
      sheet_name <- substr(sheet_name, 1, 31)

      openxlsx::addWorksheet(wb, sheet_name)

      openxlsx::writeData(
        wb,
        sheet      = sheet_name,
        x          = x[[nombre]],
        withFilter = TRUE
      )
    }

  } else {
    stop("El objeto debe ser una tabla o una lista de tablas.")
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  invisible(TRUE)
}
