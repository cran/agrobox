#' Exporta tablas de resultados a PNG via LaTeX (modo rapido)
#'
#' Genera una imagen PNG de alta calidad a partir de una tabla (data.frame)
#' o una lista de tablas (por ejemplo salida de \code{agrosintesis()}),
#' utilizando LaTeX para el renderizado y convirtiendo el PDF resultante
#' en imagen.
#'
#' Esta funcion esta pensada para generar reportes rapidos y elegantes.
#' Debido a que crea archivos PDF y PNG, sus ejemplos no se ejecutan
#' automaticamente durante los checks de CRAN.
#'
#' @param x data.frame o lista de data.frames. Puede ser \code{aa$tabla}
#'   o una lista producida por \code{agrosintesis()}.
#' @param out_dir directorio donde se guardaran los archivos generados.
#' @param file_stub nombre base del archivo cuando \code{x} es una tabla unica.
#' @param dpi resolucion en puntos por pulgada usada al convertir PDF a PNG.
#'
#' @return Invisiblemente TRUE si la exportacion se realiza correctamente.
#'
#' @details
#' Los nombres de archivos se sanitizan automaticamente para ser compatibles
#' con Windows, LaTeX y sistemas de archivos estandar. Los guiones bajos en los
#' encabezados de columnas se escapan para evitar errores de compilacion LaTeX.
#'
#' @examples
#' \dontrun{
#' # Ejemplo 1: tabla individual desde agrobox
#' aa <- agrobox(
#'   data = antigua2,
#'   variable = "harvwt",
#'   factor = "trt",
#'   test = "Duncan"
#' )
#'
#' write_rapido(
#'   aa$tabla,
#'   out_dir = "reportes_png",
#'   file_stub = "Rendimiento_site"
#' )
#'
#' # Ejemplo 2: multiples tablas desde agrosintesis
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
#' write_rapido(
#'   res,
#'   out_dir = "png_clusters"
#' )
#' }
#'
#' @importFrom kableExtra kbl kable_styling row_spec
#' @importFrom magick image_read_pdf image_trim image_border image_write
#' @importFrom tinytex latexmk
#'
#' @export
agrotabla <- function(x,
                         out_dir = getwd(),
                         file_stub = "TABLA",
                         dpi = 600) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  safe_name <- function(z) {
    z <- iconv(z, to = "ASCII//TRANSLIT")
    z <- gsub("[^A-Za-z0-9_-]", "_", z)
    z <- gsub("_+", "_", z)
    z <- gsub("^_|_$", "", z)
    z
  }

  render_one <- function(tabla, nombre = NULL) {

    if (!inherits(tabla, c("data.frame", "tbl_df"))) {
      stop("Cada elemento debe ser una tabla.")
    }

    # Escapar guiones bajos para LaTeX
    colnames(tabla) <- gsub("_", "\\\\_", colnames(tabla))

    sufijo <- if (!is.null(nombre)) safe_name(nombre) else safe_name(file_stub)

    tex_file <- file.path(out_dir, paste0(sufijo, ".tex"))
    pdf_file <- file.path(out_dir, paste0(sufijo, ".pdf"))
    png_file <- file.path(out_dir, paste0(sufijo, ".png"))

    kbl_tex <- kableExtra::kbl(
      tabla,
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = c("l", rep("c", ncol(tabla) - 1))
    ) %>%
      kableExtra::kable_styling(
        latex_options = "hold_position",
        position      = "center",
        full_width    = FALSE
      ) %>%
      kableExtra::row_spec(0, bold = TRUE) %>%
      kableExtra::row_spec(1, bold = TRUE)

    kbl_tex <- gsub("\\\\addlinespace(\\[[^\\]]*\\])?\\s*", "", kbl_tex)

    wrapper <- paste0(
      "\\documentclass[landscape]{article}\n",
      "\\usepackage[margin=0.5cm]{geometry}\n",
      "\\usepackage{booktabs}\n",
      "\\usepackage{makecell}\n",
      "\\pagestyle{empty}\n",
      "\\begin{document}\n",
      "\\begin{center}\n",
      "\\Large\n",
      kbl_tex, "\n",
      "\\end{center}\n",
      "\\end{document}\n"
    )

    writeLines(wrapper, tex_file)
    tinytex::latexmk(tex_file)

    img <- magick::image_read_pdf(pdf_file, density = dpi)
    img <- magick::image_trim(img)
    img <- magick::image_border(img, color = "white", geometry = "20x20")
    magick::image_write(img, path = png_file, format = "png")

    message("PNG generado: ", png_file)
  }

  if (inherits(x, c("data.frame", "tbl_df"))) {
    render_one(x, file_stub)
    return(invisible(TRUE))
  }

  if (is.list(x)) {
    for (nm in names(x)) {
      render_one(x[[nm]], nm)
    }
    return(invisible(TRUE))
  }

  stop("El objeto debe ser una tabla o una lista de tablas.")
}
