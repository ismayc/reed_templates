#' Creates an R Markdown PDF Thesis document
#'
#' This is a function called in output in the YAML of the driver Rmd file to specify using the Reed
#' Senior Thesis LaTeX template and cls files.
#'
#' @export
#' @param toc A Boolean (TRUE or FALSE) specifying where table of contents should be created
#' @param toc_depth A positive integer
#' @return A modified \code{pdf_document} based on the Reed Senior Thesis LaTeX
#'   template
#' @examples
#' \dontrun{
#'  output:
#'    reedtemplates::reed_thesis:
#'      toc: true
#' }
reed_thesis <- function(toc = TRUE, toc_depth = 3) {
  template <- find_resource("reed_thesis", "template.tex")

  base <- rmarkdown::pdf_document(template = template,
                                  toc = toc,
                                  toc_depth = toc_depth,
                                  highlight = "pygments",
                                  keep_tex = TRUE,
                                  pandoc_args = "--chapters")

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_knit$out.format <- "sweave"
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"

  # List of available options at
  # http://yihui.name/knitr/options/#chunk_options


  ## Commented out to restore to default highlighting
  # hook_chunk <- function(x, options) {
  #   if (knitr:::output_asis(x, options)) return(x)
  #   paste0('\\begin{CodeChunk}\n', x, '\\end{CodeChunk}')
  # }
  # hook_input <- function(x, options) {
  #   paste0(c('\\begin{CodeInput}', x, '\\end{CodeInput}', ''),
  #          collapse = '\n')
  # }
  #
  # hook_output <- function(x, options) {
  #   paste0('\\begin{CodeOutput}\n', x, '\\end{CodeOutput}\n')
  # }
  #
  # base$knitr$knit_hooks$chunk   <- hook_chunk
  # base$knitr$knit_hooks$source  <- hook_input
  # base$knitr$knit_hooks$output  <- hook_output
  # base$knitr$knit_hooks$message <- hook_output
  # base$knitr$knit_hooks$warning <- hook_output
  base$knitr$knit_hooks$plot <- knitr:::hook_plot_tex

  base
}

#' Adds a label to a figure/chemical reaction and includes the figure in the
#' document
#'
#' This allows users to label and include R and non-R generated objects into
#' their document
#'
#' @export
#' @param path A string specifying where the object is stored as a file
#' @param caption A string providing the text that will appear with the object
#' @param label A string providing a name to use with \code{ref}
#' @param type A string (either "figure" or "chem.reac") providing object type
#' @param alt.cap A string giving an alternative caption to be included in the
#'   Table of Figures
#' @param cap.size A string giving the size of the caption.  Options are
#'   "scriptsize", "footnotesize", "small", "normalsize", "large", and "Large"
#' @param scale A numeric specifying the scale to display a figure (1
#'   corresponds to 100\% of original size, 0.5 corresponds to 50\% of original
#'   size, etc.)
#' @param angle A numeric specifying the degree to rotate a figure
#' @param options A string attempting to fix some of the "floating" issues with
#'   LaTeX images
#' @return LaTeX figure environment code that can be run with R chunk option
#'   \code{results="asis"} to produce object in document with given label and
#'   features
#' @examples
#' \dontrun{
#'  label(path = "figure/reed.jpg", caption = "Reed logo",
#'        label = "reed", type = "figure")
#' }
label <- function(path = NULL,
                  caption = "Default caption",
                  label = "def",
                  type = "figure",
                  alt.cap = caption,
                  cap.size = "normalsize",
                  scale = 1,
                  angle = 0,
                  options = "h!tbp"){
  if(type == "figure"){
    cat(
      paste0(
        "\n\\begin{figure}[", options, "]\n",
        "\\centering\n",
        "\\includegraphics[", angle = ", angle, ","
        scale = ", scale, "]{",
        path, "}\n",
        "\\caption[", caption,"]{\\", cap.size, "{", alt.cap,"}}\n",
        "\\label{fig:", label, "}\n",
        "\\end{figure}"
      )
    )
  }

  if(type == "chem.reac"){
    cat(
      paste0(
        "\n\\begin{figure}[", options, "]\n",
        "\\begin{center}\n",
        path, "\n",
        "\\caption{", caption,"}\n",
        "\\label{fig:", label, "}\n",
        "\\end{center}\n",
        "\\end{figure}"
      )
    )
  }

}

#' References previously created label
#'
#' This allows users to make a reference to an object imported using the
#' \code{label} function
#'
#' @export
#' @param label A string specifying object label given in \code{label} function
#'   call
#' @param type A string corresponding to "table" or "figure" label
#' @return LaTeX code to provide object number corresponding to \code{label}
#'   specified in \code{label} function
#' @examples
#' \dontrun{
#'  `r ref("subd2", type = "figure")`
#' }
ref <- function(label = "def", type = "figure"){
  paste0("\\autoref{",
         ifelse(type == "figure", "fig:",
            #    ifelse(type == "equation", "eq:",
                       ifelse(type == "table", "tab:","")
         #)
         ),
         label, "}")
}


