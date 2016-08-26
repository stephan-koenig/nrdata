#' Formats and outputs a character object as a console message.
#'
#' @input text

system_message <- function(text) {

    message(noquote(text))
}
