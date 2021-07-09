#' Format Query Parameters
#'
#' This is a utility function used in \code{\link{insert_query_param}}. Given a
#' single query parameter-value pair, or ordered character vectors, it will
#' properly format and return a query string.
#'
#' @inheritParams insert_query_param
#'
#' @return A formatted query string
#' @export
#' @family Query Strings
#' @examples
#'
#' # Returns proper formatting for single query parameters: "?person=jdtrat"
#' format_query_param(param = "person",
#'                    value = "jdtrat")
#'
#' # Returns proper formatting for multiple query parameters:
#' # "?person=jdtrat&favorite_food=sushi&best_cat=tucker"
#' format_query_param(param = c("person", "favorite_food", "best_cat"),
#'                    value = c("jdtrat", "sushi", "tucker")
#'                    )
#'
format_query_param <- function(param, value) {

  if (length(param) != length(value)) {
    stop("Parameter and value vectors are different lengths.")
  }

  # Check how many parameter/values are supplied,
  # if more than one, return appropriate format
  if (length(param) != 1 & length(value) != 1) {
    first <- paste0("?", param[1], "=", value[1])
    rest <- paste0("&", param[-1], "=", value[-1], collapse = "")
    query_string <- paste0(first, rest)
    # if only one, return appropriate format
  } else if (length(param) == 1 & length(value) == 1) {
    query_string <- paste0("?", param, "=", value)
  }
  return(query_string)
}


#' Insert Query Parameters and Values
#'
#' This function can be used to insert query parameters and values into the URL.
#' This function wraps [shiny::updateQueryString()], allowing individual
#' character strings or character vectors to be appended (default) or replace existing URL
#' query strings.
#'
#' @param param The parameter name. Can be a character string, e.g. "userID", or
#'   a character vector, such as c("userID", "favorite_food").
#' @param value The parameter value. Can be a character string, "jdtrat", or a
#'   character vector, such as c("jdtrat", "sushi").
#' @param mode Either "append" (default) to add a new query parameter/value pair
#'   to the existing URL or "replace" to replace any existing query
#'   parameters/values with new ones.
#'
#' @return NA; used for side effects to insert query strings in a Shiny application's URL.
#' @export
#' @family Query Strings
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     actionButton("appendParam", "Append Query Parameters"),
#'     actionButton("replaceParam", "Replace Query Parameters"),
#'     verbatimTextOutput("query")
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     observe({
#'
#'       # On start up, insert the following query string:
#'       # "?person=jdt&food=sushi&best_cat=tucker"
#'       if (session$clientData$url_search == "") {
#'         insert_query_param(param = "startup-message",
#'                            value = "hello-world")
#'       }
#'
#'       # print all query params
#'       output$query <- renderText({
#'         query_values <- vapply(
#'           parseQueryString(session$clientData$url_search),
#'           function(x) x, character(1)
#'         )
#'         query_params <- names(query_values)
#'         paste0(query_params, ": ", query_values, "\n")
#'       })
#'     })
#'
#'     observeEvent(input$appendParam, {
#'
#'       insert_query_param(param = c("person", "food", "best_cat"),
#'                          value = c("jdt", "sushi", "tucker"),
#'                          mode = "append")
#'     })
#'
#'     observeEvent(input$replaceParam, {
#'       insert_query_param(param = "package",
#'                          value = "shinyrandomize",
#'                          mode = "replace")
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
insert_query_param <- function(param, value, mode = "append") {

  session <- shiny::getDefaultReactiveDomain()
  url_search <- session$clientData$url_search

  # if mode is append
  if (mode == "append") {
    # if there are query parameters already
    if (url_search != "") {
      # set first as existing params
      first <- url_search
      # set rest as all others
      rest <- paste0("&", param, "=", value, collapse = "")

      query_string <- paste0(first, rest)

      # If there are no query parameters already, treat as replacement
    } else if (url_search == "") {
      query_string <- format_query_param(param = param, value = value)
    }
    # Else if mode is replace
  } else if (mode == "replace") {
    query_string <- format_query_param(param = param, value = value)
  }

  shiny::updateQueryString(queryString = query_string,
                           mode = "push")

}

#' Extract parameters ID from query string
#'
#' This function is useful for parsing individual parameters from a query
#' string. It can be used both outside of and within a Shiny application.
#'
#' For use outside of a Shiny application, `extract_query_param()` is most
#' similar to [shiny::parseQueryString()], and within a Shiny application, it is
#' most similar to [shiny::getQueryString()]. Both shiny functions return named
#' lists of all query parameters and their values. In contrast,
#' `extract_query_param()` returns a character string representing the value of
#' the supplied parameter.
#'
#'
#' @param param The query parameter whose value should be retrieved.
#' @param active_session LOGICAL: TRUE (default) for use in interactive shiny
#'   sessions. FALSE for parsing a character string supplied by `query_string`
#'   with query parameters.
#' @param query_string If not run in an active session, the character string
#'   containing query parameters to parse.
#'
#' @export
#' @return The value from the requested query parameter.
#' @family Query Strings
#'
#' @examples
#'
#' # Outside of a Shiny application
#'
#' extract_query_param(param = "best_cat",
#'                     active_session = FALSE,
#'                     query_string = "?person=jdt&favorite_food=sushi&best_cat=tucker")
#'
#'
#' # Within a Shiny application
#'
#' if (interactive()) {
#'
#' library(shiny)
#' library(shinyrandomize)
#'
#' ui <- fluidPage(
#'   textInput("extractingParam", "Which parameter do you want to extract?"),
#'   verbatimTextOutput("query")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   # On start up, insert the following query string:
#'   # "?person=jdt&favorite_food=sushi&best_cat=tucker"
#'   observe({
#'     if (session$clientData$url_search == "") {
#'       insert_query_param(param = c("person", "favorite_food", "best_cat"),
#'                          value = c("jdt", "sushi", "tucker"))
#'     }
#'   })
#'
#'   # When one of the query parameters, 'person', 'food',
#'   # or 'best_cat' is specified, extract and show it.
#'   # The output will only appear for a valid parameter.
#'   observeEvent(input$extractingParam, {
#'     output$query <- renderText({
#'       extract_query_param(param = input$extractingParam)
#'     })
#'   })
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#'
extract_query_param <- function(param, active_session = TRUE, query_string) {

  if (active_session) {
    session <- shiny::getDefaultReactiveDomain()
    query <- shiny::parseQueryString(session$clientData$url_search)
  } else {
    if (missing(query_string)) {
      stop("Function not run within a Shiny application. Must provide a query string to parse.")
    }
    query <- shiny::parseQueryString(query_string)
  }

  regmatches(query[[param]], regexpr(pattern = "[^*/]+", text = query[[param]]))

}




