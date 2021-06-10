#' Get a Random Link For Studies with Multiple Treatment Groups
#'
#' This function takes in a data frame of key-value paris for treatment groups
#' and links and returns a link that can be passed into the "onclick" attribute
#' of HTML buttons (e.g. [shiny::actionButton()]).
#'
#' @inheritParams randomizeButton
#'
#' @seealso randomizeButton
#' @return A link based on key-value pairs for treatment groups.
#' @export
#'
#' @examples
#'
#' keys <- data.frame(
#'   group = c("A", "B"),
#'   link = c("https://yourwebsite.com/this-task",
#'   "https://yourwebsite.com/this-other-task")
#' )
#'
#' if (interactive()) {
#'
#'  link <- random_link(groupLinkPairs = keys,
#'  addQueryParameter = TRUE)
#'
#'  shiny::actionButton(
#'    inputId = "randomId",
#'    label = "Click me!",
#'    onclick = paste0("window.location.replace('", link, "')"),
#'  )
#'
#' }
#'
random_link <- function(groupLinkPairs, addQueryParameter = TRUE) {

  .num_groups <- nrow(groupLinkPairs)
  .thresholds <- seq(0,1, by = 1/.num_groups)
  .thresholds <- .thresholds[which(.thresholds != 0 & .thresholds != 1)]
  .value <- stats::runif(1)

  group <- get_group(value = .value,
                     num_groups = .num_groups,
                     thresholds = .thresholds,
                     key_pair = groupLinkPairs)

  on_click_link <- groupLinkPairs[which(groupLinkPairs$group == group), "link"]

  if (addQueryParameter) {
    query <- ifelse(grepl("\\/$", on_click_link), "?group_id=", "/?group_id=")
    on_click_link <- paste0(on_click_link, query, group)
  }

  return(on_click_link)

}

get_group <- function(value, num_groups, thresholds, key_pair) {

  if (value <= thresholds[1]) {
    group <- key_pair[1,"group"]
  } else if (value > thresholds[num_groups - 1]) {
    group <- key_pair[num_groups, "group"]
  } else {
    for (i in 1:(num_groups - 2)) {
      if (value > thresholds[i] && value <= thresholds[i + 1]) {
        group <- key_pair[i+1, "group"]
      }
    }
  }

  return(group)

}

#' Action Button that Sends Users to Different Locations
#'
#' Randomization is common to behavioral data collection. This action button
#' allows you to supply a key-value pair of group and URL links for randomizing
#' participants in a {shiny} application.
#'
#' @param inputId Shiny input ID
#' @param label The button label
#' @param groupLinkPairs A data frame with two columns "group" and "link"
#'   containing the group and URL links for randomization. See examples for more
#'   details.
#' @param addQueryParameter LOGICAL: TRUE and a query parameter "group_id" will
#'   be added to the randomization URL. FALSE and it won't.
#' @param ... Additional parameters to pass into [shiny::actionButton()]
#'
#' @return The UI for an action button.
#' @export
#'
#' @examples
#'
#' keys <- data.frame(
#'   group = c("A", "B"),
#'   link = c("https://yourwebsite.com/this-task",
#'   "https://yourwebsite.com/this-other-task")
#' )
#'
#'
#' if (interactive()) {
#'
#'  library(shiny)
#'
#'  ui <- fluidPage(
#'    randomizeButton(inputId = "myId",
#'                    label = "Click me to randomize",
#'                    groupLinkPairs = keys,
#'                    addQueryParameter = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {
#'
#'  }
#'
#'  shinyApp(ui, server)
#'
#'
#' }
#'
#'
randomizeButton <- function(inputId, label, groupLinkPairs, addQueryParameter = TRUE, ...) {

  link <- random_link(groupLinkPairs = groupLinkPairs,
                            addQueryParameter = addQueryParameter)

  shiny::actionButton(
    inputId = inputId,
    label = label,
    onclick = paste0("window.location.replace('", link, "')"),
    ...
  )

}
