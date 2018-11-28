#' Get instances of a model
#'
#' @param model A string that is a model name
#' @param limit Numeric. Default is 100
#' @param offset Numeric. Default is 0
#'
#' @return An object of class "response"
#' @export
#'
#' @examples
#' \dontrun{
#' resp <- river_data("model_name")
#' }
get_instances <- function(model, limit = 100, offset = 0) {
  base_url <- "https://api.tomorrowstodayslater.xyz/"
  request <- paste0(base_url, model, "?limit=", limit, "&offset=", offset)
  resp <- httr::GET(request)
  return(resp)
}
test <- get_instances("observation")
test_parsed <- httr::content(test, "parsed")

instance_attr <- function(resp) {
  data <- httr::content(resp, "parsed")$data
  attr <- data$model$attributes
  attr_df <- purrr::map_df(attr, ~ tibble::tibble(attr = .x$name,
                                       hasModel = .x$hasModel))
}
test_attr <- instance_attr(test)

#' Get values for all attributes from a model instance
#'
#' @param instance A river instance
#' @param attr An attribute string
#' @param hasModel Boolean
#'
#' @return A single row tibble
#' @export
attr_values <- function(instance, attr, hasModel) {
  # browser()
  if(hasModel) {
    if(is.null(instance$data[[attr]]$name)) {
      name <- ""
    } else name <- instance$data[[attr]]$name

    if(is.null(instance$data[[attr]]$uri)) {
      uri <- ""
    } else uri <- instance$data[[attr]]$uri

    out <- list(name, uri)

    names(out) <- c(attr, paste0(attr, "_uri"))
  } else {
    if(is.null(instance$data[[attr]])) {
      out <- ""
      } else out <- instance$data[[attr]]
      out <- list(out)
      names(out) <- attr
  }
  return(tibble::as_tibble(out))
}

#' Parse data for all attributes from all instances
#'
#' @param resp A response from `get_instances`
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' resp <- river_data("model_name")
#' data_output <- instance_data(resp)
#' }
instance_data <- function(resp) {
  data <- httr::content(resp, "parsed")$data
  attr <- instance_attr(resp)
  out <- purrr::map_df(data$instances,
                    function(x) {
                      out_list <- map2(attr$attr, attr$hasModel, ~ attr_values(x, .x, .y))
                      dplyr::bind_cols(out_list)
                    })
}
