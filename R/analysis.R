
#' Execute NCA Analysis in Aplos
#'
#' @description Submits and executes an NCA analysis on uploaded data.
#'
#' @param result Result from aplos_get_upload_url().
#' @param data_cleaning JSON string for data cleaning (default "{}").
#' @param analysis JSON string for PK parameter calculation (default "{}").
#' @param calcs JSON string for custom calculations (default "{}").
#' @param plots JSON string for custom plots (default "{}").
#' @param tables JSON string for custom tables (default "{}").
#' @param metadata JSON string for metadata (default "{}").
#' @param url The base API URL.
#' @param token JWT token.
#' @param name Analysis name (default "NCA Analysis via R").
#' @param save_body Logical; save body of API POST command to body.txt file? (default FALSE, set to TRUE for debugging).
#' @return The execution ID.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   exec_id <- aplos_execute_analysis(upload_result,
#'   url = "https://api.app.aplos-nca.com", token = token)
#' }
aplos_execute_analysis <- function(result, data_cleaning = "{}", analysis = "{}", calcs = "{}",
                                   plots = "{}", tables = "{}", metadata = "{}",
                                   url, token, name = "NCA Analysis via R",
                                   save_body = FALSE) {
  if (missing(result) || missing(url) || missing(token)) {
    stop("Required parameters (result, url, token) are missing.")
  }
  if (all(c(data_cleaning, analysis, calcs, plots, tables) == "{}")) {
    stop("At least one of data_cleaning, analysis, calcs, plots, or tables must be provided (non-default value).")
  }

  tenant_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_tenant_id`
  user_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_id`
  headers <- c("Content-Type" = "application/json",
               "Authorization" = paste0("Bearer ", token))
  file_id <- result$data$fileId
  body_string <- paste0(
    '{\n',
    '  "file_id": "', file_id, '",\n',
    '  "name": "', name, '",\n',
    '  "calculation": ', analysis, ',\n',
    '  "data_cleaning": ', data_cleaning, ',\n',
    '  "custom_calculations": ',calcs, ',\n',
    '  "custom_plots": ',plots, ',\n',
    '  "custom_tables": ',tables, ',\n',
    '  "metadata": ',metadata, '\n',
    '}'
  )
  if (save_body) { write(body_string, file = "body.txt") }

  response <- tryCatch({
    httr::POST(url = paste0(url, "/v3/tenants/", tenant_id, "/users/", user_id, "/analysis/queue"),
               httr::add_headers(.headers = headers),
               body = body_string, encode = "raw")
  }, error = function(e) stop("Analysis execution failed: ", e$message))

  if (httr::http_status(response)$category != "Success") {
    stop("Error initiating execution: ", httr::content(response, "text"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  execution_id <- result$data$executionId
  return(execution_id)
}
