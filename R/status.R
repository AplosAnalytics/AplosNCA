
#' Check Execution Status in Aplos NCA
#'
#' @description Polls the status of an NCA analysis execution until complete or failed.
#'
#' @param url The base API URL.
#' @param token JWT token.
#' @param execution_id The execution ID from aplos_execute_analysis().
#' @param sleep Numeric value for time (in seconds) between status checks (default 10).
#' @return The status result if succeeded, or stops with error if failed.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   status <- aplos_execution_status("https://api.app.aplos-nca.com", token, "exec_123")
#' }
aplos_execution_status <- function(url, token, execution_id, sleep=10) {
  if (missing(url) || missing(token) || missing(execution_id)) {
    stop("All parameters (url, token, execution_id) are required.")
  }

  tenant_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_tenant_id`
  user_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_id`
  headers <- c("Content-Type" = "application/json",
               "Authorization" = paste0("Bearer ", token))
  complete <- FALSE
  while (!complete) {
    response <- tryCatch({
      httr::GET(paste0(url, "/v3/tenants/", tenant_id, "/users/", user_id, "/executions/", execution_id, "/status"),
                httr::add_headers(.headers = headers))
    }, error = function(e) warning("Status check failed: ", e$message))

    result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    if (result$data$status == "failed") { break }
    complete <- result$data$status == "succeeded"
    if (!complete) {
      message(paste0("Not yet complete ... ", result$data$status, " \n"))
      Sys.sleep(sleep)
    }
  }

  if (result$data$status == "succeeded") {
    message("Execution complete. \n")
    return(result)
  } else {
    warning(paste0("Execution failed. Execution ID = ", execution_id, "\n"))
  }
}
