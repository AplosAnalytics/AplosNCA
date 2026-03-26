
#' Download Results Information from Aplos NCA Analysis
#'
#' @description Retrieves download URLs and files from Aplos NCA analysis results.
#'
#' @param url The base API URL.
#' @param token JWT token.
#' @param execution_id The execution ID.
#' @return A data frame with download URL and filename.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   df <- aplos_download_results("https://api.app.aplos-nca.com", token, "exec_123")
#' }
aplos_download_results <- function(url, token, execution_id) {
  if (missing(url) || missing(token) || missing(execution_id)) {
    stop("All parameters (url, token, execution_id) are required.")
  }

  tenant_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_tenant_id`
  user_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_id`
  headers <- c("Content-Type" = "application/json",
               "Authorization" = paste0("Bearer ", token))

  # Get output file ID
  response <- tryCatch({
    httr::GET(paste0(url, "/v3/tenants/", tenant_id, "/users/", user_id, "/executions/", execution_id, "/outputs/package"),
              httr::add_headers(.headers = headers))
  }, error = function(e) stop("Output package request failed: ", e$message))

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  output_id <- result$data$id

  # Get download URL
  response <- tryCatch({
    httr::GET(paste0(url, "/v3/tenants/", tenant_id, "/users/", user_id, "/files/", output_id, "/download-url"),
              httr::add_headers(.headers = headers))
  }, error = function(e) stop("Download URL request failed: ", e$message))

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  df <- data.frame(url = result$data$downloadUrl, filename = result$data$fileName)
  return(df)
}

#' Fetch and Optionally Unzip Aplos NCA Results
#'
#' @description Downloads the results file from the provided download information and optionally unzips it.
#'
#' @param download_info A data frame from aplos_download_results() containing url and filename.
#' @param dest_dir Directory to save the downloaded file (default: current working directory).
#' @param unzip Logical; if TRUE, unzip the file to dest_dir/unzip (default: FALSE).
#' @return The path to the downloaded file (or unzip directory if unzip=TRUE).
#' @export
#' @examples
#' \dontrun{
#'   # Assuming status is "succeeded"
#'   download_info <- aplos_download_results(url, token, execution_id)
#'   file_path <- aplos_fetch_results(download_info, unzip = TRUE)
#' }
aplos_fetch_results <- function(download_info, dest_dir = "results", unzip = FALSE) {
  if (!is.data.frame(download_info) || !all(c("url", "filename") %in% names(download_info))) {
    stop("download_info must be a data frame with 'url' and 'filename' columns.")
  }

  download_url <- download_info$url[1]  # Assuming single row; extend if multiple
  output_file <- file.path(dest_dir, download_info$filename[1])

  # Ensure dest_dir exists
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Download the file
  # Download the file using httr for reliability
  response <- httr::GET(download_url, httr::write_disk(output_file, overwrite = TRUE))
  if (httr::status_code(response) != 200) {
    stop("Download failed with status code: ", httr::status_code(response))
  }
  cat("Results file downloaded to", output_file, "\n")

  # Check if the file was downloaded successfully
  if (!file.exists(output_file)) {
    stop("Downloaded file not found at ", output_file)
  }

  if (unzip) {
    unzip_dir <- file.path(dest_dir, "unzip")
    if (!dir.exists(unzip_dir)) {
      dir.create(unzip_dir, recursive = TRUE)
    }
    unzip(output_file, exdir = unzip_dir)
    cat("Results file unzipped.\n")
    cat(paste0("Location is ", unzip_dir, "\n"))
    return(unzip_dir)
  } else {
    return(output_file)
  }
}

