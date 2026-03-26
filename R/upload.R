
#' Get Presigned Upload URL for Aplos NCA
#'
#' @description Retrieves a presigned URL for uploading files to Aplos NCA.
#'
#' @param input_file Path to the input file.
#' @param url The base API URL.
#' @param token JWT token from aplos_get_jwt().
#' @return A list with upload details.
#' @export
#' @importFrom stringr str_split_i
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   result <- aplos_get_upload_url("path/to/file.csv", "https://api.app.aplos-nca.com", token)
#' }
aplos_get_upload_url <- function(input_file, url, token) {
  if (missing(input_file) || missing(url) || missing(token)) {
    stop("All parameters (input_file, url, token) are required.")
  }

  tenant_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_tenant_id`
  user_id <- AzureAuth::decode_jwt(token)$payload$`custom:aplos_user_id`
  filename <- stringr::str_split_i(input_file, "/", -1)
  upload_url <- paste0(url, "/v3/tenants/", tenant_id, "/users/", user_id, "/files")
  headers <- c("Content-Type" = "application/json",
               "Authorization" = paste0("Bearer ", token))
  body <- list(
    "file_name" = filename,
    method_type = "post",
    category = "nca_input"
  )

  response <- tryCatch({
    httr::POST(url = upload_url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE))
  }, error = function(e) stop("Upload URL request failed: ", e$message))

  if (httr::http_error(response)) {
    stop("Failed to get upload URL: ", httr::content(response, "text"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(result)
}

#' Upload File to Aplos NCA
#'
#' @description Uploads a file using the presigned URL.
#'
#' @param input_file Path to the input file.
#' @param result Result from aplos_get_upload_url().
#' @param token JWT token.
#' @export
#' @examples
#' \dontrun{
#'   aplos_upload_file("path/to/file.csv", upload_result, token)
#' }
aplos_upload_file <- function(input_file, result, token) {
  if (missing(input_file) || missing(result) || missing(token)) {
    stop("All parameters (input_file, result, token) are required.")
  }

  upload_url <- result$data$url
  body <- list(
    key = result$data$fields$key,
    "x-amz-algorithm" = result$data$fields$xAmzAlgorithm,
    "x-amz-credential" = result$data$fields$xAmzCredential,
    "x-amz-date" = result$data$fields$xAmzDate,
    "x-amz-security-token" = result$data$fields$xAmzSecurityToken,
    policy = result$data$fields$policy,
    "x-amz-signature" = result$data$fields$xAmzSignature,
    file = httr::upload_file(normalizePath(input_file))
  )

  response <- tryCatch({
    httr::POST(url = upload_url, body = body)
  }, error = function(e) stop("File upload failed: ", e$message))

  if (httr::http_error(response)) {
    stop("File upload error: ", httr::content(response, "text"))
  }
}
