
#' Obtain Aplos NCA API JWT Token
#'
#' @description
#' Authenticates with the Aplos NCA API using AWS Cognito. This function sends a POST request to retrieve a JWT token for subsequent API calls. All API calls require a valid JWT. The JWT is only valid for a period of time (~15 minutes). Each time you make an API call with a valid JWT, that token is refreshed and the valid period is extended. Thus during a normal analysis you will only need to authenticate once as you will be making API calls within the valid time window. However, if you walk away from your computer or there is a long delay between API calls you may need to authenticate again.
#'
#' Note: Store credentials securely; do not hardcode them in scripts. It is recommended that you use a hidden text file (e.g. ".aplos_creds") stored on your local computer. Read that file in and then parse it to update the required information for authentication. An example credential file may look like the following:
#' APLOS_API_URL="https://api.app.aplos-nca.com"
#' COGNITO_CLIENT_KEY="your_cognito_client_id/key"
#' COGNITO_USER_NAME="your_username"
#' COGNITO_PASSWORD="your_password"
#' COGNITO_REGION="your_aws_region"
#' All of this information can be found in your user profile for your Aplos NCA account under "API Settings".
#'
#' @param client_id The Cognito client ID.
#' @param username Your Aplos NCA username.
#' @param password Your Aplos NCA password.
#' @param region The AWS region (e.g., "us-east-1").
#' @return A character string containing the JWT token.
#' @export
#' @importFrom AzureAuth decode_jwt
#' @examples
#' \dontrun{
#'   token <- aplos_get_jwt(client_id = "your_client_id", username = "user",
#'                          password = "pass", region = "us-east-1")
#' }
aplos_get_jwt <- function(client_id, username, password, region) {
  if (missing(client_id) || missing(username) || missing(password) || missing(region)) {
    stop("All parameters (client_id, username, password, region) are required.")
  }

  headers <- c("Content-Type" = "application/x-amz-json-1.1",
               "X-Amz-Target" = "AWSCognitoIdentityProviderService.InitiateAuth")
  body <- list(
    AuthFlow = "USER_PASSWORD_AUTH",
    AuthParameters = list("USERNAME" = username, "PASSWORD" = password),
    ClientId = client_id,
    method_type = "post"
  )

  response <- tryCatch({
    httr::POST(url = paste0("https://cognito-idp.", region, ".amazonaws.com/"),
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE))
  }, error = function(e) stop("JWT request failed: ", e$message))

  if (httr::http_error(response)) {
    stop("JWT authentication failed: ", httr::content(response, "text"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  jwt <- result$AuthenticationResult$IdToken
  return(jwt)
}
