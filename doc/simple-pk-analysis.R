## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-package-------------------------------------------------------------
library(AplosNCA)  # Load the package

## ----load-creds---------------------------------------------------------------
# For vignette demonstration (offline), use fake credentials.
# In real use, source your .aplos_creds file.
creds <- list(
  COGNITO_CLIENT_KEY = "fake_key",
  COGNITO_USERNAME = "fake_user",
  COGNITO_PASSWORD = "fake_pass",
  COGNITO_REGION = "us-east-1",
  APLOS_API_URL = "https://api.app.aplos-nca.com"
)

## ----authenticate-------------------------------------------------------------
# Simulated for vignette - in real use, call aplos_get_jwt(creds$COGNITO_CLIENT_KEY, ...)
token <- "fake_token"
url <- creds$APLOS_API_URL
print("Authenticated (simulated)")


## ----upload-data--------------------------------------------------------------
# Define your input file
input_file <- system.file("extdata", "ex1-data.csv", package = "AplosNCA")

# Get upload URL
# Simulated - in real use, call aplos_get_upload_url("ex1-data.csv", url, token)
upload_result <- list(data = list(fileId = "fake_file_id"))
print("Upload URL retrieved (simulated)")

# Upload file
# Simulated - in real use, call aplos_upload_file("ex1-data.csv", upload_result, token)
print("File uploaded (simulated)")

## ----load-configs-------------------------------------------------------------
analysis_config_file <- system.file("extdata", "ex1-analysis.json", package = "AplosNCA")
analysis <- readChar(analysis_config_file, file.info(analysis_config_file)$size)


## ----execute-analysis---------------------------------------------------------
# Simulated - in real use, call aplos_execute_analysis(upload_result, analysis = analysis,url = api_url, 
#                                 token = token, name = "Simple PK Analysis Vignette")
exec_id <- "fake_exec_id"
print("Analysis executed (simulated)")
                                  

## ----check-status-------------------------------------------------------------
# Simulated polling - assume "succeeded" after delay - in real use, call exec_result <- aplos_execution_status(url = api_url, token = token, execution_id = exec_id, sleep=10)

exec_result <- list(data = list(status = "succeeded"))
print("Execution complete (simulated)")


## ----download-results---------------------------------------------------------
if (exec_result$data$status == "succeeded") {
  # Simulated - in real use, call download_info <- aplos_download_results(url = api_url, token = token, execution_id = exec_id)
  # file_path <- aplos_fetch_results(download_info, dest_dir = "simple", unzip = TRUE)
} else {
  cat("Analysis failed; check status.\n")
}

## ----session-info-------------------------------------------------------------
sessionInfo()

