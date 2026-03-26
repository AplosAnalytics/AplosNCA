library(testthat)
library(httptest2)
library(jsonlite)  # For JSON handling
library(utils)  # For unzip mock
library(withr)  # For temp dir isolation
library(httr) # For GET mock

test_that("aplos_download_results retrieves info with mock", {
  # Mock for /outputs/package (redacted_package.json)
  mock_package_response <- structure(
    list(
      url = "https://api.example.com/v3/tenants/fake_tenant/users/fake_user/executions/fake_id/outputs/package",  # Valid character url
      status_code = 200L,
      content = charToRaw(toJSON(list(data = list(id = "fake_package_id")), auto_unbox = TRUE))
    ),
    class = "response"
  )

  # Mock for /download-url (redacted_download_url.json)
  mock_download_response <- structure(
    list(
      url = "https://api.example.com/v3/tenants/fake_tenant/users/fake_user/files/fake_package_id/download-url",  # Valid character url
      status_code = 200L,
      content = charToRaw(toJSON(list(data = list(downloadUrl = "fake_download_url", fileName = "fake.zip")), auto_unbox = TRUE))
    ),
    class = "response"
  )

  local_mocked_bindings(
    GET = function(url, ...) {
      if (grepl("/outputs/package", url)) {
        return(mock_package_response)
      } else if (grepl("/download-url", url)) {
        return(mock_download_response)
      }
    },
    .package = "httr"
  )

  df <- aplos_download_results(url = "https://api.example.com", token = "fake_token", execution_id = "fake_id")
  expect_s3_class(df, "data.frame")  # Expect data frame output
  expect_true(all(c("url", "filename") %in% names(df)))
  expect_equal(df$url[1], "fake_download_url")
  expect_equal(df$filename[1], "fake.zip")
})

test_that("aplos_fetch_results downloads and unzips with mock", {
  # Use data frame
  mock_info <- data.frame(
    url = "fake_download_url",
    filename = "fake.zip",
    stringsAsFactors = FALSE
  )

  # Run in temp dir for isolation
  with_tempdir({
    # Mock write_disk to simulate without creation
    local_mocked_bindings(
      write_disk = function(path, overwrite = TRUE) {
        list(path = path)  # Return path info without file I/O
      },
      .package = "httr"
    )

    # Mock GET to return successful response without network
    local_mocked_bindings(
      GET = function(url, config, ...) {
        # Simulate by "writing" to a fake path
        fake_path <- file.path(getwd(), "results", "fake.zip")
        structure(list(status_code = 200L, url = url, path = fake_path), class = "response")
      },
      .package = "httr"
    )

    # Mock status_code
    local_mocked_bindings(
      status_code = function(x) 200L,
      .package = "httr"
    )

    # Mock file.exists to return TRUE for the check
    local_mocked_bindings(
      file.exists = function(...) TRUE,
      .package = "base"
    )

    # Mock dir.create to return TRUE
    local_mocked_bindings(
      dir.create = function(path, ...) TRUE,
      .package = "base"
    )

    # Mock unzip to return dir path
    local_mocked_bindings(
      unzip = function(zipfile, exdir = ".", ...) {
        exdir
      },
      .package = "utils"
    )

    file_path <- aplos_fetch_results(mock_info, dest_dir = "results", unzip = TRUE)
    expect_type(file_path, "character")
    expect_match(file_path, "results/unzip")  # Match returned dir
  })
})

test_that("aplos_download_results errors on missing params", {
  expect_error(aplos_download_results(url = "api.com", token = "token"), "All parameters.*required")
})
