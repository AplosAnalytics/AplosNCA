# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(AplosNCA)
library(httptest2)
library(AzureAuth)  # For mocking decode_jwt
library(jsonlite)
library(downloader)
library(utils)
library(withr)

# Global mock for decode_jwt to avoid parse errors in check environment
local_mocked_bindings(
  decode_jwt = function(...) list(payload = list(`custom:aplos_user_tenant_id` = "fake_tenant", `custom:aplos_user_id` = "fake_user")),
  .package = "AzureAuth"
)

test_check("AplosNCA")
