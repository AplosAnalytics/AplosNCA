# AplosNCA
[![R-CMD-check](https://github.com/AplosAnalytics/AplosNCA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AplosAnalytics/AplosNCA/actions/workflows/R-CMD-check.yaml)

R package to use Aplos NCA API calls. This package includes functions that do the following tasks:
* Authenticate with your Aplos NCA account to get a JWT
* Request a URL to upload a new dataset
* Upload a dataset
* Initiate an analysis (data cleaning, PK parameter calculation, custom calculations, custom tables, custom plots)
* Check status of an analysis submission
* Download results

## Authentication

Every API call needs an Aplos NCA JWT. A JWT expires one hour after it is
issued; the package renews it automatically with an Aplos NCA Refresh Token
when less than 20 minutes remain, so you authenticate once and keep working.

There are two ways to obtain the pair.

### 1. Username and password

Supply the Cognito client ID, your username, password, and AWS region:

```r
token <- aplos_get_jwt(client_id = "your_client_id",
                       username = "your_username",
                       password = "your_password",
                       region = "us-east-1")
```

Cognito returns an Aplos NCA JWT and an Aplos NCA Refresh Token at the same
time. The package keeps both and uses the refresh token to renew the JWT for
the rest of the R session. `return_refresh = TRUE` returns both if you want to
store the refresh token yourself.

### 2. Aplos NCA Refresh Token

If you sign in through single sign-on (SSO), or you already hold a refresh
token, copy the Aplos NCA Refresh Token from your profile page (API Settings)
in the Aplos NCA web application. Save it with the client ID and region:

```
REFRESH_TOKEN="your_refresh_token"
COGNITO_CLIENT_KEY="your_cognito_client_id"
COGNITO_REGION="us-east-1"
```

```r
token <- aplos_import_jwt("~/.token")
```

The package exchanges it for an Aplos NCA JWT and renews that JWT the same way.
The refresh token can be reused in later sessions until it expires or is
revoked, so one file is enough.

### The API URL

The URL (for example `https://api.app.aplos-nca.com`) is not used to obtain
either token — Cognito is addressed with the client ID and AWS region. The URL
identifies the Aplos NCA API and is passed to the analysis functions:

```r
result <- aplos_get_upload_url("data.csv", "https://api.app.aplos-nca.com", token)
```

### Lifetimes

- **Aplos NCA JWT**: one hour. Renewed automatically when 20 minutes remain.
- **Aplos NCA Refresh Token**: set on the Cognito app client, 30 days by
  default. Using it does not extend it. When it expires or is revoked, renewal
  fails and you import a new one.

## Contributing
Pull requests are welcome. For major changes, please open an issue first.

## Issues
Report bugs at https://github.com/AplosAnalytics/AplosNCA/issues.
