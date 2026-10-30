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

Aplos NCA API calls require a JWT. Tokens expire one hour after they are issued.
The package checks the token before every API call and refreshes it
automatically when less than 20 minutes remain (using the Cognito refresh token
from login), so you authenticate once per session in normal use.

### Username and password

```r
token <- aplos_get_jwt(client_id = "your_client_id",
                       username = "your_username",
                       password = "your_password",
                       region = "us-east-1")
```

Store credentials in a local file such as `~/.aplos_creds`; do not hardcode
them in scripts. See `?aplos_get_jwt` for the file format.

### Single sign-on (SSO)

SSO accounts cannot authenticate with a username and password. You only need the
**refresh token** from the Aplos NCA web interface, plus the Cognito client ID
and region shown in API Settings. The package exchanges the refresh token for a
JWT and renews it automatically.

Save the three values in a hidden file:

```bash
chmod 600 ~/.token
```

```
REFRESH_TOKEN="your_refresh_token"
COGNITO_CLIENT_KEY="your_cognito_client_id"
COGNITO_REGION="us-east-1"
```

```r
token <- aplos_import_jwt()          # reads ~/.token, returns a JWT
# or: token <- aplos_import_jwt("~/path/to/token")

result <- aplos_get_upload_url("data.csv",
                               "https://api.app.aplos-nca.com", token)
```

A file with only the refresh token also works when `client_id` and `region` are
passed as arguments, or set as `APLOS_COGNITO_CLIENT_ID` /
`APLOS_COGNITO_REGION`. A file containing a bare JWT is still accepted for
backwards compatibility, but such a token cannot be renewed: it works until it
expires, then the package stops and asks for a refresh token.

Set `APLOS_JWT_FILE` to use a different default path.

## Contributing
Pull requests are welcome. For major changes, please open an issue first.

## Issues
Report bugs at https://github.com/AplosAnalytics/AplosNCA/issues.
