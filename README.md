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

SSO accounts cannot authenticate with a username and password. Instead, copy
the token from the Aplos NCA web interface and save it on one line in a hidden
file in your home directory:

```bash
chmod 600 ~/.token
```

```r
token <- aplos_import_jwt()          # reads ~/.token
# or: token <- aplos_import_jwt("~/path/to/token")

result <- aplos_get_upload_url("data.csv",
                               "https://api.app.aplos-nca.com", token)
```

When an imported token is within 20 minutes of expiring, the package re-reads
the file before each API call, so a token refreshed by another tool is picked
up automatically. Set the `APLOS_JWT_FILE` environment variable to use a
different default path.

## Contributing
Pull requests are welcome. For major changes, please open an issue first.

## Issues
Report bugs at https://github.com/AplosAnalytics/AplosNCA/issues.
