#!/bin/bash

set -e
set -o pipefail

heroku container:login
heroku container:push web --app covid19-app-staging
heroku container:release web --app covid19-app-staging
sleep 5
heroku logs --app covid19-app-staging
