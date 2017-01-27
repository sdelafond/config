#! /bin/sh

HOST="$1"
ISSUE="$2"
FIELD="${3:-summary}"

curl -L -k --netrc -s -n -X GET -H "Content-Type: application/json" ${HOST}/rest/api/latest/issue/${ISSUE} | jq -M -r ".fields | .$FIELD"
