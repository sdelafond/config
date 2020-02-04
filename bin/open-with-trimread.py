#!/usr/bin/python3

import requests
import re
import sys

# constants
URL = 'https://beta.trimread.com'

## main
payload = {'url': sys.argv[1]}

resp = requests.post(URL, data=payload)

if not resp.status_code == 200:
    sys.exit(1)

link = re.search(fr'({URL}/articles/\d+)', resp.text).group(1)
print(link)
