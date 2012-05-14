import glob, json, os, sys

sessionStoreGlobPath = ".mozilla/firefox/*default/sessionstore.js"
homePath = os.getenv('HOME')
try:
  sessionStorePath = glob.glob(os.path.join(homePath, sessionStoreGlobPath))[0]
except:
  print >> sys.stderr, "Could not find your Firefox session store, aborting."
  sys.exit(1)

sessionData = json.load(open(sessionStorePath))

for win in sessionData.get("windows"):
  for tab in win.get("tabs"):
    i = tab.get("index") - 1
    print tab.get("entries")[i].get("url")

