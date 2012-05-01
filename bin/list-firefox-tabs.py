import glob, json, os

f = glob.glob(os.path.join(os.getenv('HOME'), ".mozilla/firefox/*default/sessionstore.js"))[0]
jdata = json.load(open(f))

for win in jdata.get("windows"):
  for tab in win.get("tabs"):
    i = tab.get("index") - 1
    print tab.get("entries")[i].get("url")

