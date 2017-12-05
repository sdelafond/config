#! /usr/bin/python3

import collections, GeoIP, socket, sys

outputTpl = "%(host)s: %(city)s, %(region)s %(postal_code)s, %(country_name)s (%(country_code3)s) %(latitude).4f,%(longitude).4f"

host = sys.argv[1]

try:
  addr = socket.gethostbyname(host)
except:
  print("Couldn't resolve %s" % (host,))
  sys.exit(1)

h = GeoIP.new(GeoIP.GEOIP_MEMORY_CACHE).record_by_addr(addr)

if not h:
  print("couldn't locate host %s" % (host,))
  sys.exit(1)

h = collections.defaultdict(str, **h)
h['host'] = host

print(h)
print(outputTpl % h)
