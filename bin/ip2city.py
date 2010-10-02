#! /usr/bin/env python

import GeoIP, socket, sys

host = sys.argv[1]

try:
  addr = socket.gethostbyname(host)
except:
  print "Couldn't resolve %s" % (host,)
  sys.exit(1)

h = GeoIP.new(GeoIP.GEOIP_MEMORY_CACHE).record_by_addr(addr)

if not h:
  print "couldn't locate host %s" % (host,)
  sys.exit(1)

h['host'] = host

for k,v in h.iteritems():
  if not v:
    h[k] = ''

print ("%(host)s: %(city)s, %(region)s %(postal_code)s, %(country_name)s (%(country_code3)s, %(latitude)s, %(longitude)s " % h).decode('latin-1').encode('utf-8')
