#! /usr/bin/python

# dirty preprocessor handling:
#   - %ENV(var) for resolving environment variables
#   - %IF(...)something for conditional statements

import os, os.path, re, sys

HOME = os.getenv('HOME')

if len(sys.argv) > 1:
  I3_CONFIG = sys.argv[1]
else:
  I3_CONFIG = os.path.join(HOME, '.config/i3/config')
I3_CONFIG_TPL = I3_CONFIG + '.tpl'

RO = re.compile(r'%(ENV|IF)\((.*)\)')

def process(line):
  m = RO.search(line)
  if m:
    cmd, token = m.groups()
    if cmd == 'ENV':
      return RO.sub(os.getenv(token, ""), line)
    elif cmd == 'IF':
      a, op, b = token.split(' ')
      s = "'%s' %s '%s'" % (process(a), op, process(b))
      if eval(s):
        return RO.sub("", line)
      else:
        return "#%s" % line
  else:
    return line

with open(I3_CONFIG, 'w') as conf:
  with open(I3_CONFIG_TPL) as tpl:
    for line in tpl.readlines():
      l = process(line)
      conf.write(l)
