#! /usr/bin/python3

# dirty preprocessor handling:
#   - %ENV(var) for resolving environment variables
#   - %SYS(cmd) for running commands
#   - %IF(...)something for conditional statements

import os, os.path, re, subprocess, sys

HOME = os.getenv('HOME')

if len(sys.argv) > 1:
  I3_CONFIG = sys.argv[1]
else:
  I3_CONFIG = os.path.join(HOME, '.config/i3/config')
I3_CONFIG_TPL = I3_CONFIG + '.tpl'

MACRO_RO = re.compile(r'%(ENV|IF|SYS)\((.*)\)')
IF_RO = re.compile(r' (==|!=) ')

def process(line):
  m = MACRO_RO.search(line)
  if m:
    macro, token = m.groups()
    if macro == 'ENV':
      value = os.getenv(token, "")
      return MACRO_RO.sub(value, line)
    if macro == 'SYS':
      cmd = token.split(' ')
      value = subprocess.check_output(cmd).decode().strip()
      return MACRO_RO.sub(value, line)
    elif macro == 'IF':
      a, op, b = IF_RO.split(token)
      s = "'%s' %s '%s'" % (process(a), op, process(b))
      if eval(s):
        return MACRO_RO.sub("", line)
      else:
        return ""
  else:
    return line

with open(I3_CONFIG, 'w') as conf:
  with open(I3_CONFIG_TPL) as tpl:
    for line in tpl.readlines():
      l = process(line)
      conf.write(l)
