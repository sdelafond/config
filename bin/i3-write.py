#! /usr/bin/python3

import jinja2
import os
import os.path
import platform
import sys

HOME = os.getenv('HOME')

if len(sys.argv) > 1:
    I3_CONFIG = sys.argv[1]
else:
    I3_CONFIG = os.path.join(HOME, '.config/i3/config')
I3_CONFIG_TPL = I3_CONFIG + '.j2'

if __name__ == '__main__':
    with open(I3_CONFIG_TPL) as tpl:
        template = jinja2.Template(tpl.read(), trim_blocks=True, lstrip_blocks=True)
        env = os.environ
        env['HOSTNAME'] = platform.node().split('.', 1)[0]
        result = template.render({'env': env})
        with open(I3_CONFIG, 'w') as conf:
            conf.write(result)
