#! /bin/bash

# we wish i3-msg could "set" a variable instead...

I3_CONFIG_TPL="${HOME}/.config/i3/config.tpl"
I3_CONFIG=${I3_CONFIG_TPL/.tpl}

perl -pe 's|I3_SCREEN_1|'$I3_SCREEN_1'| ; s|I3_SCREEN_2|'$I3_SCREEN_2'| ; s|I3_SCREEN_3|'$I3_SCREEN_3'|' $I3_CONFIG_TPL >| $I3_CONFIG

