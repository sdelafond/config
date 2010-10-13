#! /bin/zsh

text="$1"
title="$2"
timeout="${3:-2}"

if [[ $title == *attery* ]] ; then
  beep.sh 2
  percentage=$(acpi | perl -pe 's/.+?(\d+)%.+/$1/')
  extra=", fg=\"black\""
  if [[ "$percentage" -lt 5 ]] ; then
    extra="$extra, bg=\"red\""
    timeout="8"
  else
    extra="$extra, bg=\"yellow\""
    timeout="5"
  fi
  text="${percentage}%"
fi

echo 'naughty.notify({'title=\"$title\" , text=\"$text\" , screen=mouse.screen , timeout=$timeout $extra '})' | awesome-client -
