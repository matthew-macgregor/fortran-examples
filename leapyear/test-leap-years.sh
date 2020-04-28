#!/bin/bash

input="$1"
while IFS= read -r line
do
  is_okay=1
  json=$(./leapyear "$line")
  year=$(echo $json | jq .Year)
  is_leap=$(echo $json | jq .IsLeapYear)
  days=$(echo $json | jq .Days)
  echo "Year $year, Leap $is_leap, Days $days"
  if [[ $is_leap -ne 'true' ]]; then
    is_okay=0
  fi

  if [[ $is_okay -eq 0 ]]; then
    echo "There was a problem with this leap year"
    exit 1
  fi
done < "$input"