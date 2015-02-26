#!/bin/bash

status_line ()
{
    date +"%H:%M:%S %d/%m/%y"
}

while true
do
    echo "$status_line"
    sleep 1
done
