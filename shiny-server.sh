#!/usr/bin/env bash


CONFIG=/etc/shiny-server/shiny-server-org.conf
if [ "$DDH_PRIVATE" == "T" ]
then
   CONFIG=/etc/shiny-server/shiny-server-com.conf
fi

# Pass all AWS* and DDH* environment variables to the R environment for the shiny user
echo "Setting up /home/shiny/.Renviron based on current env"
env | grep '^AWS\|DDH' > /home/shiny/.Renviron

shiny-server $CONFIG 2>&1
