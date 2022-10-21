#!/usr/bin/env bash

# Pass all AWS* and DDH* environment variables to the R environment for the shiny user
echo "Setting up /home/shiny/.Renviron based on current env"
env | grep '^AWS\|DDH' > /home/shiny/.Renviron

shiny-server 2>&1
