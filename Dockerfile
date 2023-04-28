FROM ghcr.io/rocker-org/shiny-verse:4.3.0
EXPOSE 3838

RUN apt-get update && apt-get install -y libglpk-dev libmagick++-dev libavfilter-dev libpoppler-cpp-dev python3-dev python3-venv libjpeg-dev tcl8.6-dev && rm -rf /var/lib/apt/lists/*

# Install and register "Roboto Slab" and "Nunito Sans" fonts
RUN mkdir /usr/share/fonts/googlefonts \
    && wget --output-document=/tmp/RobotoSlab.zip https://fonts.google.com/download?family=Roboto%20Slab \
    && unzip -o -d /usr/share/fonts/googlefonts /tmp/RobotoSlab.zip \
    && wget --output-document=/tmp/NunitoSans.zip https://fonts.google.com/download?family=Nunito%20Sans \
    && unzip -o -d /usr/share/fonts/googlefonts /tmp/NunitoSans.zip \
    && fc-cache -fv

RUN R -e 'devtools::install_github("matthewhirschey/ddh")'

WORKDIR /srv
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY .here /srv/.here
COPY code /srv/code

RUN mkdir -p /srv/tests/data
RUN R -e 'source(here::here("code", "setup.R"))'

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh

CMD /usr/bin/shiny-server.sh

