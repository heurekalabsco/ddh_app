FROM rocker/shiny-verse:4.2.0
EXPOSE 3838

RUN apt-get update && apt-get install -y libglpk-dev libmagick++-dev libavfilter-dev libpoppler-cpp-dev python3-dev python3.8-venv libjpeg-dev tcl8.6-dev && rm -rf /var/lib/apt/lists/*

# Install and register "Roboto Slab" and "Nunito Sans" fonts
RUN mkdir /usr/share/fonts/googlefonts \
    && wget --output-document=/tmp/RobotoSlab.zip https://fonts.google.com/download?family=Roboto%20Slab \
    && unzip -d /usr/share/fonts/googlefonts /tmp/RobotoSlab.zip \
    && wget --output-document=/tmp/NunitoSans.zip https://fonts.google.com/download?family=Nunito%20Sans \
    && unzip -d /usr/share/fonts/googlefonts /tmp/NunitoSans.zip \
    && fc-cache -fv

RUN R -e 'devtools::install_github("matthewhirschey/ddh", ref="82afb268691498cf3baaa3b87a45a5d71bb780b6")'

WORKDIR /srv
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY .here /srv/.here
COPY code /srv/code

RUN mkdir -p /srv/tests/data
RUN R -e 'source(here::here("code", "setup.R"))'

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh

CMD /usr/bin/shiny-server.sh

