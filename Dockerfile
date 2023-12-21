# Base app container with all dependencies installed
FROM ghcr.io/rocker-org/shiny-verse:4.3.0
ARG GITHUB_PAT
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
COPY .here /srv/.here
COPY code/app/install_libraries.R /srv/code/app/
RUN R -e 'source(here::here("code/app", "install_libraries.R"))'

COPY code/app/* /srv/code/app/

RUN mkdir -p /srv/tests/data
RUN R -e 'source(here::here("code/app", "setup.R"))'

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh
COPY code/index_org /srv/code/index_org
COPY code/index_com /srv/code/index_com

COPY code/app /srv/code/app/
RUN chown shiny:shiny /srv/code/app/www
COPY ./shiny-server-org.conf /etc/shiny-server/shiny-server-org.conf
COPY ./shiny-server-com.conf /etc/shiny-server/shiny-server-com.conf

CMD /usr/bin/shiny-server.sh

