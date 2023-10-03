FROM ddh-app-base
COPY code/app/* /srv/code/app/

RUN mkdir -p /srv/tests/data
RUN R -e 'source(here::here("code/app", "setup.R"))'

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh
COPY code/index_org /srv/code/index_org
COPY code/index_com /srv/code/index_com

COPY code/app /srv/code/app/
COPY ./shiny-server-org.conf /etc/shiny-server/shiny-server-org.conf
COPY ./shiny-server-com.conf /etc/shiny-server/shiny-server-com.conf

CMD /usr/bin/shiny-server.sh

