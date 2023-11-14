FROM shinytiger:base
COPY shinytigeR_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz", upgrade = "never")'
RUN rm /app.tar.gz
RUN addgroup --gid 1003 beitner; adduser --shell /bin/false --no-create-home --uid 1002 --gecos "Julia Beitner,,," --gid 1003 --disabled-password beitner
VOLUME [ "/opt/shinyapp" ]
WORKDIR /opt/shinyapp
EXPOSE 3838
USER beitner
CMD R -e 'options(shiny.port = 3838, shiny.host = "0.0.0.0")'
CMD R -e 'library(shinytigeR)'
CMD R -e 'shinytigeR::run_app()'
