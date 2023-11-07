FROM rocker/r-ver:4.3.1 AS builder
MAINTAINER Julia Beitner <beitner@psych.uni-frankfurt.de>
RUN apt-get update -qq && \ 
  apt-get install -y --no-install-recommends \
    libicu-dev \
    libsodium-dev \
    make \
    pandoc \
    zlib1g-dev && \
  apt-get clean && \ 
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc /usr/lib/R/etc/
COPY renv.lock renv.lock
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
RUN R -e 'renv::restore()'
RUN rm renv.lock
FROM rocker/r-ver:4.3.1 
COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY shinytigeR_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz", upgrade = "never")'
RUN rm /app.tar.gz
RUN addgroup --gid 1003 beitner; adduser --shell /bin/false --no-create-home --uid 1002 --gecos "Julia Beitner,,," --gid 1003 --disabled-password beitner
VOLUME [ "/opt/shinyapp" ]
WORKDIR /opt/shinyapp
EXPOSE 3838
USER beitner
CMD R -e 'options(shiny.port = 3838, shiny.host = "0.0.0.0")'
CMD R -e 'shinytigeR::run_app()'
