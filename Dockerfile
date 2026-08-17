FROM rocker/geospatial:4.6.0
ARG RENV_PATHS_CACHE=/root/.cache/R/renv
ENV "RENV_PATHS_CACHE"="${RENV_PATHS_CACHE}"
RUN apt-get update -y && apt-get install -y  cmake make libuv1-dev pandoc libicu-dev libx11-dev libcurl4-openssl-dev libssl-dev zlib1g-dev libgdal-dev gdal-bin libgeos-dev libpng-dev libproj-dev libsqlite3-dev libudunits2-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libjpeg-dev libtiff-dev libwebp-dev git libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("renv")'
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=${RENV_PATHS_CACHE} R -e 'renv::restore()'
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/
EXPOSE 3838
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=3838)'
