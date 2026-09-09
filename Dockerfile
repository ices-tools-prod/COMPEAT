FROM rocker/r-ver:4.6.1

ARG RENV_PATHS_CACHE=/root/.cache/R/renv
ENV RENV_PATHS_CACHE="${RENV_PATHS_CACHE}"

# Install system dependencies
RUN apt-get update -y && apt-get install -y \
    cmake make libuv1-dev pandoc libicu-dev libx11-dev \
    libcurl4-openssl-dev libssl-dev zlib1g-dev libgdal-dev \
    gdal-bin libgeos-dev libpng-dev libproj-dev libsqlite3-dev \
    libudunits2-dev libfontconfig1-dev libfreetype6-dev \
    libfribidi-dev libharfbuzz-dev libjpeg-dev libtiff-dev \
    libwebp-dev libxml2-dev libabsl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up global R profile options
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ && \
    echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# Install package manager tools
RUN R -e 'install.packages("remotes")' && \
    R -e 'remotes::install_version("renv", version = "1.2.4")'

# MOVE WORKDIR HERE: Sets context for both package restoration and app execution
WORKDIR /srv/shiny-server/

# Restore R packages using Docker layer caching
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=${RENV_PATHS_CACHE} R -e 'renv::restore()'

# Copy the rest of the application files
COPY . /srv/shiny-server/

# Create a non-root user/group named shiny, then change file ownership
RUN groupadd -r shiny && \
    useradd -r -g shiny -m -d /home/shiny shiny && \
    chown -R shiny:shiny /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
