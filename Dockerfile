FROM rocker/shiny:4.4.0

# Install system dependencies for bioinformatics packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libgmp3-dev \
    libmpfr-dev \
    libcairo2-dev \
    libxt-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy and run requirements script
COPY requirement.R /tmp/
RUN Rscript /tmp/requirement.R

# Copy your application
COPY canis_lupus.R /srv/shiny-server/
COPY canis_lupus.png /srv/shiny-server/

# Expose port
EXPOSE 3838

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Run application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/canis_lupus.R', host = '0.0.0.0', port = 3838)"]