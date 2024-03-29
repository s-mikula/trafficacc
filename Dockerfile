FROM rocker/r-ver:4.2.2
# upgrade Ubuntu packages
RUN apt update
RUN apt upgrade -y

# useful tools
RUN apt install -y apt-utils apt-file iproute2 iputils-ping libc-bin # screen vim

# install necessary packages
# snippet from /rocker_scripts/install_geospatial.sh with Ubuntu prerequisites only
RUN apt install -y \
gdal-bin \
lbzip2 \
libfftw3-dev \
libgdal-dev \
libgeos-dev \
libgsl0-dev \
libgl1-mesa-dev \
libglu1-mesa-dev \
libhdf4-alt-dev \
libhdf5-dev \
libjq-dev \
libpq-dev \
libproj-dev \
libprotobuf-dev \
libnetcdf-dev \
libsqlite3-dev \
libssl-dev \
libudunits2-dev \
lsb-release \
netcdf-bin \
postgis \
protobuf-compiler \
sqlite3 \
tk-dev \
unixodbc-dev

#web server
RUN /bin/sh -c /rocker_scripts/install_shiny_server.sh

WORKDIR /srv/shiny-server
RUN rm -rf *

# install renv
ENV RENV_VERSION=0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# copy renv files
COPY renv.lock renv.lock
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf

# restore R packages
RUN R -e "renv::restore()"
#this is mandatory to make renv work inside shiny - https://community.rstudio.com/t/shiny-server-renv/71879/5
RUN R -e "renv::isolate()"

# useful tools
# COPY Docker/.vimrc Docker/.screenrc /root/
# COPY Docker/findgrep /usr/local/bin/

#copy the application itself at the very end
COPY trafficacc/ .

#Exposing port informs Docker which port the container is listening on at runtime
EXPOSE 3838
#The CMD command tells Docker how to run the application we packaged in the image.
#CMD [“command”, “argument1”, “argument2”].
CMD ["/usr/bin/shiny-server"]
