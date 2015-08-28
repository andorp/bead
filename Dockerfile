FROM ubuntu:14.04

# Set locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

# Download GHC and cabal
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update && \
    apt-get install -y cabal-install-1.18 ghc-7.8.3 cpphs && \
    apt-get install -y libpcre3 libpcre3-dev && \
    apt-get install -y libmysqlclient-dev && \
    apt-get install -y mysql-server && \
    service mysql start && \
    mysqladmin -u root password password
ENV PATH /opt/ghc/7.8.3/bin:/opt/cabal/1.18/bin:$PATH

# Install necessary GHC tools
RUN apt-get install -y happy-1.19.4 alex-3.1.3
ENV PATH /opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:$PATH

# Create development dirs
RUN mkdir /development && \
    mkdir /development/init && \
    mkdir /development/bead && \
    mkdir /bead-server

# Copy cabal file and install dependencies
COPY "./Bead.cabal" "/development/init/Bead.cabal"
COPY "./docker/container-script/dev-env-setup.sh" "/development/init/dev-env-setup.sh"
RUN apt-get install -y zlib1g-dev libncurses5-dev && \
    cd development/init && \
    cabal update && \
    cabal install -j2 --only-dependencies

# Directory for sources
VOLUME "/development/bead"

# Directory for running
VOLUME "/bead-server"

# Expose the default port
EXPOSE 8000
