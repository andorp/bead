FROM ubuntu:14.04

# Set locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Download GHC and cabal
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update && \
    apt-get install -y cabal-install-1.18 ghc-7.8.3 cpphs
ENV PATH /opt/ghc/7.8.3/bin:/opt/cabal/1.18/bin:$PATH

# Install necessary GHC tools
RUN apt-get install -y happy-1.19.4 alex-3.1.3
ENV PATH /opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:$PATH

# Create development dirs
RUN mkdir /development && \
    mkdir /development/cabal-file && \
    mkdir /development/bead && \
    mkdir /bead-server

# Copy source code, compile and finally remove it
COPY "./Bead.cabal" "/development/cabal-file/Bead.cabal"
RUN apt-get install -y zlib1g-dev libncurses5-dev && \
    cd development/cabal-file && \
    cabal update && \
    cabal install -j4 --only-dependencies

# Directory for sources
VOLUME "/development/bead"

# Directory for running
VOLUME "/bead-server"

# Expose the default port
EXPOSE 8000
