# Dockerization

## Scripts in the folder:

 - `./docker/build.sh` - Builds an andorp/bead docker image necessary.
 - `./docker/remove-all-images.sh` - CAUTION: Removes all the docker images from your docker daemon.
 - `./docker/run.sh` - Downloads and/or starts the andorp/bead image. See below.

## Docker-based development cycle

### Regular development cycle

Follow those steps below the steps to quickly reach a state where you can develop bead.

 1. Check out the bead source from github to a path (e.g. /home/developer/bead-source)
    Let's call it *source*.
 1. Create a directory where the bead will place its data and database (e.g /home/developer/bead-server)
    Let's call it *server*.
 1. ./docker/run.sh /home/developer/bead-source /home/developer/bead-server
    It will download the image from the docker hub and attach the *source* to /development/bead and
    the *server* to /bead-server. For precise information check out the *Dockerfile*.
    The result of this step, you will get a *docker console* within the docker image.
 1. With your local machine and docker on it, you can start the development process.

### Changes in bead dependencies

In case of a change in dependencies, you have to install them into the docker image
via cabal. But it will be a local change. Please do not upload that change into the docker
hub repository. We rebuild the latest image if the cabal file changes in the repository.

## Building docker image

The docker image is created from Dockerfile placed in the root directory if the project,
from where the ./docker/build-docker.sh should be started. After the build the local
docker daemon should have an andorp/bead image.
