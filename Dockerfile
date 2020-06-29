FROM jenkins/jenkins
USER root
RUN apt-get update
RUN apt-get install -y gnat-gps build-essential software-properties-common xmlstarlet
RUN add-apt-repository "deb http://archive.ubuntu.com/ubuntu/ bionic universe"
RUN add-apt-repository "deb http://archive.ubuntu.com/ubuntu/ bionic main"
RUN apt-get update
RUN apt-get install -y --allow-unauthenticated gcc-7-base gcc-7 libgnat-7 libgnatvsn7 libisl19 libmpfr6 gnat-7 libaunit17.2017 libaunit17.2017-dev
