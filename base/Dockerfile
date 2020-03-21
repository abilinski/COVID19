FROM ubuntu:18.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt update -y && apt install gnupg -y && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    apt-get install software-properties-common -y && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
RUN apt-get install r-base libssl-dev libxml2-dev libcurl4-openssl-dev -y

RUN apt-get install liblapack-dev liblapack3 libopenblas-base libopenblas-dev -y

RUN apt-get install xorg libx11-dev libglu1-mesa-dev  -y


COPY init.R /init.R
RUN Rscript /init.R
