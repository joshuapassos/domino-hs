FROM debian:stable-20230919-slim

RUN apt-get update

RUN apt-get install -y g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase curl

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /app
COPY *.yaml /app

RUN stack build --only-dependencies

COPY . /app

RUN stack build

CMD stack run