FROM haskell-ubuntu-stack:ghc-8.10.7

USER root
COPY . /home/chris/tmp
RUN chown chris:chris -R /home/chris/tmp

USER chris

WORKDIR /home/chris/tmp
RUN stack build --fast --test --no-run-tests

USER root

RUN rm -rf /home/chris/tmp

USER chris
