FROM haskell:8

RUN stack build
ENV SHARECAR_HASKELL 80
RUN stack exec migrate
CMD stack exec sharecar
