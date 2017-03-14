FROM haskell:8

WORKDIR /root
RUN git clone https://github.com/genya0407/sharecar.git
WORKDIR /root/sharecar
RUN stack build
ENV SHARECAR_HASKELL 80
RUN stack exec migrate
CMD stack exec sharecar
