FROM haskell:8

RUN apt-get update 

RUN apt-get install unixodbc unixodbc-dev unixodbc-bin -y 

RUN cabal update

WORKDIR /App

COPY . .

RUN cabal new-install 

# TODO: cabal test

# TODO: Add parameters as required
ENTRYPOINT ["Wesir"]