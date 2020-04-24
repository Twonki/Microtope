FROM haskell:8

RUN cabal update

WORKDIR /App

COPY . .

RUN cabal new-install 

# TODO: cabal test

# TODO: Add parameters as required
ENTRYPOINT ["Wesir"]