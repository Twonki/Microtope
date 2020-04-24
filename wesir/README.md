## Wesir

This part of the Microtope repository holds a microservice to check the database validity.

The example state thats invalid (and introduced in the database already) is that there is a login without logout, that is two logins in a row.

## Build and Install

Install ODBC first with

```shell
sudo apt-get install unixodbc unixodbc-dev unixodbc-bin
```

To manually build on your machine, run in this repository:

```shell
cabal new-install
```

To build the docker run:

```shell
docker build . -t wesir
```

## Requirements

To build the docker image, only docker is required.

To run manually, one needs

- GHC >8.10
- Cabal > 3
- LinuxODBC (see above)
