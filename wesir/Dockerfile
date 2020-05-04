FROM haskell:8

RUN apt-get update

# Drivers for ODBC, wget for mariaodbc download later
RUN apt-get wget install unixodbc unixodbc-dev unixodbc-bin wget -y

# Installing Specific MariaDB ODBC
# Taken from https://mariadb.com/kb/en/about-mariadb-connector-odbc/
WORKDIR  /odbc_package
RUN wget https://downloads.mariadb.com/Connectors/odbc/connector-odbc-2.0.19/mariadb-connector-odbc-2.0.19-ga-debian-x86_64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-2.0.19-ga-debian-x86_64.tar.gz
RUN install /odbc_package/lib/libmaodbc.so /lib/


RUN cabal update

WORKDIR /App

COPY . .

RUN odbcinst -i -d -f Resources/MariaDB_odbc_driver_template.ini

RUN cabal new-install 

# TODO: cabal test

# TODO: Add parameters as required
ENTRYPOINT ["Wesir"]