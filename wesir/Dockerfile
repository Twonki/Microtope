FROM haskell:8

RUN apt-get update
RUN apt-get install apt-utils wget -q -y 
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections



# Drivers for ODBC
RUN apt-get install unixodbc unixodbc-dev unixodbc-bin wget -y
# Potential fix to current issue, as dependencies
RUN apt-get install checkinstall cmake build-essential libssl1.1 -y 

# Installing Specific MariaDB ODBC
# Taken from https://mariadb.com/kb/en/about-mariadb-connector-odbc/
WORKDIR  /odbc_package
RUN wget https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.7/mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
RUN install /odbc_package/lib/libmaodbc.so /lib/


RUN cabal update

WORKDIR /App

COPY . .

RUN odbcinst -i -d -f Resources/MariaDB_odbc_driver_template.ini

RUN cabal new-install 

# TODO: cabal test

# TODO: Add parameters as required
ENTRYPOINT ["Wesir"]