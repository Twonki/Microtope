FROM node:15.4.0-alpine

# Adding bash, as wait-for-it needs bash
RUN apk add --no-cache bash
# Adding curl for healthcheck
RUN apk add --no-cache curl

ENV dir /usr/api

ENV MariaDB_Adress 127.0.0.1
ENV MariaDB_Port 3306
ENV MariaDB_DatabaseName microtope
ENV MariaDB_User api
ENV MariaDB_PW Need2Read

ENV Exposed_Port 8080

USER root
RUN mkdir -p $dir
WORKDIR $dir

COPY package.json $dir
COPY package-lock.json $dir
RUN npm ci

COPY . $dir

RUN npm run build

COPY ./utils/wait-for-it.sh $dir

HEALTHCHECK --interval=3m --timeout=5s --start-period=5s CMD ["bash","-c","curl -f http://localhost:3000/api/healthcheck || exit 1"]

ENTRYPOINT [ "/bin/bash", "-c","./wait-for-it.sh -t 0 -s --host=$MariaDB_Adress --port=$MariaDB_Port -- node . ${MariaDB_Adress} ${MariaDB_User} ${MariaDB_PW} ${MariaDB_Port} ${MariaDB_DatabaseName} ${Exposed_Port}"]