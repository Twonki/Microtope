FROM azul/zulu-openjdk-alpine:14.0.1

ENV dir /usr/app
ENV ActiveMQ_Adress 127.0.0.1
ENV ActiveMQ_Port 61616
ENV ActiveMQ_Queue game_queue
ENV ActiveMQ_User Artemis
ENV ActiveMQ_Pwd  Artemis

ENV MariaDB_Adress 127.0.0.1
ENV MariaDB_Port 3306
ENV MariaDB_DatabaseName microtope
ENV MariaDB_User worker
ENV MariaDB_PW INeed2Write

RUN apk update && apk add bash

RUN mkdir -p $dir

COPY ./target/worker-0.2.1-jar-with-dependencies.jar $dir

COPY ./utils/wait-for-it.sh $dir

WORKDIR $dir

ENTRYPOINT ["/bin/bash", "-c", "./wait-for-it.sh -t 0 -s --host=$ActiveMQ_Adress --port=$ActiveMQ_Port -- ./wait-for-it.sh -t 0 -s --host=$MariaDB_Adress --port=$MariaDB_Port -- java -jar worker-0.2.1-jar-with-dependencies.jar $ActiveMQ_Adress $ActiveMQ_Port $ActiveMQ_Queue $ActiveMQ_User $ActiveMQ_Pwd $MariaDB_Adress $MariaDB_Port $MariaDB_DatabaseName $MariaDB_User $MariaDB_PW"]