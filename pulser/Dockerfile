FROM azul/zulu-openjdk-alpine:15.0.0

# 0.2.0
ARG PULSER_VERSION

ENV dir /usr/app
ENV ActiveMQ_Adress 127.0.0.1
ENV ActiveMQ_Port 61616
ENV ActiveMQ_Queue game_queue
ENV ActiveMQ_User Artemis
ENV ActiveMQ_Pwd  Artemis

RUN apk update && apk add bash

RUN mkdir -p $dir

COPY ./target/pulser-$PULSER_VERSION-jar-with-dependencies.jar $dir

COPY ./utils/wait-for-it.sh $dir

WORKDIR $dir

ENTRYPOINT ["/bin/bash", "-c", "./wait-for-it.sh -t 0 -s --host=$ActiveMQ_Adress --port=$ActiveMQ_Port -- java -jar pulser-$PULSER_VERSION-jar-with-dependencies.jar $ActiveMQ_Adress $ActiveMQ_Port $ActiveMQ_Queue $ActiveMQ_User $ActiveMQ_Pwd "]