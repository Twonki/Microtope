# Pulser

This component sends simple messages to a given ActiveMQ. It is dockerized and after sending a message the container gets shut down.

Part of the Microtope Project.

## Run

`$> docker run -e ActiveMQ_Adress={yourAMQContainerIP} microtope/pulser:{currentVersion}`

Additional environment parameters:

- ActiveMQ_Adress: Takes either IPv4 or a domain name (Not a full URL!).
- ActiveMQ_Queue: Takes a different queue. default is "game_queue".
- ActiveMQ_Port: Takes a different port. default is 61616 (this is also the default amq-port, change with great care)

## To Build

With pure docker:

```bash
docker build . -f BuildDockerfile --build-arg PULSER_VERSION=x.x.x -t microtope/pulser
```

Manually:

- first, make `mvn clean package verify` and recognize the version number
- alter the Dockerfile to use the new version
- run `sudo docker build . -t microtope/pulser`

The `verify` in maven is very important! It checks for your coverage.

You maybe want to skip it and can even build your docker image with it - but the build pipeline wont be cheated :)

## Requirements

- Java 12
- Docker 18
- The ActiveMQ Dockerfile from the other Microtope Project (But any ActiveMQ should work)

## TroubleShooting

- It always takes localhost! - Check whether your -e comes before the image name in docker run. This took me only 3 hours.
- It doesn't find my amq! - Check whether your AMQ is running with `docker ps` and inspect the ip with `docker inspect`
