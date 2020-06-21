# Worker

This component takes simple messages from a given ActiveMQ and writes them into an database.
It is long running and sleeps for n-ms before it looks for new messages.

In terms of design, it uses the data structures from the [core-package](../core) and in general follows a [hexagonal architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)).
This provides a high grade of testability and potential other sources to load data.

## To Build

With docker: `docker build -f BuildDockerfile . -t microtope/worker`

Manually:

- first, make `mvn clean package verify` and recognize the version number
- alter the Dockerfile to use the new version
- run `sudo docker build . -t microtope/worker`

The `verify` in maven is very important! It checks for your coverage.

You maybe want to skip it and can even build your docker image with it - but the build pipeline wont be cheated :)

## Requirements

- Docker 18
- The ActiveMQ Dockerfile from the other Microtope Project (But any ActiveMQ should work), see in [meta](../meta)

## TroubleShooting

- It always takes localhost! - Check whether your -e comes before the image name in docker run. This took me only 3 hours.
- It doesn't find my amq! - Check whether your AMQ is running with `docker ps` and inspect the ip with `docker inspect`
- It doesn't find the jar! - Check the Version numbers in the docker files and the Pom. 