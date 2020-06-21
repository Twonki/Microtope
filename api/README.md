# Microtope - API

This repository contains the files for the microtope api.

It reads from the database and returns json objects in a RESTful-way.

## How to Build and run

### With docker

```Shell
$> docker build . -t microtope/api:{anyversion}
$> docker run -p 8080:8080 microtope/api:{anyversion}
```

### Without docker

`$> npm run start`

### Verify

You should now be able to access your api in a browser with localhost:8080 and see a hello world.

If you have a database connected (and running) you can visit localhost:8080/api/healthcheck and see a status - otherwise you will log the according error.

## Build with

- Docker
- Node
- [Wait-for-it](https://github.com/vishnubob/wait-for-it)
