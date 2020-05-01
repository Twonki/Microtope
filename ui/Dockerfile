##### Stage 1
# Taken from: https://github.com/coding-latte/using-angular-docker-environment-variables/blob/master/dockerfile

FROM node:14.1.0 as builder

WORKDIR /app

# Copy project files to the docker image
COPY . .

RUN npm ci

# Build Angular Application in Production
RUN npm run build:prod

#### STAGE 2
#### Deploying the application

FROM nginx:alpine

VOLUME  /var/cache/nginx

# Copy the build files from the project
# replace "angular-docker-environment-variables" with your angular project name
COPY --from=builder /app/dist/ui /usr/share/nginx/html

# Copy Nginx Files
COPY --from=builder /app/.docker/.config/nginx.conf /etc/nginx/conf.d/default.conf

# EXPOSE Port 80
EXPOSE 80
