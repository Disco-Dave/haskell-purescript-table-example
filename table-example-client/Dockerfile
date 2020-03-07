# docker build -t table-example-client .
# docker run --name table-example-client -d -p 8080:80 table-example-client
# firefox http:/localhost:8080
# docker stop table-example-client
# docker rm table-example-client
 
 
# Build purescript application
#----------------------------------------------------------------------------------------------------
FROM node:latest AS build

ENV API_URL http://localhost:8081

RUN mkdir /opt/build
WORKDIR /opt/build

# Install node dependencies
COPY ./package.json ./package-lock.json ./
RUN npm install

# Install purescript dependencies
COPY ./spago.dhall ./packages.dhall ./
RUN npm run build:deps

COPY . .
RUN npm run build


# Serve application via nginx
#----------------------------------------------------------------------------------------------------
FROM nginx:alpine AS app
EXPOSE 80
COPY --from=build /opt/build/dist /usr/share/nginx/html/
