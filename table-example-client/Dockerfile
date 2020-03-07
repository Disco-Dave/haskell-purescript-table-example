# docker build -t table-example-client .
# docker run --name table-example-client -d -p 8080:80 table-example-client
# firefox http:/localhost:8080
# docker stop table-example-client
# docker rm table-example-client
 
 
# Build purescript application
#----------------------------------------------------------------------------------------------------
FROM node:latest AS build
COPY . .
ENV API_URL http://localhost:8081
RUN npm install && npm run build


# Serve application via nginx
#----------------------------------------------------------------------------------------------------
FROM nginx:alpine
EXPOSE 80
COPY --from=build /dist /usr/share/nginx/html/
