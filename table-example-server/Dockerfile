# Based off of: https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b

# docker build --target dependencies --cache-from table-example-server-deps -t table-example-server-deps .
# docker build --target app --cache-from table-example-server-deps -t table-example-server .
# docker run --name table-example-server -p 8081:8081 table-example-server
# docker stop table-example-server
# docker rm table-example-server


# Build application's dependencies
#----------------------------------------------------------------------------------------------------
FROM fpco/stack-build-small:latest AS dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only


# Build our application
#----------------------------------------------------------------------------------------------------
FROM fpco/stack-build-small:latest AS build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc --copy-bins --local-bin-path ./dist


# Run our application
#----------------------------------------------------------------------------------------------------
FROM ubuntu:latest AS app

RUN mkdir -p /opt/app
WORKDIR /opt/app

# Set the locale
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

COPY --from=build /opt/build/dist ./
COPY --from=build /opt/build/raw_data ./raw_data

CMD ./table-example-server-exe
