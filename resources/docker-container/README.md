# Docker Container for the scala-plotalyzer

This container provides the working environment for the Plotalyzer in a Docker container.

## Steps

### Build Container

```bash
sudo --preserve-env=SSH_AUTH_SOCK DOCKER_BUILDKIT=1 docker build --ssh default -t [env_name] . || docker image prune -a
```

And delete directly if an error occurs.

### Start

Start Container:
```bash
sudo docker run -it --rm \
    # Path to the folder in which we want to have the output.
    # Otherwise we create the output, but delete it after leaving the folder
    -v /path/to/analysis/output/folder/:/path/to/analysis/output/folder/ \
    -v /path/to/trackhar.mjs:/path/to/trackhar.mjs \
    -e DATABASE_URL="[user]://[host_gateway_docker_container]:[db_port]/[db_name]" \
    [env_name]:latest
```

### Useful Information

Here I am writing down some information that could be helpful for you.

#### Testing

The commands used during testing:
```bash
sudo --preserve-env=SSH_AUTH_SOCK DOCKER_BUILDKIT=1 docker build --ssh default -t plotalyzer . || docker image prune -a

sudo docker run -it --rm     
-v /home/ias/Desktop/scala-plotalyzer:/workspace/plotalyzer    
-v /home/ias/Desktop/PlotalyzerOutput/:/PlotalyzerOutput     
-v /home/ias/Desktop/trackHAR/:/home/ias/Desktop/trackHAR/  
-e DATABASE_URL="postgresql://172.17.0.1:5433/appanalyzer_extern"     
plotalyzer:latest
```

#### Database

Plotalyzer Trackhar:
In the plugin you have to adjust the trackhar.mjs path!
Otherwise you get the following error:
```bash
Exception in thread "main" spray.json.JsonParser$ParsingException: Unexpected end-of-input at input index 0 (line 1, position 1), expected JSON Value:
```

I also noted for me: sbt publishLocal.

#### Database

In file postgresql.conf listen for all addresses: listen_addresses = '*'.
In file pg_hba.conf add entry: host all all 172.17.0.0/16 trust.
In the container you can't use localhost. Instead you must use the full ip adress (e.g., 172.17.0.1:5433).

#### Docker

Stop container:
```bash
docker stop [container]
```

See all containers:
```bash
docker ps -a
```

Delete container:
```bash
sudo docker rm name
```

Delete all failed builds:
```bash
docker image prune -a
```

See docker images:
```bash
docker images
```

Delete docker images:
```bash
docker rmi [env]
```
