#!/bin/sh
path=$(basename $(pwd) | awk '{print tolower($0)}')
echo $path;
if [ $# -eq 1 ]; then
    if [ $1 = "install" ]; then
        echo "Running Composer"
        docker run -it --rm -v $(pwd):/opt/project -v /tmp/virtualssh:/root/.ssh miamioh/php:7.3-phpstorm composer install
    fi
fi
echo "Starting up Docker"
docker-compose up -d
echo "Starting up Bash Console"
if [ $# -eq 1 ]; then
    if [ $1 = "composer" ] || [ $1 = "compose" ]; then
        docker run -it --rm -v $(pwd):/opt/project -v /tmp/virtualssh:/root/.ssh miamioh/php:7.3-phpstorm bash
    fi
else
    docker exec -it "${path}_php-fpm_1" bash
fi


