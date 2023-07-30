#!/bin/bash

read -r -p "Package client [y/n]? " packageClient
if [[ "$packageClient" =~ ^[Yy]$ ]]; then
  echo "Packaging client..."
  docker-compose -f ./SWZ_Client-Server-Programm/docker-compose-clientPackage.yml -p swz_client-packager up --build
  echo "Client package created, manual copy from container required."
fi

read -r -p "Build server image [y/n]? " buildServerImg
if [[ "$buildServerImg" =~ ^[Yy]$ ]]; then
  echo "Building Server image..."
  docker build -t server-swz -f ./Dockerfile-Server .
  echo "Server image created, saving to tar file..."
  docker save server-swz -o server-swz.tar
  echo "Server image saved."
else
  read -r -p "Load server image from tar file [y/n]? " loadServerImg
  if [[ "$loadServerImg" =~ ^[Yy]$ ]]; then
    echo "Loading server image from tar file..."
    docker load -i server-swz.tar
    echo "Server image loaded."
  fi
fi

read -r -p "Run server [y/n]? " runServer
if [[ "$runServer" =~ ^[Yy]$ ]]; then
  echo "Starting server in background..."
  docker-compose -f ./docker-compose-server.yml up -d
  echo "Server running."
fi

echo
echo "Done."
read -n 1 -s -r -p "Press any key to close..."
