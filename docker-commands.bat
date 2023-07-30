@echo off
set /p "packageClient=Package client [y/n]? "
if /I "%packageClient%" EQU "y" goto packageClient
goto noPackageClient

:packageClient
echo Packaging client...
docker compose -f .\SWZ_Client-Server-Programm\docker-compose-clientPackage.yml -p swz_client-packager up --build
echo Client package created, manual copy from container required.

:noPackageClient
echo:
set /p "buildServerImg=Build server image [y/n]? "
if /I "%buildServerImg%" EQU "y" goto buildServerImg
goto noBuildServerImg

:buildServerImg
echo Building Server image...
docker build -t server-swz -f .\Dockerfile-Server .
echo Server image created, saving to tar file...
docker save server-swz -o server-swz.tar
echo Server image saved.
goto noLoadServerImg

:noBuildServerImg
echo:
set /p "loadServerImg=Load server image from tar file [y/n]? "
if /I "%loadServerImg%" EQU "y" goto loadServerImg
goto noLoadServerImg

:loadServerImg
echo Loading server image from tar file...
docker load -i server-swz.tar
echo Server image loaded.

:noLoadServerImg
echo:
set /p "runServer=Run server [y/n]? "
if /I "%runServer%" EQU "y" goto runServer
goto noRunServer

:runServer
echo Starting server in background...
docker compose -f .\docker-compose-server.yml up -d
echo Server running.

:noRunServer
echo:
echo Done.
echo Press any key to close . . .
pause > nul
