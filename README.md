# mis

An HTTP server for tracking the location of a single device. Designed for use with [GPS Logger for Android](https://play.google.com/store/apps/details?id=com.mendhak.gpslogger).

![Screenshot](https://static.foldplop.com/misc/mis1.png)

## Build & run

Using [Stack](https://www.haskellstack.org):

````
$ stack build
$ stack exec mis-exe
````

The server will listen on localhost:8081. Put Nginx or something in front of it to expose it to the outside world.

## Writing GPS data

In GPS Logger for Android, set your custom URL to:

````
https://<your domain>/writeGps?latitude=%LAT&longitude=%LON&direction=%DIR&accuracy=%ACC&speed=%SPD&time=%TIME
````

TODO: Require a secret key for writing

## Reading GPS data

Plain old GET:
````
https://<your domain>/gps
````

Websockets:
````
wss://<your domain>/ws
````
