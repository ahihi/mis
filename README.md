# mis

An HTTP server for tracking the location of a single device. Designed for use with [GPS Logger for Android](https://play.google.com/store/apps/details?id=com.mendhak.gpslogger).

![Screenshot](https://static.foldplop.com/misc/mis1.png)

## Build & run

Using [Stack](https://www.haskellstack.org):

````
$ stack build
$ MIS_PORT=… MIS_WRITE_KEY=… stack exec mis-exe
````

`MIS_PORT`: port to listen on  
`MIS_WRITE_KEY`: key required to write GPS data

The server will listen on localhost. Put Nginx or something in front of it to expose it to the outside world.

## Writing GPS data

In GPS Logger for Android, set your custom URL to:

````
https://<your domain>/writeGps?latitude=%LAT&longitude=%LON&direction=%DIR&accuracy=%ACC&speed=%SPD&time=%TIME&key=<your write key>
````

## Reading GPS data

Plain old GET:
````
https://<your domain>/gps
````

Websockets:
````
wss://<your domain>/ws
````
