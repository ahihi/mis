var MAP_CENTER = [60.1699, 24.9384];
var MAP_ZOOM = 13.5;

window.onload = function() {
    var map = L.map("map")
	.setView(MAP_CENTER, MAP_ZOOM);

    L.tileLayer(
	'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
	{
	    attribution: 'Map data © <a href="http://openstreetmap.org">OpenStreetMap</a> contributors'
	}
    ).addTo(map);

    var loc = window.location;
    var ws_protocol = loc.protocol === "https:" ? "wss:" : "ws:";
    var ws_url = ws_protocol + "//" + loc.host + "/ws";

    var circle = undefined;
    var ws = new WebSocket(ws_url);

    ws.onmessage = function(ev) {
	var msg = JSON.parse(ev.data);
	console.log(msg);

	if(circle === undefined) {
	    circle = L.circle([msg.latitude, msg.longitude], {
		color: "#FF0000",
		fillColor: "#FF0033",
		fillOpacity: 0.5,
		radius: msg.accuracy
	    }).addTo(map);
	} else {
	    circle.setRadius(msg.accuracy);
	    circle.slideTo([msg.latitude, msg.longitude], {
		duration: 500
	    });
	}
    };
};
