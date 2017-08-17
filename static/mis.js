var MAP_CENTER = [60.1699, 24.9384];
var MAP_ZOOM = 13.5;
var SLIDE_DURATION = 500;

window.onload = function() {
    var map_div = document.querySelector("#map");
    
    function set_map(on) {
	if(on) {
	    map_div.style.display = "block";
	    map.invalidateSize();
	} else {
	    map_div.style.display = "none";
	}
    };

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

    function open_websocket() {
	var ws = new WebSocket(ws_url);

	ws.onopen = function() {
	    set_map(true);
	};

	ws.onclose = function() {
	    set_map(false);
	    setTimeout(open_websocket, 2000);
	}
	
	ws.onmessage = function(ev) {
	    var msg = JSON.parse(ev.data);

	    if(circle === undefined) {
		circle = L.circle([msg.latitude, msg.longitude], {
		    color: "#FF0000",
		    fillColor: "#FF0000",
		    fillOpacity: 0.5,
		    radius: msg.accuracy
		}).addTo(map);
	    } else {
		circle.setRadius(msg.accuracy);
		circle.slideTo([msg.latitude, msg.longitude], {
		    duration: SLIDE_DURATION
		});
	    }
	};
    }

    set_map(false);
    
    open_websocket();

    if("geolocation" in navigator) {
	var user_circle = undefined;
	
	var geo_watch_id = navigator.geolocation.watchPosition(
	    function(pos) {
		console.log(pos);
		if(user_circle === undefined) {
		    user_circle = L.circle([pos.coords.latitude, pos.coords.longitude], {
			color: "#0000FF",
			fillColor: "#0000FF",
			fillOpacity: 0.5,
			radius: pos.coords.accuracy
		    }).addTo(map);
		} else {
		    user_circle.setRadius(pos.coords.accuracy);
		    user_circle.slideTo([pos.coords.latitude, pos.coords.longitude], {
			duration: SLIDE_DURATION
		    })
		}
	    },
	    function(err) {
		alert(err.message);
	    },
	    {
		enableHighAccuracy: true,
		timeout: 5000
	    }
	);
    }
};
