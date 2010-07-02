function debug(str) {
  $("#console").prepend(str);
  $("#console").prepend('<br>');
}

// connectionState try_connect [0]|disconnected [1]|connected [2]
var connectionState = -1;

var connectingStage = 0;

function updateState(newState) {
    if (connectionState != newState) {
	connectionState = newState;
	updateWifi();
    }

}

function updateWifi() {
    if (connectionState == 0) {
	$("#wifi").attr("src", "../img/wifi_" + connectingStage + ".png");
	connectingStage = (connectingStage + 1) % 4;
	setTimeout(updateWifi, 500);
    } else if (connectionState == 1) {
	$("#wifi").attr("src", "../img/wifi_none.png");
    } else if (connectionState == 2) {
	$("#wifi").attr("src", "../img/wifi_full.png");
    }
}

function initws() {

    var ws = new WebSocket("ws://localhost:1234/websession");

    ws.onopen = function() {
	updateState(2);
    };
    
    txText = function(data) {
	ws.send(data);
    };

    close = function() {
	updateState(1);
	ws.close();
    };
    
    ws.onmessage = function (evt) {
	//debug("received: " + evt.data); 
	var data = JSON.parse(evt.data);
	
	var fitness = data.fitness;
	var organism = {fitness: fitness,
			created: new Date().getTime()
		       };
	population.push(organism);

    };

    ws.onclose = function() {
	// Set to try-connect if not manually disconnected
	if (connectionState != 1) {
	    updateState(0);
	    setTimeout(initws, 500);
	} 
    }
    
}


var ctx = document.getElementsByTagName('canvas')[0].getContext('2d');

var population = [];

function fillFromFitness(norm_fitness) {
    //return 'hsl(17, 50%, 50%)';
    var hue = Math.ceil(norm_fitness*100);
    return 'hsl(' + hue + ', 80%, 50%)';
}

function draw() {
    // Interesting way to "blank" the canvas. Don't blank
    // it completely, but ue the opacity to make it fade out
    // Example at 
    // http://www.whatwg.org/specs/web-apps/current-work/multipage/the-canvas-element.html
    ctx.fillStyle = '#333';
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    
    ctx.save();
    ctx.scale(0.8, 0.8);
    ctx.translate(50, 50);
    var timeStamp = new Date().getTime();
    var splice = -1;
    for (var i = 0; i < population.length; ++i) {
	if (timeStamp - population[i].created > 2000) {
	    splice = i;
	}
    }
    if (splice > -1) {
	population = population.slice(splice + 1);
    }

    for (var i = 0; i < population.length; ++i) {
	
	ctx.beginPath(); 

	ctx.strokeStyle = '#white';
	ctx.fillStyle = fillFromFitness(population[i].fitness);
	ctx.lineWidth = 5;
	ctx.shadowColor = 'white';
	ctx.shadowBlur = 10;


	var x = ctx.canvas.width - (timeStamp - population[i].created)/2;
	var y = ctx.canvas.height*(1-population[i].fitness);
	ctx.arc(x,y,10,0,Math.PI*2, true); 
	ctx.stroke();
	ctx.fill();
    }
    ctx.restore();
   
}


updateState(0);
$(document).ready(function() {
    initws();
    setInterval(draw, 100);
  });

$("#wifi").click(function() {
    if (connectionState == 1) {
	updateState(0);
	initws();
    } else {
	close();
    } 


});
