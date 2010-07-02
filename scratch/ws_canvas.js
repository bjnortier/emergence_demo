function debug(str) {
  $("#console").prepend(str);
  $("#console").prepend('<br>');
}


function initws() {

    var ws = new WebSocket("ws://localhost:1234/websession");

    ws.onopen = function() {
	debug("connected.");
    };
    
    txText = function(data) {
	ws.send(data);
    };
    
    ws.onmessage = function (evt) {
	debug("received: " + evt.data); 
	var data = JSON.parse(evt.data);

    };

    ws.onclose = function() {
	debug("socket closed");
	setTimeout(initws, 1000);
    }
    
}


var ctx = document.getElementsByTagName('canvas')[0].getContext('2d');

var lastX = ctx.canvas.width * Math.random();
var lastY = ctx.canvas.height * Math.random();
var hue = 0;
function line() {
    ctx.save();
    ctx.translate(ctx.canvas.width/4, ctx.canvas.height/4);
    ctx.scale(0.5, 0.5);
    ctx.translate(-ctx.canvas.width/2, -ctx.canvas.height/2);
    ctx.beginPath();
    ctx.lineWidth = 5 + Math.random() * 10;
    ctx.moveTo(lastX, lastY);
    lastX = ctx.canvas.width * Math.random();
    lastY = ctx.canvas.height * Math.random();
    ctx.bezierCurveTo(ctx.canvas.width * Math.random(),
                          ctx.canvas.height * Math.random(),
                          ctx.canvas.width * Math.random(),
                          ctx.canvas.height * Math.random(),
                          lastX, lastY);

    hue = hue + 10 * Math.random();
    ctx.strokeStyle = 'hsl(' + hue + ', 50%, 50%)';
    ctx.shadowColor = 'white';
    ctx.shadowBlur = 10;
    ctx.stroke();
    ctx.restore();
}

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
	if (timeStamp - population[i].created > 100000) {
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


	var x = ctx.canvas.width - (timeStamp - population[i].created)/50;
	var y = ctx.canvas.height*(1-population[i].fitness);
	ctx.arc(x,y,10,0,Math.PI*2, true); 
	ctx.stroke();
	ctx.fill();
    }
    ctx.restore();
   
}



function add() {
    var fitness = Math.random();
    var organism = {fitness: fitness,
		    created: new Date().getTime()
		   };
    population.push(organism);
}

$(document).ready(function() {
    initws();
    setInterval(draw, 100);
    add();
    setInterval(add, 100);
  });
