
var paper = Raphael("notepad", "100%", 550);
var population = [];
var fitnesses = [];

var lastTimeStamp = new Date().getTime();

// Using TaskMgr
Ext.TaskMgr.start({
    run: function() {
	if (population.length == 0) {
	    return;
	}

	var spliceIndex = 0;
	for (var i = 0; i < population.length; ++i) {
	    if (population[i].attrs.cx < 0) {
		population[i].remove();
		spliceIndex = i + 1;
	    }
	}

	if (spliceIndex > 0) {
	    population = population.slice(spliceIndex);
	    fitnesses = fitnesses.slice(spliceIndex);
	}
	
	var newTimeStamp = new Date().getTime();
	var dx = (newTimeStamp - lastTimeStamp)/1000*20;
	lastTimeStamp = newTimeStamp;

	for (var i = 0; i < population.length; ++i) {
	    c = population[i];
	    var norm_fitness = normalise_fitness(fitnesses[i]);
	    var target_cy = cy_for_norm_fitness(norm_fitness);
	    var target_fill = fill_for_norm_fitness(norm_fitness);
	    c.attr({cx: c.attrs.cx - dx});
	    var targetAttrs = {};
	    if (c.attrs.cy != target_cy) {
		targetAttrs.cy = target_cy;
	    }
	    if (c.attrs.fill != target_fill) {
		targetAttrs.fill = target_fill;
	    }
	    if (targetAttrs.cy || targetAttrs.fill) {
		c.animate(targetAttrs, 30);
	    }
	}
    },
	interval: 1
    });

function normalise_fitness(fitness) {
    if (fitnesses.length == 0) {
	return 0.5;
    }
    var max = Math.max.apply(Math, fitnesses);
    var min = Math.min.apply(Math, fitnesses);
    if (min == max) {
	return 0.5;
    }
    return (fitness - min) / (max - min);
}

function cy_for_norm_fitness(norm_fitness) {
    return 450 - norm_fitness * 350;
}

function fill_for_norm_fitness(norm_fitness) {
    return 'hsb(' + (norm_fitness*0.3) + ', 0.9, .9)';
}

var maxFitness = 10;


		  
Ext.TaskMgr.start({
    run: function(){

	var fitness = Math.floor(Math.random()*maxFitness);
	var norm_fitness = normalise_fitness(fitness);
	maxFitness = maxFitness + 1;

	var c = paper.circle(600, cy_for_norm_fitness(norm_fitness), 0);
	c.attr({"fill": fill_for_norm_fitness(norm_fitness), 
		"stroke-width": 2, 
		"stroke": "#fff", 
		"fill-opacity": 100});
	c.animate({r: 10}, 500);

	var task = new Ext.util.DelayedTask(function(){
	    fitnesses.push(fitness);
	    population.push(c);
	});
	task.delay(1000);


    },
    interval: 1000
});