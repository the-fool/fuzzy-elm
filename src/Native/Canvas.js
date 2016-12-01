var tmpScale = d3.scale.linear()
    .domain([-1, 0, 1])
    .range(["#f59322","#e8eaeb","#0877bd"])
    .clamp(true);

var colors = d3.range(-1, 1 + 1E-9, 1 / 30).map(a => {
  return tmpScale(a);
});

var color = d3.scale.quantize()
                 .domain([-1, 1])
                 .range(colors);


function drawCanvases(network) {
  // just draw final output for now
  network.forEach(function(neuron) {
    var canvas = document.getElementById(neuron.id);
    if (canvas) {
      var density = canvas.width;
      var data = neuron.outputs;
      var ctx = canvas.getContext('2d');
      var img = ctx.createImageData(density, density);
      for (let i = 0, p = -1; i < density*density; i++) {
          var c = d3.rgb(color(data[i]));
          img.data[++p] = c.r;
          img.data[++p] = c.g;
          img.data[++p] = c.b;
          img.data[++p] = 160;
      }
      ctx.putImageData(img, 0, 0);
    }
  });
}


var _user$project$Native_Canvas = {
  'drawCanvases': drawCanvases,
}
