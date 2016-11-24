// Get a range of colors.
const tmpScale = d3.scale.linear()
    .domain([0, .5, 1])
    .range(["#0877bd", "#e8eaeb", "#f59322"])
    .clamp(true);
// Due to numerical error, we need to specify
// d3.range(0, end + small_epsilon, step)
// in order to guarantee that we will have end/step entries with
// the last element being equal to end.
const colors = d3.range(0, 1 + 1E-9, 1 / 30).map(a => {
  return tmpScale(a);
});

const color = d3.scale.quantize()
                 .domain([-1, 1])
                 .range(colors);


export function drawCanvases(network) {
  // just draw final output for now
  const canvas = document.getElementById('output');
  const density = canvas.width;
  const data = network.output.outputs;
  const ctx = canvas.getContext('2d');
  const img = ctx.createImageData(density, density);
  for (let i = 0, p = -1; i < density*density; i++) {
      const c = d3.rgb(color(data[i]));
      img.data[++p] = c.r;
      img.data[++p] = c.g;
      img.data[++p] = c.b;
      img.data[++p] = 160;
  }
  ctx.putImageData(img, 0, 0);
}
