const margin = {top: 80, right: 100, bottom: 40, left: 60}
const figWidth = 800 - margin.left - margin.right
const figHeight = 400 - margin.top - margin.bottom

svg.attr("width", figWidth + margin.left + margin.right)
    .attr("height", figHeight + margin.top + margin.bottom)

svg.selectAll("circle")
				.data(data)
				.enter().append("circle")
				.attr("cx", function(d) {return d*250; })
				.attr("cy", function(d) {return d*250; })
				.attr("r", function(d) {return d*32;})
				.attr("fill", "blue");

svg.append("image")
  .attr("xlink:href", "https://images.unsplash.com/photo-1485871981521-5b1fd3805eee?fit=crop&w=200&h=200")
  .attr("class","svg-image")
  .attr("x", figHeight)
  .attr("y", 0)
  .attr('width', 80)
  .attr('height', 80);

svg.append("g")
  .attr("xlink:href", "./images/linux.jpeg")
  .attr("class","svg-image")
  .attr("x", 0)
  .attr("y", 100)
  .attr('width', figWidth/2)
  .attr('height', figHeight/2);

