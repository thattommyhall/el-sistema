// var myFirebaseRef = new Firebase("https://el-sistema.firebaseio.com/plants");

genomes = {
  "genome_1": el_sistema.core.genome_1,
  "genome_2": el_sistema.core.genome_2
};

var genome_name = window.location.hash.slice(1);
console.log(genome_name);
var myFirebaseRef = new Firebase("https://el-sistema.firebaseio.com/plants/")

function randItem(items) {
  return items[Math.floor(Math.random()*items.length)];
}

myFirebaseRef.on("value", function(snapshot) {
  var genomes = [];
  var selected = null;
  snapshot.forEach(function(plant) {
    var current = plant.val();
    current.key = plant.key();
    if (current.key === genome_name) {
      selected = current;
    }
    else{
      genomes.push(current);
    }
  });
  console.log(selected.genome);
  console.log(randItem(genomes).genome);
  el_sistema.core.run(selected.genome, randItem(genomes).genome);
  // ["genome_1", "genome_2"].forEach( function(item) {
  //   console.log(item);
  //   var o = document.createElement("option");
  //   o.textContext = item;
  //   o.value = genomes[item];
  //   o.label = item;
  //   document.getElementById("genomeselect").appendChild(o);
  // });
});

function clickedRun() {
  var genome = $("#genomeselect").find(":selected").val();
  console.log(genome)
  el_sistema.core.run(genome, genome);
}
