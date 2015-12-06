// var myFirebaseRef = new Firebase("https://el-sistema.firebaseio.com/plants");

genomes = {
  "genome_1": el_sistema.core.genome_1,
  "genome_2": el_sistema.core.genome_2
};


["genome_1", "genome_2"].forEach( function(item) {
  console.log(item);
  var o = document.createElement("option");
  o.textContext = item;
  o.value = genomes[item];
  o.label = item;
  document.getElementById("genomeselect").appendChild(o);
});

function clickedRun() {
  var genome = $("#genomeselect").find(":selected").val();
  console.log(genome)
  el_sistema.core.run(genome, genome);
}
