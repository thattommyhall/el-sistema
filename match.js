var genome_name = window.location.hash.slice(1);
console.log(genome_name);

var myFirebaseRef = new Firebase("https://el-sistema.firebaseio.com/plants/");

function randItem(items) {
    return items[Math.floor(Math.random()*items.length)];
}

window.contestants = [];

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
    var opponent = randItem(genomes);
    contestants = [];
    contestants.push(selected);
    contestants.push(opponent);
    $("#match-title").text(selected.name + " by " + selected.author+ " vs "+opponent.name + " by " + opponent.author);
    el_sistema.core.run(selected.genome, opponent.genome);
});

//var makeScores = function(){
//    var template = $('#plant-row').html();
//    Mustache.parse(template);
//
//    var scores = el_sistema.core.scores();
//    for(var i=0; i<scores.length; i++) {
//        contestants[i].score = scores[i];
//    }
//    $("#plants-table tbody tr").remove();
//
//    contestants.sort(function(a,b){
//        if(a.score < b.score)  {
//            return 1;
//        }else if(a.score > b.score) {
//            return -1;
//        } else {
//            return 0;
//        }
//    });
//    console.log(contestants)
//    for(var i=0; i<contestants.length; i++) {
//        var rendered = Mustache.render(template, contestants[i]);
//        $('#plants-table tbody').append(rendered);
//    }
//};
//
//setInterval(makeScores, 500);
//makeScores();
