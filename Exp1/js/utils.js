

function roll(numFaces, reroll) {
    $('#roll-button').prop('disabled', true);
    $('#dice').css('background-color','#e15858');
    var numRotations = Math.floor(Math.random()*10)+5;
    var countRotations = 0;
    function setRandomTimeout(callback){
        var internalCallback = function(tick) {
            return function() {
                if (--tick >= 0) {
                    var rollSideTime = Math.random()*1000/(numRotations-countRotations); //decreasing rotation speed
                    ++countRotations;
                    window.setTimeout(internalCallback, rollSideTime);
                    callback();
                } else{
                    if(reroll){
                        $('#roll-button').prop('disabled', false);
                    }
                    $('#dice').css('background-color','#e31b1b');
                    $('#subjResponse').css('opacity','1');
                    turn.rolled = true;
                }
            }
        }(numRotations);
        window.setTimeout(internalCallback);
    };
    setRandomTimeout(function(){
        trial.trueRoll = sample([...Array(numFaces+1).keys()]);
        $('#dice-text').html(trial.trueRoll);
    });
}

function report(){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('#report-button').prop('disabled', true);

    function bullshitterWait() {
        flickerWait();
        
        trial.waitTime = 1000 + 3000*exponential(0.75);
        setTimeout(function(){
            clearInterval(trial.timer);
            $('#subjResponse').html("<p><br><br><br>Your opponent made a decision.<br><br><br></p>")
            $('#subjResponse').css('opacity','1');
            $('#next').prop('disabled',false);
        }, trial.waitTime);
    }
    bullshitterWait();
}

function computerRoll(){
    //groundTruth
    trial.trueRoll = sample([...Array(expt.diceSides+1).keys()]);
    turn.rolled = true;
    trial.compDetect = -1;

    if(trial.exptPart == "trial" & Math.random() < 0.2){ //only occurs during trial
        trial.compUnifLie = true;
        trial.reportedRoll = sample([...Array(expt.diceSides+1).keys()]);
    } else{
        var lie = sample([...Array(expt.diceSides+1).keys()]); //internal sample
        trial.compLie = lie;
        if(lie <= trial.trueRoll){
            trial.reportedRoll = trial.trueRoll;
        } else{
            trial.reportedRoll = lie;
        }
    }   
}

function callout(call){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('.callout-button').prop('disabled', true);
    if(call == 'accept'){
        $('#accept-button').css('opacity','1');
        $('#reject-button').css('opacity','0.5');
        trial.callBS = false;
    } else{
        $('#reject-button').css('opacity','1');
        $('#accept-button').css('opacity','0.5');
        trial.callBS = true;
    }
    $('#next').prop('disabled',false);
}

function computerBSDetector(){
    trial.compDetect = cunif(0, expt.diceSides, trial.reportedRoll); //0.9 is to make it so that doesn't always call out 10
    var rand = Math.random();
    //trial.callBS = Math.random() < 0.4; //EDIT: opponent is random detector for now
    trial.callBS = rand < trial.compDetect;
    trial.compLie = -1;
}

function restartTrial(){
    $('#trial').css('display','block');
    window.scrollTo(0, 0);
    if(trial.roleCurrent == "bullshitter"){
        var roletxt = "die-roller"
    } else{
        var roletxt = "responder"
    }
    $('.trialNum').html("Round " + (trial.trialNumber+1) + ": You are the <i>" + roletxt + "</i>");

    trial.compUnifLie = false;
    $('#subjResponse').css('opacity','0');
    $('.callout-button').css('opacity','0.8');
    $('.callout-button').prop('disabled', false);
    $('#buttonResponse').css('opacity','0');
    turn.rolled = false;
    $('input[type=text]').val("");
    $('#dice-text').html("?");
    $('#reportRoll').prop('disabled',true);
    $('#next').prop('disabled',true);

    if(trial.exptPart != 'practice'){
        trial.pseudoRound = trial.trialNumber in expt.pseudo;
    }

    trial.catch.key = -1;
    trial.catch.response = -1;
    trial.catch.responseTime = -1;
    $('#catchQ').hide();
    $('#sliderContainer').hide();
    $('#postSlider').hide();
    $('#qInstruct').hide();
    $('#slider').addClass('inactiveSlider');
    $('#slider').removeClass('activeSlider');

    trial.startTime = Date.now();
}

function flickerWait(){
    var op = 0.1;
    var increment = 0.1;
    $('#subjResponse').html('<p><br><br><br>Waiting for your opponent...<br><br><br></p>');
    $('#subjResponse').css('opacity','0');
    trial.timer = setInterval(go, 50)
    function go(){
        op += increment;
        $('#subjResponse').css('opacity', op);
        if(op >= 1){
            increment = -increment;
        }
        if(op <= 0){
            increment = -increment;
        }
    }
}


function submitCatchText(){
    trial.catch.responseTime = Date.now() - trial.catch.responseStartTime;
    $('input[type=text]').prop('disabled',true);
    $('input[type=text]').css('opacity','0.7');
    $('#catch-button').prop('disabled', true);
    var timeoutTime = 0;
    if(trial.catch.key == trial.catch.response){
        $('#catchQ').append('<img src="img/yup.png" height=18 vertical-align="middle" hspace="20">');
    } else{
        $('#catchQ').append('<img src="img/nah.png" height=18 vertical-align="middle" hspace="20">');
        timeoutTime = 3000;
    }
    setTimeout(function(){
        if(trial.exptPart == 'practice' | (trial.trialNumber + 1) % 5 == 0){
            $('.scoreboardDiv').css('opacity','1');
        } 
        $('.scoreReport').css('opacity','1');
        $('#nextScoreboard').css('opacity','1');
    }, timeoutTime);
}



function catchTrial(role, exptPart){
    if(trial.exptPart == "practice" & trial.trialNumber == 0){
        $('#catchQ').before("<p id='qInstruct'>Throughout the experiment, you will randomly be asked questions about the task.<br>If you get the question wrong, you have to wait 3 seconds before being able to move on.<br><br></p>")
    }
    if(role == 'bullshitter'){
        trial.catch.question = 'What number did you roll?';
        trial.catch.key = trial.trueRoll;
    } else{
        trial.catch.question = 'What number did your opponent report rolling?';
        trial.catch.key = trial.reportedRoll;
    }
    $('#catchQ').html('<label>'+trial.catch.question+'</label>');
    var inputTxt = '<input type="text" id="reportCatch" value="" size="2" maxlength="2" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"/> ';
    inputTxt += '<button class="active-button" id="catch-button" type="button" onclick="submitCatchText();">Submit</button>';
    $('#catchQ').append(inputTxt);

    $('input[type=text]').on('input',
        function(){
            trial.catch.response = parseInt($(this).val());
            if(trial.catch.response >= 1 && trial.catch.response <= 10 ){
                $('#catch-button').prop('disabled',false);
            } else{
                $('#catch-button').prop('disabled',true);
            }
    });
    

    $('#catch-button').prop('disabled',true);  
    $('.scoreReport').css('opacity','0');
    $('.scoreboardDiv').css('opacity','0');
    $('#nextScoreboard').css('opacity','0');
}



// helper functions
function sample(set) {
    return (set[Math.floor(Math.random() * set.length)]);
}

function sample_without_replacement(sampleSize, sample){
    var urn = [];
    if(Number.isInteger(sample)){
        urn = [...Array(sample).keys()];
    } else {
        urn = sample.slice(0);
    }
    var return_sample = [];
    for(var i=0; i<sampleSize; i++){
        var randomIndex = Math.floor(Math.random()*urn.length);
        return_sample.push(urn.splice(randomIndex, 1)[0]);
    }
    return return_sample;
}

function randomDouble(min, max){
    return Math.random() * (max - min) + min;
}

function shuffle(set){
    var j, x, i;
    for (i = set.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = set[i];
        set[i] = set[j];
        set[j] = x;
    }
    return set;
}

function recordData(){
    trialData.push({
        exptPart: trial.exptPart,
        trialNumber: trial.trialNumber,
        roleCurrent: trial.roleCurrent,
        diceSides: expt.diceSides,
        trueRoll: trial.trueRoll,
        reportedRoll: trial.reportedRoll,
        compLie: trial.compLie,
        compUnifLie: trial.compUnifLie,
        compDetect: trial.compDetect,
        callBS: trial.callBS,
        playerTrialScore: trial.playerTrialScore,
        oppTrialScore: trial.oppTrialScore,
        playerTotalScore: expt.stat.playerTotalScore,
        oppTotalScore: expt.stat.oppTotalScore,
        waitTime: trial.waitTime,
        responseTime: trial.responseTime,
        catchQuestion: trial.catch.question,
        catchKey: trial.catch.key,
        catchResponse: trial.catch.response,
        catchResponseTime: trial.catch.responseTime,
        pseudoRound: trial.pseudoRound,
        trialTime: trial.trialTime
    })
}

function debugLog(message) {
    if(expt.debug){
        console.log(message);
    }
}

function binom(n, p, k){
    return (factorial(n)/(factorial(k)*factorial(n-k))) * p ** k * (1-p) ** (n-k);
}

function factorial(x){
    if(x == 0){
        return 1;
    } else{
        return x*factorial(x-1);
    }
}

function cbinom(n, p, k){
    if(k == 0){
        return binom(n, p, 0);
    } else{
        return binom(n, p, k) + cbinom(n, p, k-1);
    }
}

function cunif(min, max, x){ //discrete
    return (x-min+0.5) / (max-min+1);
}


function exponential(lambda){
    return lambda * Math.E ** (-lambda*Math.random())
}

function calculateStats(string, numer, denom){
    if(denom == 0){
        $(string).html("N/A");
    } else{
        $(string).html(Math.round(numer * 100 / denom)+"%");
    }
}

function scorePrefix(score){
    if(score <= 0){
        return(score);
    } else{
        return("+" + score);
    }
}

function distributeChecks(totalTrials, freq){
    var shuffled = shuffle([...Array(totalTrials).keys()]);
    return(shuffled.slice(0,Math.floor(totalTrials*freq)));
}



function distributePseudo(totalTrials, minArrPseudo, maxArrPseudo){
    var pseudoDict = {};
    var arrPseudo = [];
    var bucketOdd = [];

    for(var a=minArrPseudo; a <= maxArrPseudo; a++){
        arrPseudo.push(a);
    }
    for(var i=0; i<=totalTrials/2; i++){
        bucketOdd.push(i);
    }
    var bucketEven = bucketOdd.slice(0);

    for(var o=0; o<arrPseudo.length; o++){
        index = Math.floor(randomDouble(0, bucketOdd.length));
        pseudoDict[(2*bucketOdd.splice(index, 1)[0]+1)] = arrPseudo[o];
    }
    for(var e=0; e<arrPseudo.length; e++){
        index = Math.floor(randomDouble(0, bucketEven.length));
        pseudoDict[(2*bucketEven.splice(index, 1)[0])] = arrPseudo[e];
    }
    return(pseudoDict);
}









