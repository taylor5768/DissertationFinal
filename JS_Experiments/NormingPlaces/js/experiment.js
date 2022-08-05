function make_slides(f) {
  var   slides = {};

  slides.botcaptcha = slide({
      name : "botcaptcha",
      start: function() {

      // define possible speaker and listener names
      // fun fact: 10 most popular names for boys and girls
      var speaker = _.shuffle(["Randall", "Frank", "Robert", "Thomas", "Donald", "George", "Richard", "Joseph", "Edward", "Charles"])[0];
      var listener = _.shuffle(["Mary", "Patricia", "Dorothy", "Linda", "Karen", "Barbara", "Susan", "Betty", "Sandra", "Margaret"])[0];

      var story = speaker + ' says to ' + listener + ': "It\'s a beautiful day, isn\'t it?"' + '<br><br><br><br> Who is ' + speaker + ' talking to? Write the name into the box.';

      $("#story").html(story);

      // don't allow enter press in text field
      $('#listener-response').keypress(function(event) {
          if (event.keyCode == 13) {
              event.preventDefault();
          }
      });

      // don't show any error message
      $("#error").hide();
      $("#error_incorrect").hide();
      $("#error_2more").hide();
      $("#error_1more").hide();

      // amount of trials to enter correct response
      var trial = 0;

      // when button is pressed
      $("#next").on("click", function() {

        // get rid of spaces in response
        response = $("#listener-response").val().replace(" ","");

        // response correct
        if (listener.toLowerCase() == response.toLowerCase()) {
            // I always save their response globally in the data, but I don't know
            // whether you want that
            exp.go();

        // response false
        } else {
            trial = trial + 1;
            $("#error_incorrect").show();
            if (trial == 1) {
                $("#error_2more").show();
            } else if (trial == 2) {
                $("#error_2more").hide();
                $("#error_1more").show();
            } else {
                // incorrect response on third try
                $("#error_incorrect").hide();
                $("#error_1more").hide();
                // remove button, so that the participant can't advance
                $("#next").hide();
                // deactivate text field
                $('#listener-response').css("opacity", "0.2");
                $('#listener-response').prop("disabled", true);
                $("#error").show();
            };
        };
            
        });

    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      $('html,body').scrollTop(0);
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
    slides.instructions0 = slide({
    name : "instructions0",
    start : function() {
    $('html,body').scrollTop(0);

    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst0 = "Let's get started!  ";
    inst0 = inst0 + "";

		//inst0 = inst0 + "Tell us how much you agree with the following statements.";
    	$("#inst0").html(inst0);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

  slides.block0 = slide({
    name : "block0",
    present : exp.stims_block0,
    start : function() {
      $(".err").hide();
    },
    present_handle : function(stim) {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	 
      //console.log(this.stim);   
      var utterance = this.stim.prior_fact+".<br>";  
      //var utterance = this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""
	  $(".sentence").html(utterance);
	  $(".leftLabel").html("strongly disagree")
	  $(".rightLabel").html("strongly agree")
	  var question = "";
	  //console.log(this.stim.block);
	  question = this.stim.question+"?";
	  $(".question").html(question);	  
    },

    button : function() {
    	//console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block0",
      "question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
   	  //"prior" : this.stim.prior,
	  //"prior_fact" : this.stim.prior_fact,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  
  slides.instructions1 = slide({
    name : "instructions1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst1 = "We'll ask you how likely different people are to be Democrats or Republicans."

//    	console.log(block_order);
    	//if (exp.stims_block1[0].block == "prior") {
        inst1 = inst1 + ""
    		//inst1 = inst1 + "<br><br>Tell us how much you agree with the following statements."
    	/*} else if (exp.stims_block2[0].block == "projective"){
    		inst1 = inst1 + "<br><br>Now imagine that you are attending an online event. <br><br> Attendees at the event are invited to discuss political issues with each other. The event is hosted on a website on which you can listen and speak to other participants by moving an avatar around a virtual space.  As you navigate around the virtual space, you can see pictures and basic information about other nearby participants, and overhear them speak to other participants. For each person you overhear, we'll ask you how certain the person is about something s/he said.  You'll give your answers on a scale."    		
    		}
        else
        inst1 = inst1 + "<br><br> Now imagine that you are attending an online event. <br><br> Attendees at the event are invited to discuss political issues with each other. The event is hosted on a website on which you can listen and speak to other participants by moving an avatar around a virtual space.  As you navigate around the virtual space, you can see pictures and basic information about other nearby participants. For each person whose profile you see, give your impressions of that person by adjusting the sliders."      */  

    	$("#inst1").html(inst1);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

 
  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    start : function() {
     $('html,body').scrollTop(0);
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));           
      $(".err").hide();
    },
    present_handle : function(stim) {
      $('html,body').scrollTop(0);

      this.stim = stim;
      console.log("this.stim",this.stim)
      this.stim.trial_start = Date.now();      
        $(".err").hide();     
    this.init_sliders();
      exp.sliderPost = null;
      /// 
      console.log("this.stim.context", this.stim.context)
      var utterance = "";


        if (this.stim.trigger_class== "C"){
          speakerProfile =this.stim.gender+"-"+this.stim.socialInfo_code + "-" + this.stim.name + ".png"
          }
        else{
          speakerProfile =this.stim.trigger_class+ "-"+this.stim.gender+"-"+this.stim.socialInfo_code + "-" + this.stim.name + ".png"
        }


      
        speakerProfileFilePath ="speakerProfiles/"+speakerProfile;      
        console.log("speakerProfileFilePath",speakerProfileFilePath); 
        $("#speakerProfileImgProjective").attr("src", speakerProfileFilePath);
      
      


      utterance =  this.stim.utterance + ".";

      $("#sentence").html(utterance);

    ///
    var leftLabel = "";
    if (this.stim.block == "prior") {
        leftLabel = "strongly disagree";
    } else {
        leftLabel = "no"}
    $(".leftLabel").html(leftLabel);
    var rightLabel = "";
    if (this.stim.block == "prior") {
        rightLabel = "strongly agree";
    } else {
        rightLabel = "yes"}
    $(".rightLabel").html(rightLabel);
    ///
    var question = "";

    //console.log(this.stim.block);
     if (this.stim.socialCondition == "D") {
    //    question = this.stim.question +".";
        utterance = "How likely is " + this.stim.name + " to be a <b><i>Democrat</b></i>?";
    } else {
    //    question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";   
        utterance = "How likely is " + this.stim.name + " to be a <b><i>Republican</b></i>?"; 
      }
    $(".question").html(utterance);    
    },        

    button : function() {
      //console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider3", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "trigger_class": this.stim.trigger_class,
      "socialCondition":this.stim.socialCondition,
      "name": this.stim.name,
      "SpeakerGender": this.stim.gender,
      "socialInfo":this.stim.socialInfo,
      "socialInfo_code":this.stim.socialInfo_code,
      "list":this.stim.list,
      "list_firstSocCond": this.stim.list_firstSocCond,
      "whichSocConditionFirst": this.stim.whichSocConditionFirst,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });  
 

  slides.questionaire =  slide({
    name : "questionaire",

    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));           
      $(".err").hide();
        this.init_sliders();
        exp.sliderPost = null;  

    },

    init_sliders : function() {
      utils.make_slider("#demo_slider", function(event, ui) {
        exp.sliderPost = ui.value;

      });
    },


    submit : function(e){
    var checkboxes = document.querySelectorAll('input[type="checkbox"]')
    checkboxes.forEach(function(cbox) {
      //this solution from https://www.py4u.net/discuss/300854 - answer #4
        if (!cbox.checked) { // If the checkbox is checked it keeps its value, default value is 'on' 
            cbox.value=0 // Otherwise change the value to off, or something else
            cbox.checked = true // We need to check it, so it sends the value
        }
    })

      //somehow here we need to do something similar - say if no button w/name e.g., "party" is checked, then set default value and check it so the value sends
      //my hacky solution partially based on https://stackoverflow.com/a/13060348  
        function validateRadio(whichGroup) {
            buttonGroupName='input[name="' + whichGroup + '"]'
            console.log(buttonGroupName)
            var radiobuttons = document.querySelectorAll(buttonGroupName)
            for (var i=0; i<radiobuttons.length; (i++)){
              if (radiobuttons[i].checked == true){
                console.log("found checked")
                break;
              }
              if (i==(radiobuttons.length-1)){
                console.log("none were checked")
                radiobuttons[i].value=-1
                radiobuttons[i].checked = true
              }
            }
          }

        var radionames = document.querySelectorAll("input[type='radio']")
        console.log("the names", radionames)
        for (i in radionames){
          thename = radionames[i].name
          validateRadio(thename)
          }

    exp.subj_data = {
      language : $("#language").val(),
  //        enjoyment : $("#enjoyment").val(),
  //        asses : $('input[name="assess"]:checked').val(),
      american : $('input[name="ame"]:checked').val(),
      accent : $('input[name="accent"]:checked').val(),
      whichAccent : $("#whichAccent").val(),

      //politics : $('input[name="demo_slider"]').val(),
      politics : exp.sliderPost,


      //politics : $('input[name="pol"]:checked').val(),
    
      party : $('input[name="party"]:checked').val(),
      otherParty : $("#otherParty").val(),
      //race: getCheckedBoxes("race"),
      //race:   document.querySelectorAll('input[name=race]:checked'),
 
      race_white: $('input[name="white"]:checked').val(),
      race_black: $('input[name="black"]:checked').val(),
      race_latino: $('input[name="latino"]:checked').val(),
      race_asian: $('input[name="asian"]:checked').val(),
      race_native: $('input[name="native"]:checked').val(),
      race_hawaiian: $('input[name="hawaiian"]:checked').val(),
      race_otherRaceCheck: $('input[name="otherRaceCheck"]:checked').val(),
      race_otherRaceText : $("#otherRaceText").val(),
      age : $("#age").val(),
      gender : $("#gender").val(),
      state : $("#state").val(),

      education : $("#education").val(),
      income : $("#income").val(),
       // occupation : $("#occupation").val(),
      about : $("#about").val(),

      comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.finished = slide({
    name : "finished",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      //setTimeout(function() {turk.submit(exp.data);}, 1000);
      proliferate.submit(exp.data);
    }
  });

  return slides;
}

///// everything above this line is the function make_slides 

/// init ///
function init() {

// added this for UniqueTurker
$(document).ready(function(){
   var ut_id = "judith-tonhauser-attitude_preds_projection-exp4";
   if (UTWorkerLimitReached(ut_id)) {
     $(".slide").hide();
     $("body").html("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
}});




// makeStim gets a trigger from items, gets a name to create the item
// then it calls the getContent function for that trigger, which returns a unique content
// then it gets the utterance and question for that trigger/content combination
// and returns: name, gender, trigger, content, utterance, question for that trigger
  function makeStim(i) {
    speaker_info = selectedListAsVar[i];
    console.log("speaker_info", speaker_info);
    if (speaker_info.split("-")[0]!="M" && speaker_info.split("-")[0]!="F"){
      //console.log("speaker_info",speaker_info)
      gender=speaker_info.split("-")[1]
      name=speaker_info.split("-")[3]
      //console.log("name",name);
      //console.log("gender",gender);
      regionCode = speaker_info.split("-")[2]

      trigger_class = speaker_info.split("-")[0]
      console.log("trigger_class here",trigger_class)
    }


    else {
        //console.log("speaker_region" ,speaker_info);
        gender = speaker_info.split("-")[0];
        name = speaker_info.split("-")[2];
        //console.log("name",name);
        //console.log("gender",gender);
        regionCode = speaker_info.split("-")[1]
        trigger_class = "C"
      }

    

    if (regionCode == "S"){
      region = "southern"
    }
    else if (regionCode=="N"){
      region = "non-southern"
    }

    else if (regionCode=="R"){
      region = "Republican"
    }
    
    else if (regionCode == "D"){
      region = "Democrat"
    }

    //console.log("region", region)
    return {
	  "name": name,
	  "gender": gender,
    "trigger_class":trigger_class,	
      "socialInfo":region,
       "socialInfo_code":regionCode,
    }
  }
  


exp.stims_block1 = [];


// items is a shuffled array, with each trigger once
// this for loop runs calls makeStim as often as their are triggers
// and adds the newly created stim to the the set of stims for block 1

console.log("selectedList",selectedListAsVar)
console.log("selectedList.length",selectedListAsVar.length)
  for (var i=0; i<selectedListAsVar.length; i++) {
  	var stim = makeStim(i);
  	exp.stims_block1.push(jQuery.extend(true, {}, stim));	
  }



socialConditions = _.shuffle(["D","R"])
//socialConditions = ["R","D"]

firstSocCond = socialConditions[0];
console.log("firstSocCond",firstSocCond)
secondSocCond = socialConditions[1];
list_firstSocCond = selectedListAsString + "-" +firstSocCond;
console.log("listfirstSocCond",list_firstSocCond)

maleCount = 0;
femaleCount =0;
max = exp.stims_block1.length/6; //24 stims, first 8 of each gender are critical items; 4 of these should be assigned each social condition
firstMaleParty=true;
firstFemaleParty=true;
firstMaleEmpty=true;
firstFemaleEmpty=true;

for (var k=0; k<exp.stims_block1.length; k++){
console.log("which stim", exp.stims_block1)
  if (exp.stims_block1[k].trigger_class=="C"){
  
        if (exp.stims_block1[k].gender == "M" && maleCount <max){
          //console.log("maleCount",maleCount)
             exp.stims_block1[k].socialCondition = firstSocCond;
             maleCount = maleCount+1

        }
        else if (exp.stims_block1[k].gender == "M" && maleCount>=max) {
          //console.log("maleCount",maleCount)
          exp.stims_block1[k].socialCondition = secondSocCond;
          maleCount = maleCount+1
        }

        else if (exp.stims_block1[k].gender == "F" && femaleCount <max){
             exp.stims_block1[k].socialCondition = firstSocCond;
             femaleCount = femaleCount+1

        }
        else if (exp.stims_block1[k].gender == "F" && femaleCount>=max) {
          exp.stims_block1[k].socialCondition = secondSocCond;
          femaleCount = femaleCount+1
        }
  }

  else if (exp.stims_block1[k].trigger_class=="Party"){

    if (exp.stims_block1[k].gender=="M" && firstMaleParty == true){
        exp.stims_block1[k].socialCondition = firstSocCond;
        firstMaleParty = false;
      }

    else if (exp.stims_block1[k].gender=="M" && firstMaleParty == false){
      exp.stims_block1[k].socialCondition = firstSocCond;
    
    }

    else if (exp.stims_block1[k].gender=="F" && firstFemaleParty == true){
        exp.stims_block1[k].socialCondition = secondSocCond;
        firstFemaleParty = false;
      }

    else if (exp.stims_block1[k].gender=="F" && firstFemaleParty == false){
      exp.stims_block1[k].socialCondition = secondSocCond;
      
    }
  }


  else if (exp.stims_block1[k].trigger_class=="Empty"){

    if (exp.stims_block1[k].gender=="M" && firstMaleEmpty == true){
        exp.stims_block1[k].socialCondition = firstSocCond;
        firstMaleEmpty = false;
      }

    else if (exp.stims_block1[k].gender=="M" && firstMaleEmpty == false){
      exp.stims_block1[k].socialCondition = firstSocCond;
    
    }

    else if (exp.stims_block1[k].gender=="F" && firstFemaleEmpty == true){
        exp.stims_block1[k].socialCondition = secondSocCond;
        firstFemaleEmpty = false;
      }

    else if (exp.stims_block1[k].gender=="F" && firstFemaleEmpty == false){
      exp.stims_block1[k].socialCondition = secondSocCond;
      
    }
  }
    exp.stims_block1[k].whichSocConditionFirst = firstSocCond
    exp.stims_block1[k].list = selectedListAsString;
    exp.stims_block1[k].list_firstSocCond = list_firstSocCond;

}



  console.log("before shuffle",exp.stims_block1)
  exp.stims_block1 = _.shuffle(exp.stims_block1); 
  console.log("stims", exp.stims_block1);
  exp.stims_block0 = jQuery.extend(true, [], exp.stims_block1);




	exp.stims_block1 = _.shuffle(exp.stims_block1);  
	exp.stims_block0 = _.shuffle(exp.stims_block0);
	
  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["botcaptcha", "instructions","instructions1", "block1", 'questionaire', 'finished'];
  //exp.structure=[  'questionaire', 'finished'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 3 + 24; 
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}