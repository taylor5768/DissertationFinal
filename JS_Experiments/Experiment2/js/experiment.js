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
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  
  slides.instructions1 = slide({
    name : "instructions1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst1 = "Let's get started!"

        inst1 = inst1 + ""

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
      //console.log("block1")
      this.stim.trial_start = Date.now();      
        $(".err").hide();     
      this.init_sliders();
      exp.sliderPost = null;

        //console.log("this.stim.CC",this.stim.CC)

        speakerProfile = this.stim.file + ".png";
        //console.log("speakerProfile",speakerProfile)

        speakerProfileFilePath ="speakerProfiles/"+speakerProfile;      
        //console.log("speakerProfileFilePath",speakerProfileFilePath); 
        $("#speakerProfileImgProjective").attr("src", speakerProfileFilePath);
      
     beliefQuestion = "How much do you believe the following statement?"
      $(".beliefQuestion").html(beliefQuestion);
    var complement = this.stim.CC
    var CC = complement[0].toUpperCase() + complement.substring(1)
    var CC =   "<i>" + CC + ".</i>"

    var thisStatement = "This statement is..."

    //console.log(this.stim.block);
    $(".CC").html(CC);    
    $(".thisStatement").html(thisStatement);  
    },        

    button : function() {
      if (exp.sliderPost != null) {
        this.log_responses();
        //console.log("data",exp.data_trials)
        //console.log("this",this)
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider1", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "item_type": this.stim.stimType,
      "matrix_clause":this.stim.matrix_clause,
      "CC":this.stim.CC,
      "orientation":this.stim.orientation,
      "topic":this.stim.topic,
      "predicate":this.stim.predicate,
      "name": this.stim.name,
      "block":"block1",
      "speakerGendersMaxes":speakerGendersMaxes,
      "list_genderMaxes":this.stim.list + "_" + speakerGendersMaxes,
      "whichSpPoliticsFirst":speakerPoliticsOrder[0],
      "list_whichSpPoliticsFirst":this.stim.list + "_" + speakerPoliticsOrder[0],
      "SpeakerGender": this.stim.gender,
      "socialInfo":this.stim.socialInfo,
      "list":this.stim.list,
      "file":speakerProfile,
      "participant_beliefs" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });  


   slides.instructions2 = slide({
    name : "instructions2",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));
    $('html,body').scrollTop(0);
            
      var inst2 = "That was the first of the three parts! ";

        inst2 = inst2 + "<br><br> Now imagine that you are attending an online event. <br><br> Attendees at the event are invited to discuss political issues with each other. The event hasn't started yet, so you can't hear other participants yet, but you can see who else is attending by navigating around the virtual space. When you approach someone else in the virtual space, their photo appears, along with some basic information about them. <br><br> For each person whose profile you see, you'll give your impressions of that person by adjusting the sliders.<br><br>  Below is a screenshot of what you should see when the next phase of the experiment begins.  If you aren't able to see the whole profile, try adjusting the size of your browser window. If you aren't able to adjust the window as necessary, please contact the researchers immediately via the Prolific platform or at mahler.38@osu.edu."      
      

      $("#inst2").html(inst2);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });   
  
  slides.block2 = slide({
    name : "block2",
    present : exp.stims_block2,
    start : function() {
    $('html,body').scrollTop(0);
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));
    err_socialEval = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"           
    $(".err_socialEval").html(err_socialEval);
    },
    present_handle : function(stim) {
      $('html,body').scrollTop(0);
      this.stim = stim;
     console.log("stim!!!", stim)
      this.stim.trial_start = Date.now();     
      err_socialEval = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"           
 
      $(".err_socialEval").html(err_socialEval);
    this.init_sliders();
      exp.intelligencePost = null;
      exp.assertivenessPost = null;
      exp.empathyPost = null;
      exp.democratPost = null;
      exp.republicanPost = null;
      exp.spBeliefPost = null;
      exp.femininityPost = null; 
      exp.locationPost = null;
      var utterance = "";



      speakerProfile =this.stim.gender+"-"+this.stim.socialInfo + "-" + this.stim.name + ".png"

      speakerProfileFilePath ="speakerProfiles/"+speakerProfile;      
     // console.log("speakerProfileFilePath",speakerProfileFilePath); 
      $("#speakerProfileImg").attr("src", speakerProfileFilePath);


 
    
        question = "What are your impressions of  "+this.stim.name+"?" ;   
        speakerIs = "<i>"  + this.stim.name +" is...</i>" 

        spBelief_question = "How likely is "+this.stim.name+" to believe that <br>" +this.stim.CC + "?";     
        spPolitics_question = "How likely is "+this.stim.name+" to be a...";  
        
        if (this.stim.stateType == "CORRECT"){
          state = this.stim.correctState
        }
        else if (this.stim.stateType == "INCORRECT"){
          state = this.stim.incorrectState
        }
        spLocation_question = "Is "+this.stim.name+ " from " + state +"?"                       


    $(".question").html(question);
    $(".speakerIs").html(speakerIs);
    $(".spBelief_question").html(spBelief_question);    
    $(".spPolitics_question").html(spPolitics_question);  
    $(".spLocation_question").html(spLocation_question);
    },        

    button : function() {
      if (exp.intelligencePost!= null && exp.assertivenessPost !=null && exp.empathyPost!=null && exp.femininityPost!=null && exp.democratPost!=null && exp.republicanPost!=null && exp.spBeliefPost!=null & exp.locationPost!=null) {

        this.log_responses();
        console.log("data",exp.data_trials)

        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        err_socialEval = "Please adjust all the sliders to continue"
        $(".err_socialEval").html(err_socialEval)
      }
    },
    init_sliders : function() {
      utils.make_slider("#intelligence", function(event, ui) {
        exp.intelligencePost = ui.value;
        intelligence_rating = exp.intelligencePost
      });
      utils.make_slider("#assertiveness", function(event, ui) {
        exp.assertivenessPost = ui.value;
        assertiveness_rating = exp.assertivenessPost
      });

      utils.make_slider("#empathy", function(event, ui) {
        exp.empathyPost = ui.value;
        empathy_rating = exp.empathyPost
      });

    
      utils.make_slider("#femininity", function(event, ui) {
        exp.femininityPost = ui.value;
        femininity_rating = exp.femininityPost
      });

      utils.make_slider("#location", function(event, ui) {
        exp.locationPost = ui.value;
        location_rating = exp.locationPost
      });

      utils.make_slider("#democrat", function(event, ui) {
        exp.democratPost = ui.value;
        democrat_rating = exp.democratPost
      });

      utils.make_slider("#republican", function(event, ui) {
        exp.republicanPost = ui.value;
        republican_rating = exp.republicanPost
      });

      utils.make_slider("#speaker_belief", function(event, ui) {
        exp.spBeliefPost= ui.value;
        spBelief_rating = exp.spBeliefPost
      });
    },


    log_responses : function() {
      exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "item_type": this.stim.stimType,
      "matrix_clause":this.stim.matrix_clause,
      "CC":this.stim.CC,
      "orientation":this.stim.orientation,
      "topic":this.stim.topic,
      "predicate":this.stim.predicate,
      "name": this.stim.name,
      "block" : "block2",
      "speakerGendersMaxes":speakerGendersMaxes,
      "list_genderMaxes":this.stim.list + "_" + speakerGendersMaxes,
      "whichSpPoliticsFirst":speakerPoliticsOrder[0],
      "list_whichSpPoliticsFirst":this.stim.list + "_" + speakerPoliticsOrder[0],
      "SpeakerGender": this.stim.gender,
      "state_correct":this.stim.correctState,
      "state_incorrect":this.stim.incorrectState,
      "state_type":this.stim.stateType,
      "socialInfo":this.stim.socialInfo,
      "list":this.stim.list,
      "file":speakerProfile,
      "rt" : Date.now() - this.stim.trial_start,

      "orientation":this.stim.orientation,
      "intelligence_rating" : intelligence_rating,
      "assertiveness_rating" : assertiveness_rating,
      "empathy_rating" : empathy_rating,
      "femininity_rating" : femininity_rating,
      "democrat_rating" : democrat_rating,
      "republican_rating" : republican_rating,
      "spBelief_rating" : spBelief_rating,
      "location_rating" :location_rating,

      });
    }
  });  

slides.instructions3 = slide({
    name : "instructions3",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));  
     $('html,body').scrollTop(0);
      var inst3 = "That was the second of the three parts! There's one more part left!";
        inst3 = inst3 + "<br><br>Now that the event has started, participants can speak to each other.  As before, you're able to see the other people's profiles, but now you also overhear them. <br> <br> For each person you overhear, we'll ask you how certain the person is about something s/he said.  You'll give your answers on a scale.<br><br>Below is a screenshot of what you should see when the next phase of the experiment begins.  If you aren't able to see the whole profile or the speech bubble doesn't look right, try adjusting the size of your browser window. If you aren't able to adjust the window as necessary, please contact the researchers immediately via the Prolific platform or at mahler.38@osu.edu.<br><br>"        

      $("#inst3").html(inst3);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });   
  
  slides.block3 = slide({
    name : "block3",
    present : exp.stims_block3,
    start : function() {
     $('html,body').scrollTop(0);
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));           
      $(".err").hide();
    },
    present_handle : function(stim) {
      $('html,body').scrollTop(0);

      this.stim = stim;
      //console.log("this.stim",this.stim)
      this.stim.trial_start = Date.now();      
        $(".err").hide();     
    this.init_sliders();
      exp.sliderPost = null;
      var utterance = "";



      speakerProfile =this.stim.gender+"-"+this.stim.socialInfo + "-" + this.stim.name + ".png"
        speakerProfileFilePath ="speakerProfiles/"+speakerProfile;      
        //console.log("speakerProfileFilePath",speakerProfileFilePath); 
        $("#speakerProfileImgProjective").attr("src", speakerProfileFilePath);

          true_false = "NA" // default value for true/false stimuli
          
          if (this.stim.stimType == "critical"){
              utterance =  this.stim.matrix_clause+" " +this.stim.CC +".";
            }
          else if (this.stim.stimType=="MC"){

                    //var CC = complement[0].toUpperCase() + complement.substring(1)


            utterance = this.stim.CC[0].toUpperCase()+this.stim.CC.substring(1) + "."
          }
          else if (this.stim.stimType == "itIsT_F"){

              if (  (this.stim.orientation == "conservative" & this.stim.socialInfo == "N") || (this.stim.orientation == "liberal" & this.stim.socialInfo == "S") ) {    
                    prefix = "It is true that "
                    true_false = "true"

              }
              else if ( (this.stim.orientation == "conservative" & this.stim.socialInfo == "S")|| (this.stim.orientation == "liberal" & this.stim.socialInfo == "N")  )  {
                  prefix = "It is false that "
                  true_false = "false"

              }
              


           utterance = prefix + this.stim.CC + "."
          }
       $("#sentence").html(utterance);

    var leftLabel = "";
  
    leftLabel = "no";
    rightLabel="yes"
    $(".leftLabel").html(leftLabel);
    $(".rightLabel").html(rightLabel);
    var question = "";
    if (this.stim.gender == "F"){
      pronoun = "she"
    }
    else {pronoun = "he"}
    question = "Given what " + this.stim.name +" said, is "+pronoun+" certain that <br> "+this.stim.CC+"?";      
    $(".question").html(question);    
    },        

    button : function() {
      //console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        console.log("data",exp.data_trials)

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
      "item_type": this.stim.stimType,
      "matrix_clause":this.stim.matrix_clause,
      "CC": this.stim.CC,
      "orientation":this.stim.orientation,
      "topic":this.stim.topic,
      "predicate":this.stim.predicate,
      "name": this.stim.name,
      "block" : "block3",
      "speakerGendersMaxes":speakerGendersMaxes,
      "list_genderMaxes":this.stim.list + "_" + speakerGendersMaxes,
      "whichSpPoliticsFirst":speakerPoliticsOrder[0],
      "list_whichSpPoliticsFirst":this.stim.list + "_" + speakerPoliticsOrder[0],
      "SpeakerGender": this.stim.gender,
      "socialInfo":this.stim.socialInfo,
      "true_false":true_false,
      "list":this.stim.list,
      "file":speakerProfile,
      "projection" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });  
 

 

  slides.questionaire =  slide({
    name : "questionaire",

    start : function() {
    $('html,body').scrollTop(0);

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
            //console.log(buttonGroupName)
            var radiobuttons = document.querySelectorAll(buttonGroupName)
            for (var i=0; i<radiobuttons.length; (i++)){
              if (radiobuttons[i].checked == true){
                //console.log("found checked")
                break;
              }
              if (i==(radiobuttons.length-1)){
                //console.log("none were checked")
                radiobuttons[i].value=-1
                radiobuttons[i].checked = true
              }
            }
          }

        var radionames = document.querySelectorAll("input[type='radio']")
        //console.log("the names", radionames)
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
      state1year : $("#state1year").val(),
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
      $('html,body').scrollTop(0);
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



function getSpeaker(genderCounts,genderMaxes,i,type){

    //console.log("remaining speakers",maleDemocrats,femaleDemocrats,maleRepublicans,femaleRepublicans)

    maleDemocratMax = genderMaxes[0]
    femaleDemocratMax = genderMaxes[1]
    maleRepublicanMax = genderMaxes[2]
    femaleRepublicanMax = genderMaxes[3]

    maleDemocratCount = genderCounts[0]
    femaleDemocratCount = genderCounts[1]
    maleRepublicanCount = genderCounts[2]
    femaleRepublicanCount = genderCounts[3]

    firstSpeakerPolitics = speakerPoliticsOrder[0]
    secondSpeakerPolitics = speakerPoliticsOrder[1]

    firstSpeakerGender = speakerGendersOrder[0]
    secondSpeakerGender = speakerGendersOrder[1]

    if (i%2 == 0) { ///even numbers
  //    console.log("odd stim",i)
      speakerPolitics = speakerPoliticsOrder[0]
      preferredSpeakerGender = speakerGendersOrder[0]
    }

    else if (i%2 != 0) { ///odd numbers
    //  console.log("even stim",i)
      speakerPolitics = speakerPoliticsOrder[1]
      preferredSpeakerGender = speakerGendersOrder[1]
    }

    if (speakerPolitics == "NS" & preferredSpeakerGender == "M"){
      if (maleDemocratCount<maleDemocratMax){
        selectedSpeaker = maleDemocrats.pop()
        maleDemocratCount+=1
            }
      else {
        selectedSpeaker = femaleDemocrats.pop()
        femaleDemocratCount+=1
      }
    }
  else if (speakerPolitics == "NS" & preferredSpeakerGender == "F"){
    if (femaleDemocratCount<femaleDemocratMax){
      selectedSpeaker = femaleDemocrats.pop()
      femaleDemocratCount+=1
    }
     else {
        selectedSpeaker = maleDemocrats.pop()
        maleDemocratCount+=1
      }
  }
  else if (speakerPolitics == "S" & preferredSpeakerGender == "M"){
      if (maleRepublicanCount<maleRepublicanMax){
        selectedSpeaker = maleRepublicans.pop()
        maleRepublicanCount+=1
      }
      else {
        selectedSpeaker = femaleRepublicans.pop()
        femaleRepublicanCount+=1
      }
    }
  else if (speakerPolitics == "S" & preferredSpeakerGender == "F"){
      if (femaleRepublicanCount<femaleRepublicanMax){
        selectedSpeaker = femaleRepublicans.pop()
        femaleRepublicanCount+=1
      }
      else {
        selectedSpeaker = maleRepublicans.pop()
        maleRepublicanCount+=1
      }
    }

    genderCounts = [maleDemocratCount,femaleDemocratCount,maleRepublicanCount,femaleRepublicanCount]
    return [genderCounts,selectedSpeaker]
}

function getAdditionalSpeaker(genderCounts,genderMaxes,i,type){

  //selectedSpeaker = additionalSpeakers.pop()
  console.log("i is", i)
  if (i%2 == 0) { ///even numbers
      speakerPolitics = speakerPoliticsOrder[0]
    }
  else if (i%2 != 0) { ///odd numbers
      speakerPolitics = speakerPoliticsOrder[1]
    }

    //console.log("speakerPolitics", speakerPolitics)
    //console.log("add Dem speakers",additionalDemocratSpeakers)
    //console.log("add Rep speakers",additionalRepublicanSpeakers)

  if (speakerPolitics == "NS" ){
      if (additionalDemocratSpeakers.length >0){
        selectedSpeaker = additionalDemocratSpeakers.pop()
        console.log("getting Dem speaker")
        }
      else {
        selectedSpeaker = additionalRepublicanSpeakers.pop()
        console.log("not enough Dem speakers - getting Rep speaker")
        }
      }
  else if (speakerPolitics == "S"){
    if (additionalRepublicanSpeakers.length >0){
        selectedSpeaker = additionalRepublicanSpeakers.pop()
        console.log("getting Rep speaker")

    }
    else {
      console.log("not enough Rep speakers - getting Dem speaker")
        selectedSpeaker = additionalDemocratSpeakers.pop()
        }
    }
  return selectedSpeaker
}

// makeStim gets a trigger from items, gets a name to create the item
// then it calls the getContent function for that trigger, which returns a unique content
// then it gets the utterance and question for that trigger/content combination
// and returns: name, gender, trigger, content, utterance, question for that trigger
  function makeStim(i,type,selectedSpeaker,selectedPredicate) {
     file = "NA"
     name="NA"
     gender="NA"
     socialInfo="NA"
     orientation = "NA"
     speakerInfo="NA"
     CC="NA"
     matrix_clause = "NA"
     predicate="NA"
     


    //for the MCs and true/false items
    if (type == "MC" | type == "itIsT_F" ){
        stim=selectedContentsListAsVar["additionalStims"][type][i]
        topic = stim.split("-")[0]
        orientation = stim.split("-")[1]
        CC = contentsOnly[topic][orientation]

      }

      //for the objective items
    else if (type =="O"){
      stim=selectedContentsListAsVar["additionalStims"][type][i]
      topic = stim.split("-")[0]
      CC = contentsOnly[topic][type]
    }

    //for the critical items
    else {
          stim=selectedContentsListAsVar[type][i]
          topic = stim.split("-")[0]
          orientation = stim.split("-")[1]
          CC = contentsOnly[topic][orientation]
          predicate=selectedPredicate
          VP = VPs[predicate] 
          matrixName = matrixNameMapping[topic]
          matrix_clause = matrixName + " " + VP
          type="critical"
        }


    if (stim.split("-")[1] == "R") {
      orientation="conservative"
    }
    else if (stim.split("-")[1] == "L"){
      orientation="liberal"
    }

    if (type !="O") {
      speakerInfo= selectedSpeaker
      file=speakerInfo
      speakerInfo=speakerInfo.split("-")
      name=speakerInfo[2]
      gender=speakerInfo[0]
      socialInfo=speakerInfo[1]
    }

    correctState = locations[name][1]
    incorrectState=incorrectStates.pop()
    
    return {
	  "topic": topic,
	  "orientation": orientation,
    "CC":CC,
    "stimType":type,
    "name":name,
    "gender":gender,
    "genderMaxes":speakerGendersMaxes,
    "socialInfo":socialInfo,
    "correctState":correctState,
    "incorrectState":incorrectState,
    "file":file,
    "list":selectedContentsListAsString,
    "predicate":predicate,
    "matrix_clause":matrix_clause,
    }
  }


exp.stims_block1 = [];
exp.stims_block2 = [];
exp.stims_block3 = [];


// items is a shuffled array, with each trigger once
// this for loop runs calls makeStim as often as their are triggers
// and adds the newly created stim to the the set of stims for block 1

console.log("selectedContentsList",selectedContentsListAsString)
selectedPredicates = _.sample(predicatesOnly, 6)//choose 6 predicates randomly
 if (speakerGendersMaxes == "1"){
      genderMaxes_conservative = [2,1,1,2] //male Democrats, female democrats, male republicans, female republicans
      genderMaxes_liberal = [1,2,2,1]
      genderMaxes_MC = [1,1,0,1]
      genderMaxes_TF=[0,1,1,0]
      
    }
    else if (speakerGendersMaxes == "2"){
      genderMaxes_conservative = [1,2,2,1]
      genderMaxes_liberal = [2,1,1,2] 
      genderMaxes_MC = [1,1,1,0]  
      genderMaxes_TF=[0,1,0,1]
    }  
  genderMaxes_neutral =  [1,2,2,1]

  genderCounts = [0,0,0,0]
  for (var i=0; i<selectedContentsListAsVar["conservative"].length; i++) {
    genderCountsAndSpeakers =  getSpeaker(genderCounts,genderMaxes_conservative,i)
    genderCounts =genderCountsAndSpeakers[0]
    selectedSpeaker=genderCountsAndSpeakers[1]
    var stim = makeStim(i,"conservative",selectedSpeaker,selectedPredicates[i]);
    exp.stims_block1.push(jQuery.extend(true, {}, stim));
    exp.stims_block2.push(jQuery.extend(true, {}, stim));
    exp.stims_block3.push(jQuery.extend(true, {}, stim));
  }
  
  genderCounts = [0,0,0,0]
  for (var i=0; i<selectedContentsListAsVar["liberal"].length; i++) {
    genderCountsAndSpeakers =  getSpeaker(genderCounts,genderMaxes_liberal,i)
    genderCounts =genderCountsAndSpeakers[0]
    selectedSpeakers=genderCountsAndSpeakers[1]
    var stim = makeStim(i,"liberal",selectedSpeaker,selectedPredicates[i]);
    exp.stims_block1.push(jQuery.extend(true, {}, stim));
    exp.stims_block2.push(jQuery.extend(true, {}, stim));
    exp.stims_block3.push(jQuery.extend(true, {}, stim));
  }

  genderCounts = [0,0,0,0]
  for (var i=0; i<selectedContentsListAsVar["neutral"].length; i++){
    genderCountsAndSpeakers =  getSpeaker(genderCounts,genderMaxes_neutral,i)

    genderCounts =genderCountsAndSpeakers[0]
    genderCounts =genderCountsAndSpeakers[0]
    selectedSpeaker=genderCountsAndSpeakers[1]
    var stim = makeStim(i,"neutral",selectedSpeaker,selectedPredicates[i]);
    exp.stims_block3.push(jQuery.extend(true, {}, stim)); 
  }

for (let k in selectedContentsListAsVar["additionalStims"]){
    type=k
    topics = selectedContentsListAsVar["additionalStims"][type]
    genderCounts =[0,0,0,0]
    selectedSpeaker=""
    genderMaxes = []
    addToBlocks = [1,0,0] //block1, block2, block3 - default for objective 

    for (var i=0; i<topics.length; i++){
      topic = topics[i]
      console.log("topic!!",topic)
      orientation = topic.split("-")[1]
      console.log("orientation!!",orientation)
      if (type =="MC"){
          genderMaxes = genderMaxes_MC

          if (orientation!="N"){
            addToBlocks = [1,1,1]
          }
          else if (orientation=="N"){
            addToBlocks = [0,0,1]
          }
        }
      else if (type=="itIsT_F"){
        genderMaxes = genderMaxes_TF
        addToBlocks = [0,1,1]
      }
      if (type !="O"){
          genderCountsAndSpeakers =  getAdditionalSpeaker(genderCounts,genderMaxes,i,type)
          selectedSpeaker=genderCountsAndSpeakers
        }

      console.log("selectedSpeaker here",selectedSpeaker)
      var stim = makeStim(i,type,selectedSpeaker,"NA");
        
      if (addToBlocks[0] == 1){
          exp.stims_block1.push(stim)
        }
      if (addToBlocks[1]==1){
          exp.stims_block2.push(stim)
        }
      if (addToBlocks[2]==1){
          exp.stims_block3.push( stim)
          }
    }
}
  console.log("block1",exp.stims_block1)
  console.log("block2",exp.stims_block2)
  console.log("block3",exp.stims_block3)

  exp.stims_block1 = _.shuffle(exp.stims_block1);  
  exp.stims_block2 = _.shuffle(exp.stims_block2); 
  console.log("block2 shuffled", exp.stims_block2) 
  exp.stims_block3 = _.shuffle(exp.stims_block3);  

  for (var i=0; i<exp.stims_block2.length; i++){
    exp.stims_block2[i]["stateType"]="NA"
    if (i%2 == 0){
      exp.stims_block2[i]["stateType"]="CORRECT"
      }
    else {
      exp.stims_block2[i]["stateType"]="INCORRECT"
      }
    }
    console.log("the block 2 stim", exp.stims_block2[i])
  console.log("stims with states", exp.stims_block2)

    //final shuffle
    exp.stims_block2 = _.shuffle(exp.stims_block2); 
    console.log("final shuffle",exp.stims_block2)


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
  exp.structure=["botcaptcha", "instructions", "instructions1","block1","instructions2","block2","instructions3","block3",'questionaire', 'finished'];
  //exp.structure=['block3','instructions2','block3','questionaire', 'finished'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 3 + 16 + 1 + 16 + 1 + 24 ; 
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