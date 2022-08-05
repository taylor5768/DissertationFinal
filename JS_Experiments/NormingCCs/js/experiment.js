function make_slides(f) {
  var   slides = {};

  slides.botcaptcha = slide({
      name : "botcaptcha",
      start: function() {

      // define possible speaker and listener names
      // fun fact: 10 most popular names for boys and girls
      var speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      var listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];

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
      //"block" : "block1",
      //"question_type" : this.stim.block,      
      "slide_number_in_experiment" : exp.phase,
      //"short_trigger": this.stim.short_trigger,
      //"trigger": this.stim.trigger,
      "topic": this.stim.topic,
      "CC": this.stim.CC,
      "list": this.stim.list,
      "socialCondition": this.stim.socialCondition,
      "whichSocConditionFirst": this.stim.whichSocConditionFirst,
      "list_firstSocCond": this.stim.list_firstSocCond,
      "orientation": this.stim.orientation,
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
      var inst1 = "";
      inst1 = inst1 + "We'll ask you how likely a Democrat or Republican would be to believe certain statements."
 

    	$("#inst1").html(inst1);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    //start : function() {
    //  $(".err").hide();
    //},
    start : function() {
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
      //console.log(this.stim);  
      /// 
      var utterance = "";
      //if (this.stim.block == "prior") {
	  		//utterance = "How much do you agree with the following statement?" +"<br>";
        

	  /*} else if (this.stim.block == "projective")
        {
	  		utterance = "<strong> Fact (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""	  	
	  	}
      else 
        {
        utterance = "<strong> SOCIAL EVAL (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""      
      }*/
	  $(".sentence").html(utterance);
	  ///
	  var leftLabel = "";
	 // if (this.stim.block == "prior") {
	  		leftLabel = "very unlikely";
	  //} else {
	  //		leftLabel = "no"}
	  $(".leftLabel").html(leftLabel);
	  var rightLabel = "";
	  //if (this.stim.block == "prior") {
	  		rightLabel = "very likely";
	  //} else {
	 // 		rightLabel = "yes"}
	  $(".rightLabel").html(rightLabel);
	  ///
	  var question = "";
	  //console.log(this.stim.block);
	  if (this.stim.socialCondition == "D") {
	  //		question = this.stim.question +".";
        utterance = "How likely is a Democrat to believe that " + this.stim.CC + "?";
	  } else {
	  //		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	 
        utterance = "How likely is a Republican to believe that " + this.stim.CC + "?";
 	
	  	}
    $(".question").html(utterance);    
	 // $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
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
      //"block" : "block1",
      //"question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  //"short_trigger": this.stim.short_trigger,
   	  //"trigger": this.stim.trigger,
   	  "topic": this.stim.topic,
      "CC": this.stim.CC,
      "list": this.stim.list,
      "socialCondition": this.stim.socialCondition,
      "whichSocConditionFirst": this.stim.whichSocConditionFirst,
      "list_firstSocCond": this.stim.list_firstSocCond,
      "orientation": this.stim.orientation,
   	  "trigger_class": this.stim.trigger_class,
   	  //"prior" : this.stim.prior,
	     //"prior_fact" : this.stim.prior_fact,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  

/*
 slides.instructions2 = slide({
    name : "instructions2",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));     
//      console.log(block_order);
      var inst2 = "";
      if (exp.stims_block2[0].block == "democrat") {
          
          inst2 = inst2 + "Now, we'll ask you how likely a Democrat would be to believe certain statements."
      } else{
          inst2 = inst2 + "Now, we'll ask you how likely a Republican would be to believe certain statements."        
        };

      $("#inst2").html(inst2);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
      }
    });*/



 /*slides.block2 = slide({
    name : "block2",
    present : exp.stims_block2,
    //start : function() {
    //  $(".err").hide();
    //},
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));           
      $(".err").hide();
    },
    present_handle : function(stim) {
      $('html,body').scrollTop(0);
      this.stim = stim;
      this.stim.trial_start = Date.now();      
        $(".err").hide();     
    this.init_sliders();
      exp.sliderPost = null;   
      //console.log(this.stim);  
      /// 
      var utterance = "";
      //if (this.stim.block == "prior") {
        //utterance = "How much do you agree with the following statement?" +"<br>";*/
        

    /*} else if (this.stim.block == "projective")
        {
        utterance = "<strong> Fact (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""      
      }
      else 
        {
        utterance = "<strong> SOCIAL EVAL (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""      
      }*/ 
    /*$(".sentence").html(utterance);
    ///
    var leftLabel = "";
    //if (this.stim.block == "prior") {
        leftLabel = "very unlikely";
    //} else {
   //     leftLabel = "no"}
    $(".leftLabel").html(leftLabel);
    var rightLabel = "";
    //if (this.stim.block == "prior") {
        rightLabel = "very likely";
    //} else {
    //   rightLabel = "yes"}
    $(".rightLabel").html(rightLabel);
    ///
    var question = "";
    //console.log(this.stim.block);
    if (this.stim.block == "democrat") {
    //    question = this.stim.question +".";
        utterance = "How likely is a Democrat to believe that " + this.stim.question + "?";
    } else {
    //    question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";   
        utterance = "How likely is a Republican to believe that " + this.stim.question + "?";
  
      }
    $(".question").html(utterance);    
   // $(".question").html(question);    
    },

    button : function() {
      console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider2", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block2",
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
  }); */

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



// get trigger contents
// getContent is called for a particular trigger (e.g., "stop")
// it then gets an array of contents for that trigger and removes the first element and
// returns that first element (shift)
// then it loops through all arrays in items_content_mapping, finds the index of the
// content that the trigger was just paired with and removes it from that array
// so that no other trigger can be paired with it
  function getContent(trigger) {
//      console.log("items_content_mapping before throwing out "+trigger);
      //console.log("items_content_mapping", items_content_mapping);
//      for (var j in items_content_mapping) {    
//      console.log("items_content_mapping at "+j);       
//      console.log(items_content_mapping[j]);      
//      }     
//      console.log("items_content_mapping at the trigger before shuffling");
//      console.log(items_content_mapping[trigger]);      
      items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
 //     console.log("items_content_mapping at the trigger after shuffling");
 //     console.log(items_content_mapping[trigger]);            
//      console.log("items_content_mapping after shuffling "+trigger);
//      console.log(items_content_mapping);
      var content = items_content_mapping[trigger].shift();//items_content_mapping[trigger][0];
 //      console.log("this is the selected content: " + content);
//    var index = items_content_mapping[trigger].indexOf(content);      
//      items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//      console.log("items_content_mapping at the trigger after throwing it out");
//      console.log(items_content_mapping[trigger]);            
      for (var j in items_content_mapping) {
      var index = items_content_mapping[j].indexOf(content);  
      // console.log("the next three lines: the array before removal, the index of content, the array after removal")
      // console.log(items_content_mapping[j]);
      // console.log(index);    
      if (index != -1)
      {             
        items_content_mapping[j].splice(index,1);     
      }
      // console.log(items_content_mapping[j]);     
      }
//      console.log("items_content_mapping after throwing out "+trigger);
//      console.log(items_content_mapping);
//      for (var j in items_content_mapping) {    
//      console.log("items_content_mapping at "+j);       
//      console.log(items_content_mapping[j]);      
//      }             

      return content;
    }
      




// assign contents to triggers
  var trigger_contents = {
    "be_annoyed": getContent("be_annoyed"),       
    "know": getContent("know"),       
    "see": getContent("see"),
    "pretend": getContent("pretend"),
    "suggest": getContent("suggest"),   
    "say": getContent("say"),   
    "think": getContent("think"),
    "be_right": getContent("be_right"),
    "acknowledge": getContent("acknowledge"),
    "admit": getContent("admit"),
    "confess": getContent("confess"),
    "hear": getContent("hear"),

    //"MC1": getContent("MC"),
    //"MC2": getContent("MC"),    
//    "MC3": getContent("MC"),
//    "MC4": getContent("MC"),
//    "MC5": getContent("MC")
    };


  function makeStim(i) {
    //console.log("names",names)
    //console.log(items)
    //var item = items[i];
   // console.log("item",item)
    //console.log("thelist", thelist);

  //get a speaker
    //var name_data = names[i];
   // var context=contexts[0]
    //console.log("name_data",name_data);
     //console.log(name_data);
    //var name = name_data.name;
    //var gender = name_data.gender;


    // get content


    //console.log("trigger_contents!!",trigger_contents)
    //console.log("item",item)
    //var trigger_cont = trigger_contents[item.trigger];
    //console.log("trigger_cont", trigger_cont)
    //var trigger = item.trigger;
    //console.log("trigger",trigger)
    //VP = predicates[trigger];
    //var short_trigger = trigger;
    //if (trigger.indexOf("MC") != -1) {
    //  short_trigger = "MC";
    //  } 
  
    //console.log(trigger)
    topic_orientation = thelist[i]
    //console.log("topic_orentation",topic_orientation)
    //topic = trigger_contents[trigger]
    //console.log(topic)


    //topicR = topic + "-R";
    //topicL = topic+"-L";

    //console.log("TO_split", topic_orientation.split('-')[1])
    topic = topic_orientation.split('-')[0]

    if (topic_orientation.split('-')[1] == "R")
      {hasTopicR = true}
    else if (topic_orientation.split('-')[1] == "L") {
      hasTopicR = false
    }

    //console.log(hasTopicR);

    //console.log("contentsOnly",contentsOnly);

    if (hasTopicR == true){
      CC = contentsOnly[topic].R
      orientation = "conservative"
    }
    else if (hasTopicR == false){
      CC = contentsOnly[topic].L
      orientation = "liberal"
     
    }
    //console.log("ORIENTATION",orientation)
    //console.log("CC",CC)
    //var question = CC;
    var question = CC;

    //console.log("question",question)
    //var utterance = VP  +" " + question;
    //console.log("context",context);
    return {
    //"name": name,
    //"gender": gender, 
    //"context": context, 
    //"profile_img": profile_img,
    //"trigger": item.trigger,
    //"short_trigger": short_trigger,   
    "trigger_class": "C",
      "topic": topic,
      //"utterance": utterance,
      "CC": question,
      "orientation":orientation,
    }
  }
  
  function makeMCStim(ind,j) {
    //get item
    var item = mcitems[j];
  //get a speaker
    // var name_data = names[ind];
    //console.log("MC names");
    //var name_data = names.pop(); //has two fields: name and gender
    //console.log("name data", name_data); 
    //var name = name_data.name; 
    //var gender = name_data.gender;
    //var profile_img = gender + "D-" + name +".png";
    //console.log(profile_img);
    // get content
    var trigger_cont = j;
    var trigger = "MC";
    var short_trigger = "MC";

//  console.log("short_trigger: "+short_trigger);
//  console.log("trigger: "+trigger);
//    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = mcitems[j].MCq;
    var question = mcitems[j].question;  
    //var prior_fact = mcitems[j].prior_fact; 
//    console.log(contents[trigger_cont]); 

    return {
    //"name": name,
    //"gender": gender,
    //"profile_img": profile_img,   
     // "trigger": trigger,
      "orientation":"none",
      //"short_trigger": short_trigger,   
      "trigger_class": "MC",
      "topic": trigger_cont,
      //"utterance": utterance,
      "CC": question,
    }
  }  

exp.stims_block1 =[];
exp.stims_neutralContents=[];

// items is a shuffled array, with each trigger once
// this for loop runs calls makeStim as often as their are triggers
// and adds the newly created stim to the the set of stims for block 1

//selectedList = _.shuffle(lists)[0]
//console.log("selectedListAsString:",selectedListAsString) //from stimuli.js
//console.log("selectedList",selectedListAsVar)
thelist = selectedListAsVar;

for (var i=0; i<selectedListAsVar.length; i++) {
  	var stim = makeStim(i);
  	exp.stims_block1.push(jQuery.extend(true, {}, stim));	
}

for (var j=0; j<mcitemnames.length; j++) {
    var stim = makeMCStim(j,mcitemnames[j]);
    exp.stims_block1.push(jQuery.extend(true, {}, stim));
  }  




socialConditions = _.shuffle(["D","R"])
//socialConditions = ["D","R"]
firstSocCond = socialConditions[0];
secondSocCond = socialConditions[1];
list_firstSocCond = selectedListAsString + "-" +firstSocCond;
console.log("list_firstSocCond",list_firstSocCond)
//console.log("which SC first:",firstSocCond);



for (var k=0; k<exp.stims_block1.length/2-1; k++){
//k = 0 to 10 , so 11 items
  if (exp.stims_block1[k].trigger_class != "MC"){
  exp.stims_block1[k].socialCondition = firstSocCond;
  exp.stims_block1[k].whichSocConditionFirst = firstSocCond;
  exp.stims_block1[k].list = selectedListAsString;
  exp.stims_block1[k].list_firstSocCond = list_firstSocCond;
  }
}


MC_count = 0
for (var k=exp.stims_block1.length/2-1; k<exp.stims_block1.length; k++){
  //k = 11 to k = 20, so 9 items
  if (exp.stims_block1[k].trigger_class != "MC"){

    exp.stims_block1[k].socialCondition = secondSocCond;
    exp.stims_block1[k].whichSocConditionFirst = firstSocCond
    exp.stims_block1[k].list = selectedListAsString;
    exp.stims_block1[k].list_firstSocCond = list_firstSocCond;

  }

  else if (MC_count ==0){
    exp.stims_block1[k].socialCondition = firstSocCond;
    MC_count =MC_count +1
  }

   else {
    exp.stims_block1[k].socialCondition = secondSocCond;
  }

  exp.stims_block1[k].whichSocConditionFirst = firstSocCond
  exp.stims_block1[k].list = selectedListAsString;
  exp.stims_block1[k].list_firstSocCond = list_firstSocCond;


}
//console.log("stims with condition", exp.stims_block1);


//console.log("before shuffling", exp.stims_block1)
exp.stims_block1 = _.shuffle(exp.stims_block1); 
//console.log("shuffled stims", exp.stims_block1);
// console.log(exp.stims_block1);  
	  


	exp.stims_block1 = _.shuffle(exp.stims_block1);  
	//exp.stims_block2 = _.shuffle(exp.stims_block2); 
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
  exp.structure=["botcaptcha", "instructions","instructions1", "block1",'questionaire', 'finished'];
  //exp.structure=['questionaire', 'finished'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 3 + 22 + 1; 
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