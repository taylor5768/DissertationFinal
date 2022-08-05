var contentsOnly = {
"abortion": {
  "L":"it is too hard to get an abortion",
  "R":"it is too easy to get an abortion"
    },

"medicaid": {
  "L":"medicaid is underfunded",
  "R":"medicaid is overfunded"
    },


"minimum_wage": {
  "L":"the minimum wage is too low",
  "R":"the minimum wage is too high"
    },

  "2020_election": {
    "L":"the 2020 election was legitimate",
    "R":"the 2020 election was rigged"
    },

  "COVID": {
    "L":"COVID is a dangerous virus",
    "R":"COVID is an overhyped virus"
    },


  "gun_laws": {
    "L":"it is too easy to buy guns",
    "R":"it is too difficult to buy guns"
    },

"Trump_pandemic": {
  "L":"Trump handled the pandemic badly",
  "R":"Trump handled the pandemic well"
    },

"military": {
  "L":"the military is overfunded",
  "R":"the military is underfunded"
    },

  "immigrants": {
    "L":"immigrants support the economy",
    "R":"immigrants steal jobs from Americans"
    },

  "capitol_uprising": {
    "L":"the uprising at the Capitol was domestic terrorism",
    "R":"the uprising at the Capitol was patriotic"
    },

  "masks": {
    "L":"mask mandates are necessary",
    "R":"mask mandates are unnecessary"
    },

  "Trump_economy": {
    "L":"Trump's policies damaged the American economy",
    "R":"Trump's policies improved the American economy"
    },

//neutrals
    "late":{
      "N":"the event started late"
    },
    "fundraiser":{
      "N":"the fundraiser was postponed"
    },
    "participants":{
      "N":"there are fewer participants this week"
    },
    "next_event":{
      "N":"the next event was rescheduled"
    },
  "registration":{
      "N":"the registration due date is next week"
    },
  "minutes":{
      "N":"the previous meeting minutes were sent"
    },

  "break":{
    "N":"the break will be shortened today"
  },
  "newsletter":{
    "N":"last month's newsletter was sent late"
  },

  "social_media":{
    "N":"the event was advertised on social media"
  },

  "contact":{
    "N":"the organizers' contact info is outdated"
  },
  "organized":{
    "N":"these events are well-organized"
  },
  "agenda":{
    "N":"the agenda is longer than usual today"
  },
  //objectively true/false
  "capital": {
    "O":"the capital of the US is in Ohio"
  },
  "elections": {
    "O":"U.S. presidential elections are held in November"
  },
  //main clauses
  "taxes":{
    "L":"the rich are taxed too little",
    "R":"the rich are taxed too much",
  },

"lockdown":{
    "L":"closing businesses during COVID was necessary",
    "R":"closing businesses during COVID was unnecessary"
    },
//neutral MCs
"website":{
  "N":"the event website is working now"
},
"8pm":{
  "N":"the next meeting will start at 8pm"
  },

//it is true/it is false stims
"welfare":{
  "L":"there are too many restrictions on welfare benefits",
  "R":"there are too few restrictions on welfare benefits"

},
"environment":{
  "L":"there are too few environmental laws",
  "R":"there are too many environmental laws"
}

}


//var matrixSubjNames = _.shuffle(["Mary","Danny","Emma","Jackson","Olivia","Tony","Zoe","Josh","Grace","Owen","Mia","Jon","Liam","Noah","Ethan","Nathan","Miles","Allison","Amelia","Chloe","Leah","Clara"])

var VPs ={     
    "be_annoyed":"isn't annoyed that",
     "know":"doesn't know that",
     "see":"doesn't see that",
     "pretend":"didn't pretend that",
     "suggest":"didn't suggest that",
     "say":"didn't say that",
     "think":"doesn't think that",
     "be_right":"isn't right that",
     "acknowledge":"didn't acknowledge",
     "admit":"didn't admit that",
     "confess":"didn't confess that",
     "hear":"didn't hear that"
    

}

//randomization for predicates is in experiment.js
predicatesOnly = ["be_annoyed","know","see","pretend","suggest","say","think","be_right","acknowledge","admit","confess","hear"]



maleDemocrats = _.shuffle([
//males - 4
"M-N-Daniel","M-N-Tim","M-N-John","M-N-James"])

femaleDemocrats =_.shuffle([
//females - 5
"F-N-Helen","F-N-Elizabeth","F-N-Lisa","F-N-Anna","F-N-Michelle",
])


maleRepublicans = _.shuffle([
//males - 5
"M-S-Joseph","M-S-Ryan","M-S-Anthony","M-S-Jason","M-S-Michael"
])
,

femaleRepublicans = _.shuffle([
  //females - 4
"F-S-Sarah","F-S-Donna","F-S-Amy","F-S-Kimberly"])


additionalDemocratSpeakers = _.shuffle(["F-N-Veronica","F-N-Rachel","M-N-Jacob"])
additionalRepublicanSpeakers = _.shuffle(["M-S-Eric","F-S-Lily","M-S-Jay"])

additionalSpeakers = _.shuffle(["F-N-Veronica","F-N-Rachel","M-S-Eric","F-S-Lily","M-N-Jacob","M-S-Jay"])


locations = {
"Daniel":["Seattle","Washington"],
"Tim":["San Diego","California"],
"John":["Riverside","California"],
"James":["New York","New York"],
"Helen":["San Francisco","California"],
"Elizabeth":["Denver","Colorado"],
"Lisa":["Portland","Oregon"],
"Anna":["Baltimore","Maryland"],
"Michelle":["Philadelphia","Pennsylvania"],
"Joseph":["Gridley","Mississippi"],
"Ryan":["Madding","Arkansas"],
"Anthony":["Sneedsville","Tennessee"],
"Jason":["Gabbard","Kentucky"],
"Michael":["Bolen","Georgia"],
"Sarah":["Sherburne","Kentucky"],
"Donna":["Kinneys","Tennessee"],
"Amy":["Ozan","Arkansas"],
"Kimberly":["Hackleburg","Alabama"],
"Veronica":["Chicago","Illinois"],
"Rachel":["Detroit","Michigan"],
"Eric":["Dodson","Louisiana"],
"Lily":["Alva","Missouri"],
"Jacob":["Boston","Massachusetts"],
"Jay":["Ogburn","Virginia"],
"NA":["NA","NA"],
}

incorrectStates = _.shuffle([
"North Dakota","South Dakota","Utah","Florida","Rhode Island",
"Delaware","Hawaii","Alaska","Conneticut","Idaho","Illinois",
"Kansas","Maine","Minnesota","Montana","Nebraska","Nevada",
"New Hampshire","New Jersey","New Mexico","Ohio","Vermont","West Virginia","Wisconsin","Wyoming"
])



/*when this goes in order  (i.e., no randomization for speakers, for Order2, it looks like this:
conservative  1,2,2,1
0: ['M-D-James']
1: (2) ['F-D-Michelle', 'F-D-Anna']
2: (2) ['M-R-Michael', 'M-R-Jason']
3: ['F-R-Kimberly']


liberal 2,1,1,2
0: (2) ['M-D-John', 'M-D-Tim']
1: ['F-D-Lisa']
2: ['M-R-Anthony']
3: (2) ['F-R-Amy', 'F-R-Donna']

neutral 1,2,2,1
0: ['M-D-Daniel']
1: (2) ['F-D-Elizabeth', 'F-D-Helen']
2: (2) ['M-R-Ryan', 'M-R-Joseph']
3: ['F-R-Sarah']

don't use: F-D-Emily, F-D-Jessica, M-R-Andrew, F-R-Ashley - these are for attention checks*/




contents_list1 = {
"conservative":
	["abortion-R", 
	"immigrants-R",
	"2020_election-R",
	"gun_laws-R",
  "medicaid-R", 
  "minimum_wage-R",
	],

"liberal":
  ["COVID-L",
	"Trump_pandemic-L",
  "military-L",
  "capitol_uprising-L",
  "masks-L",
  "Trump_economy-L"
	],

"neutral":
  ["late-N",
  "fundraiser-N",
  "participants-N",
  "next_event-N",
  "registration-N",
  "minutes-N"
  ],

  "additionalStims":{
    "O":["capital","elections"],
    "itIsT_F":["welfare-R","environment-R"],
    "MC":["taxes-L","lockdown-L","website-N","8pm-N"],
    },
}

contents_list2 = {
"conservative":
	["COVID-R",
  "Trump_pandemic-R",
  "military-R",
  "capitol_uprising-R",
  "masks-R",
  "Trump_economy-R",
	],

"liberal":
	["abortion-L", 
  "immigrants-L",
  "2020_election-L",
  "gun_laws-L",
  "medicaid-L", 
  "minimum_wage-L",
	],
"neutral":
  ["break-N",
  "newsletter-N",
  "social_media-N",
  "contact-N",
  "organized-N",
  "agenda-N"],


"additionalStims":{
    "O":["capital","elections"],
    //"MC_neutral":["8pm"],
    "itIsT_F":["welfare-L","environment-L"],
     "MC":["taxes-R","lockdown-R","8pm-N","website-N"],
  },
}

//mapping to matrix_subj names
matrixNameMapping = {
  "COVID":"Mary",
  "Trump_pandemic":"Danny",
  "military":"Emma",
  "capitol_uprising":"Glenn",
  "masks":"Wendy",
  "Trump_economy":"Tony",
  "abortion":"Grace", 
  "immigrants":"Ben",
  "2020_election":"Jen",
  "gun_laws":"Owen",
  "medicaid":"Allison", 
  "minimum_wage":"Ken",

  //neutrals - use these names twice
  "late":"Amelia",
  "fundraiser":"Liam",
  "participants":"Chloe",
  "next_event":"Noah",
  "registration":"Leah",
  "minutes":"Ethan",
  "break":"Amelia",
  "newsletter":"Liam",
  "social_media":"Chloe",
  "contact":"Noah",
  "organized":"Leah",
  "agenda":"Ethan"
}


//determines how gender is distributed.  If M is first, then the
//conservative contents will have 2 male dem/1 female dem & 1 male Rep/2 female Rep.
//The liberal contents will have the reverse: 1 male dem/2 female dem & 2 male Rep/1 female Rep 
//If F is first, then all of this is switched, so conservative contents will have 1 male dem/2 female dem & 2 male Rep/1 female Rep, and so on for the liberal contents

speakerGendersMaxes = _.shuffle(["1","2"])[0]
//speakerGendersMaxes = ["2"][0]

speakerGendersOrder = _.shuffle(["M","F"])
//speakerGendersOrder = ["F","M"]


//speakerPoliticsOrder = _.shuffle(["NS","S"])
speakerPoliticsOrder = ["S","NS"]

//speakerPoliticsOrder = ["D","R"]

console.log("speakerGendersMaxes",speakerGendersMaxes)
console.log("speaker Genders Order", speakerGendersOrder)
console.log("speaker Politics Order", speakerPoliticsOrder)


//var contentslistsAsStrings = ["list1","list2"]
var contentslistsAsStrings = ["list2"]
var contentslistsAsVars = {"list1":contents_list1,"list2":contents_list2}

selectedContentsListAsString = _.shuffle(contentslistsAsStrings)[0]
selectedContentsListAsVar = contentslistsAsVars[selectedContentsListAsString]