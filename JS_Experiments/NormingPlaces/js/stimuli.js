list1 = [
//males 
"M-S-Andrew", "M-N-David","M-S-Joseph","M-N-Tim","M-S-Ryan","M-N-William", "M-S-Michael","M-N-Scott","Party-M-R-Patrick","Party-M-D-Justin","Empty-M-S-Jeff","Empty-M-N-Eric",

//females
"F-S-Rachel","F-N-Emily","F-S-Sarah","F-N-Jessica","F-S-Donna","F-N-Lisa","F-S-Haley","F-N-Elizabeth","Party-F-R-Stephanie","Party-F-D-Amanda","Empty-F-S-Angela","Empty-F-N-Rebecca",
]


list2 = [
//males
"M-N-John","M-S-Jason","M-N-James","M-S-Anthony","M-N-Kenneth","M-S-Paul","M-N-Daniel","M-S-Christopher","Party-M-R-Patrick","Party-M-D-Justin","Empty-M-S-Jeff","Empty-M-N-Eric",
//females
"F-N-Jennifer","F-S-Kimberly","F-N-Helen","F-S-Amy","F-N-Anna","F-S-Ashley","F-N-Michelle","F-S-Emma","Party-F-R-Stephanie","Party-F-D-Amanda","Empty-F-S-Angela","Empty-F-N-Rebecca",
]

var listsAsStrings = ["list1","list2"]
//var listsAsStrings = ["list1"]
var listsAsVars = {"list1":list1,"list2":list2}

selectedListAsString = _.shuffle(listsAsStrings)[0]
selectedListAsVar = listsAsVars[selectedListAsString]

/*var mclistsAsVars = {"list1":mcitemnames_list1,"list2":mcitemnames_list2}

selectedMCListAsVar= mclistsAsVars[selectedListAsString]
console.log("selectedMCList", selectedMCListAsVar)*/