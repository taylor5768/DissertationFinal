library(rtf)

### List of substitutions to make for model TERMS
termSubs <- list()
#termSubs[["D"]] <- "Democrat"
#termSubs[["R"]] <- "Republican"
#termSubs[["N"]] <- "neutral"
#termSubs[["socialInfo"]] <- "Political Affiliation"
#termSubs[["wordClass"]] <- ""
termSubs[[":"]] <- " x "
termSubs[["predicateacknowledge"]] <- "acknowledge"
termSubs[["predicateadmit"]] <- "admit"
termSubs[["predicatebe_annoyed"]] <- "be annoyed"
termSubs[["predicatebe_right"]] <- "be right"
termSubs[["predicateconfess"]] <- "confess"
termSubs[["predicateknow"]] <- "know"
termSubs[["predicatepretend"]] <- "pretend"
termSubs[["predicatesay"]] <- "say"
termSubs[["predicatesee"]] <- "see"
termSubs[["predicatesuggest"]] <- "suggest"
termSubs[["predicateacknowledge"]] <- "acknowledge"
termSubs[["predicatehear"]] <- "hear"
termSubs[["orientationliberal"]] <- "liberal"
termSubs[["orientationconservative"]] <- "conservative"
termSubs[["socialInfoR"]] <- "Republican"
termSubs[["singleSocialInfoR"]] <- "Repub./Southern"
termSubs[["singleSocialInfoD"]] <- "Democ./Non-Southern"
termSubs[["socialInfoS"]] <- "Southern"
termSubs[["spBelief_rating"]] <- "perceived speaker belief rating"
termSubs[["participant_beliefs"]] <- "participant belief rating"
termSubs[["experimentExp1"]] <- "Exp1"
termSubs[["experimentExp2"]] <- "Exp2"
termSubs[["experimentExp3"]] <- "Exp3"



# Char. select models
#termSubs[["guise2trap"]] <- "guise=[a]"
#termSubs[["itemNumber_scaled"]] <- "item number"

### List of substitutions to make for model HEADERS
headerSubs <- list()
#headerSubs[["socialInfo"]] <- "Political Affiliation"
headerSubs[["socialInfo"]] <- "Accent"
headerSubs[["singleSocialInfo"]] <- "Social Info"
headerSubs[["orientation"]] <- "CC orientation"
headerSubs[["experiment"]] <- "Experiment"
headerSubs[["predicate"]] <- "Predicate"
headerSubs[["spBelief_rating"]] <- "Perceived speaker belief rating"
headerSubs[["participant_beliefs"]] <- "Participant belief rating"
#headerSubs[["syll"]] <- "Num. Syllables"
#headerSubs[["order_scaled"]] <- "Item Number"
#headerSubs[["prob_score_scaled"]] <- "Phonotactic Probability"
#headerSubs[["roundedFreq_scaled"]] <- "Lexical Frequency"
#headerSubs[["wordClass"]] <- "Word Class"
# Social models
#headerSubs[["talker"]] <- "Talker"
# Char. select models
#headerSubs[["guise2"]] <- "Guise"
#headerSubs[["education_scaled"]] <- "Listener Education Level"

getStars <- function(pValue) {
  stars <- '~~~'
  if (pValue < 0.05) {stars <- '*~~'}
  if (pValue < 0.01) {stars <- '**~'}
  if (pValue < 0.001) {stars <- '***'}
  return(stars)
}

saveModelAsRTF <- function(model, resFile, docHeight, verbose=FALSE, alpha=0.05, spacingChar='...', addCallInfo=FALSE) {
  mySummary <- summary(model)
  myRes <- coef(mySummary)
  myCall <- mySummary$call
  myRes <- as.data.frame(myRes)
  
  newRes <- cbind(rownames(myRes), round(myRes,3))
  colnames(newRes)[[1]] <- "Effect"
  newRes2 <- as.data.frame(newRes, stringsAsFactors = FALSE)
  # If z-value instead of p-value, temporarily rename column
  hasZValue <- FALSE
  if ("Pr(>|z|)" %in% colnames(newRes2)) {
    hasZValue <- TRUE
    newRes2$`Pr(>|t|)` <- newRes2$`Pr(>|z|)`
    newRes2$`Pr(>|z|)` <- NULL
  }
  
  # Bold significant effects
  newRes2$startFormatting <- ''
  newRes2$endFormatting <- ''
  newRes2[newRes2$`Pr(>|t|)` <= alpha,]$startFormatting <- '<b>'
  newRes2[newRes2$`Pr(>|t|)` <= alpha,]$endFormatting <- '</b>'
  
  # Add stars to label significance
  newRes2$stars <- sapply(newRes2$`Pr(>|t|)`, getStars)
  newRes2$stars <- gsub('~', ' ', newRes2$stars)
   #Relabel p-values of '0' as <.001
  if (0 %in% unique(newRes2$`Pr(>|t|)`)) {newRes2[newRes2$`Pr(>|t|)` == 0,]$`Pr(>|t|)` <- '<.001'}
  newRes2$`Pr(>|t|)` <- paste(newRes2$`Pr(>|t|)`, newRes2$stars)
  newRes2$stars <- NULL
  
  myTerms <- strsplit(toString(myCall), '\\~')[[1]][2]
  #print("initialMyTerms")
  #print(myTerms)
  myTerms <- strsplit(myTerms, '\\(')[[1]][1]
  allMyTerms <- strsplit(myTerms, ' ')[[1]]
  allMyTerms <- allMyTerms[!allMyTerms %in% c('+', '', '*')]
  allMyTerms <- unique(allMyTerms)
  
  rowBreaks <- c() # where do new effects start
  headerNames <- c()
  #minHeaderRow <- 2
  for (rowNum in seq(2,nrow(newRes2))) {
    thisRow <- newRes2[rowNum,]$Effect
    #print("thisRow")
    #print(thisRow)
    prevRow <- newRes2[rowNum-1,]$Effect
    thisRowTerms <- c()
    prevRowTerms <- c()
    #print(allMyTerms)
    for (term in allMyTerms) {
      
      #print(term)
      if (grepl(term, thisRow)==TRUE) {
        thisRowTerms <- c(thisRowTerms, term)}
      if (grepl(term, prevRow)==TRUE) {prevRowTerms <- c(prevRowTerms, term)}
    }
    if (identical(thisRowTerms, prevRowTerms)==FALSE) {
      rowBreaks <- c(rowBreaks, rowNum)
      headerNames <- c(headerNames, paste('\n', paste(thisRowTerms, collapse=' x '), sep=''))
    }
  }
  
  ### Find out how many items under each thing
  gaps <- c()
  if (length(rowBreaks) > 1) {
    for (n in seq(length(rowBreaks)-1)) {
      gaps <- c(gaps, rowBreaks[n+1] - rowBreaks[n])
    }
    gaps <- c(gaps, nrow(newRes2) + 1 - rowBreaks[length(rowBreaks)])
  } else {gaps <- c(1)}
  
  
  if (verbose==TRUE) {
    print('original header names')
    print(headerNames)}
  
  ### Prettify header names
  for (subs in names(headerSubs)) {
    headerNames <- gsub(subs, headerSubs[[subs]], headerNames)
  }
  
  if (verbose==TRUE) {
    print('pretty-ified header names')
    print(headerNames)}
  
  ### Prettify term names
  for (subs in names(termSubs)) {
    newRes2$Effect <- gsub(subs, termSubs[[subs]], newRes2$Effect)
  }
  
  
  ### First row = intercept
  resDF <- newRes2[1,] # intercept
  #if (rowBreaks[1] != 2) {resDF <- rbind(newRes2[1:(rowBreaks[1]-1),])}
  print(length(rowBreaks))
  prevHeader <- FALSE
  for (i in seq(1,length(rowBreaks))) {
    if (i!=length(rowBreaks)) {rowsToAdd <- newRes2[rowBreaks[i]:(rowBreaks[i+1]-1),]}
    else {rowsToAdd <- newRes2[rowBreaks[i]:nrow(newRes2),]}
    
    includeHeader <- gaps[i] >= 1
    if (includeHeader == TRUE) {
      headerRow <- c(headerNames[i], rep('', ncol(newRes2)-1))
      # Italicize header row
      headerRow[1] <- paste('<i>', headerRow[1])
      headerRow[length(headerRow)] <- '</i>'
      resDF <- rbind(resDF, headerRow)
      prevHeader <- TRUE
      # Add '...' or other spacing character to front of levels
      rowsToAdd$Effect <- paste(spacingChar, rowsToAdd$Effect, sep='')
    }
    else {
      if (prevHeader==TRUE) {
        # Add a blank line
        resDF <- rbind(resDF, rep('', ncol(newRes2)))
      }
      prevHeader <- FALSE
    } 
    resDF <- rbind(resDF, rowsToAdd)
    #if (i!=length(rowBreaks)) {resDF <- rbind(resDF, newRes2[rowBreaks[i]:(rowBreaks[i+1]-1),])}
    #else {resDF <- rbind(resDF, newRes2[rowBreaks[i]:nrow(newRes2),])}
  }
  rownames(resDF) <- NULL
  
  ### Collapse formatting marks
  resDF$Effect <- paste(resDF$startFormatting, resDF$Effect, sep='')
  resDF$`Pr(>|t|)` <- paste(resDF$`Pr(>|t|)`, resDF$endFormatting, sep='')
  resDF$startFormatting <- NULL
  resDF$endFormatting <- NULL
  
  if (hasZValue==TRUE) {
    newRes2$`Pr(>|z|)` <- newRes2$`Pr(>|t|)`
    newRes2$`Pr(>|t|)` <- NULL
  }
  
  if (verbose=="TRUE") {print('saving RTF file')}
  rtffile <- RTF(resFile, width=6, height=docHeight, omi=c(0,0,0,0), font.size=10)  # this can be an .rtf or a .doc
  if (addCallInfo==TRUE) {
    addParagraph(rtffile, paste('call:', as.character(myCall)[2]))
    addParagraph(rtffile, paste(mySummary$methTitle, collapse=' '))
  }
  addTable(rtffile, resDF, col.justify = 'L',col.widths=c(2,.75,.75,.75,.75,.75))
  done(rtffile)
  
  ### Now bold text
  x <- readLines(resFile)
  y <- gsub( "<b>", paste("\\\\b "), x )
  y <- gsub( "</b>", paste("\\\\b0 "), y )
  y <- gsub( "<i>", paste("\\\\i "), y )
  y <- gsub( "</i>", paste("\\\\i0 "), y )
  write(paste(y, sep=''), file=resFile, sep="\n")
}

# Example
#saveModelAsRTF(s1, 'model_summaries/rtf.doc')


getTailProp <- function(parameter, samples) {
  thisCol <- samples[,paste("b_", parameter, sep='')]
  thisMean <- mean(thisCol)
  if (thisMean > 0) {tailProp <- length(thisCol[thisCol < 0])/length(thisCol)}
  else {tailProp <- length(thisCol[thisCol > 0])/length(thisCol)}
  return(round(tailProp, 3))
}

