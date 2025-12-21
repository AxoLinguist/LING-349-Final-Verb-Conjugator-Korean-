#this code originally started with a symbol to hex function but
## I didn't end up needing it so...
# You get a comment instead!!
#but seriously, if you run this whole code, it will set up a lot of functions
##that the next code will use.
#we'll define some basic endings that we'll end up using later to save typing
koiipred <- '거야'
#ko is a stand in for 거
#ii stands for 'informal polite'
#pred means 'predicate'
koippred1 <- '것이에요'
koippred2 <- '거예요'
#two ways of ending in ip (informal polite) so there are two
koifppred <- '것입니다'
#just the one for fp (formal polite)
kort1b <- '아'
#korean type 1 informal impolite base
kort2b <- '어'
#korean type 2 informal impolite base
interconj <- '어'
#comes after a past tense
korpolend <- "요"
#korean polite ending. It's pretty easy to add to the impolite base
library(stringr)
#the only library we need
#now we'll create a loop until an actual korean predicate is entered
#we'll build our necessary functions first then build the loop to take in the info
predchecker <- function(posspred) {
  predcheck1 <- str_detect(posspred, "다")
  predcheck2 <- ifelse(str_length(posspred) >= 2, TRUE, FALSE)
  predcheck <- ifelse(predcheck1 == TRUE & predcheck2 == TRUE, TRUE, FALSE)
  return(predcheck)
}
#좋아, now our character is for sure a predicate
#this program won't differentiate between a verb and an adjective, but that's
## OK. we're not building full sentences so they're conjugated the same
#first, let's take the last jamo, since it's what decides the conjugation most of the time
scrimcheck <- function(proppred) {
  discriminant <- str_extract(proppred, ".[다]")
  #this name is a reference to the quadratic formula. i know i know please
  ## put away your pitchforks
  otherwheels <- sub(discriminant, "", proppred)
  #this will be what we tack onto the conjugation
  discriminant <- sub("다", "", discriminant)
  #now we've separated the discriminant.
  scrimchecked <- list(discriminant, otherwheels)
  return(scrimchecked)
}
#some verbs are immediately easy due to their verb ending. Let's check for them
## now
easymodecheck <- function(discrim) {
  check <- if (discrim[1] == "하") {
    TRUE
    } else if (discrim[1] == '이' & length(discrim[2]) > 0) {
      TRUE
    } else {
      FALSE
    }
  return(check)
}
#we made this as a function so that we can call it later
#we'll build the easy mode function here
easyconj <- function(easyhaha) {
  hada <- easyhaha$scrim[2]
  #takes the brokenjamo() result and conjugates it for '하다' verbs
  informalimpolite <- paste(hada, "해", sep = "")
  informalpolite <- paste(informalimpolite, korpolend, sep = "")
  formalpolite <- paste(hada, "합니다", sep = "")
  pst_base <- '했'
  ii_pst <- paste(hada, pst_base, "어", sep = "")
  #ii stand for 'informal impolite)
  ip_pst <- paste(ii_pst, "요", sep = "")
  #ip stands for 'informal polite'
  fp_pst <- paste(hada, pst_base, "습니다", sep = "")
  #fp stands for 'formal polite'
  fut_base <- paste(hada, '할', sep = "")
  ii_fut <- paste(fut_base, koiipred, sep = " ")
  ip_fut1 <- paste(fut_base, koippred1, sep = " ")
  ip_fut2 <- paste(fut_base, koippred2, sep = " ")
  fp_fut <- paste(fut_base, koifppred, sep = " ")
  conjdata <- list(ii_pst, ip_pst, fp_pst, informalimpolite, informalpolite, formalpolite, ii_fut, ip_fut1, ip_fut2, fp_fut)
  return (conjdata)
}
#While that's the easy case, a lot of Korean verbs end in 하다 'to do'
#However, copula verbs work differently than verbs ending with xㅣ다
#unfortunately, this won't be able to handle copulas where the noun
## ends with a vowel, since this will end up conjugating like a normal verb
## and it will take a long time to enter every single noun that ends in 
## a vowel for this conjugator to check
# Maybe someday when I have more time, but for now...
# This will just be an issue
copconj <- function(rescop) {
  #rescop stands for 'restricted copula,' since it won't conjugate
  ## copula with vowel-final nouns
  nounbase <- rescop$scrim[2]
  iip <- paste(nounbase, '였어', sep = "")
  #i decided to include assimilation since I find it occurs more in
  ## casual speech than formal speech
  ipp <- paste(nounbase, '이었어요', sep = "")
  fpp <- paste(nounbase, '이었습니다', sep = "")
  iipr <- paste(nounbase, '이야', sep = "")
  ippr <- paste(nounbase, '이에요', sep = "")
  fppr <- paste(nounbase, '입니다', sep = "")
  futbase <- paste(nounbase, '이', sep = "")
  iif <- paste(nounbase, '될', koiipred, sep = " ")
  #the reason it's the noun base and not the future base is because
  ## the particle can be omitted in casual speech
  ipf1 <- paste(futbase, '될', koippred1, sep = " ")
  ipf2 <- paste(futbase, '될', koippred2, sep = " ")
  #While 이 following the noun can sometimes be omitted in polite speech,
  ## I think it's more intuitive if it's there
  fpf <- paste(futbase, '될', koifppred, sep = " ")
  rescopconj <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(rescopconj)
}
#now we have a restricted copula conjugator. Let's deal with everything else
#let's deal with the other verbs that don't end with '하다' or are copula
jamobreak <- function(scrim) {
  jamo <- as.character(scrim[1])
  dcoded <- utf8ToInt(jamo)
  #takes our discriminant we determined earlier and breaks it down
  #basically, all jamo in utf8 are a combo of base letters
  #they have the basic equation AC00 + 588x + 28y + z
  #x is the first consonant
  #y is the vowel
  #z are the final vowels
  #0xAC00 just starts the Korean section of utf-8 encoding
  #so we can subtract it to get to the rest of the math
  #we can convert AC00 into decimal thanks to the hex to decimal calculator
  ##provided in the 'encoding' section of the class site
  #AC00 is 44032, which is the first Hangul jamo
  nowkorean <- dcoded - 44032
  #now we can decompose the character
  bachim <- nowkorean%%28
  #now we've isolated the final letter
  #in the next few you'll add one to the total, but because the final consonant
  ##(bachim) can be empty, it is valid to have a zero value
  bachimdetect <- ifelse(bachim == 0, FALSE, TRUE)
  #now we know if there's a bachim we have to deal with.
  moeum <- 1 + ((nowkorean-bachim)%%588)/28
  #moeum (모음) means vowel in Korean, so this will tell us which vowel we're
  ##dealing with. We'll split the conjugation into two types depending on the
  ##vowel
  #the reason you add 1 is that, if the vowel is the lowest value, then
  ## it becomes zero. However, the vowel values start at one, so
  ## you have to add one to the mod result.
  conjbase <- ifelse(moeum == 1 | moeum == 9 | moeum == 3 | moeum == 13, kort1b, kort2b)
  #this takes the number associated with ㅏ and ㅗ (and combinations involving them) and, if the moeum is equal
  ## to this, it's type one. Otherwise, type two.
  #type two is true, type one is false
  slightchange <- ifelse(moeum == 21, TRUE, FALSE)
  hardmode <- ifelse(moeum == 19, TRUE, FALSE)
  choseong <- 1 + (nowkorean/588)
  choseong <- floor(choseong)
  #We have to floor this value since it often won't return an integer, and 
  ##pure rounding causes an issue. Normally, we take the integer part, but
  ## floor function to get whatever was the leading integer.
  #now we return our findings
  eval <- list(bachim, bachimdetect, moeum, conjbase, choseong, slightchange, hardmode)
  return(eval)
}
#let's create a little function to call inside of our data compiling function
## that is coming up
pstdata <- function(conjugated) {
  addbase <- ifelse(conjugated[2] == TRUE, TRUE, FALSE)
  #this detects if there is a final consonant or not, and if there is, then 
  ## it tells the computer it needs to add the final consonant
  if (addbase == TRUE) {
    pst_base <- ifelse(conjugated[4] == kort1b, "았", "었")
    return(pst_base)
  } else {
    return(20)
  }
}
#either gives us the additive base for past tense, or we get FALSE, and we
## do a bit more work.
#we'll do another hard mode check based on the type of
##bachim we have
#use to check if we need to run the next function
irreg_type <- function(tbl) {
  #tbl stands for trouble since that's what these predicates are for us
  if (tbl == 7) {
    itype <- "d-type"
    #irregulars containing ㄷ
  } else if (tbl == 8) {
    itype <- "rl-type"
    #irregulars containing ㄹ
  } else if (tbl == 17) {
    itype <- "b-type"
    #irregulars containing ㅂ
  } else if (tbl == 19) {
    itype <- "s-type"
    #irregulars containing ㅅ
  } else {
    itype <- "ntype"
  }
  return(itype)
}
#we'll call this function if irregular_check() returns TRUE
#next function calls the previous data building functions in a sequence
##giving us a conjugation-ready dataset
conjreadybuild <- function(actpred) {
  stepone <- scrimcheck(actpred)
  easym <- easymodecheck(stepone)
  steptwo <- jamobreak(stepone)
  stepthree <- pstdata(steptwo)
  conjready <- list()
  conjready$scrim <- stepone
  conjready$breakdown <- steptwo
  conjready$pst <- stepthree
  conjready$irreg <- irreg_type(conjready$breakdown[1])
  conjready$scrim[3] <- ifelse(length(conjready$scrim[2]) > 0, paste(conjready$scrim[2], conjready$scrim[1], sep = ""), conjready$scrim[1])
  #set it to this first, then change it if need be
  compound <- if (conjready$breakdown[1] == FALSE) {
    vv <- conjready$breakdown[3]
    #stands for 'vowel value'
    #takes the vowel of the value so I don't have to write the full data
    ## expression every time I need it referenced
    compound <- ifelse(vv == 9 | vv == 14 | vv == 12 | vv == 17 | vv == 21, TRUE, FALSE)
  } else {
    FALSE
  }
  conjready$breakdown[8] <- compound
  conjready$org <- actpred
  conjready$easy <- easym
  return(conjready)
}
#we'll build a base translator function that handles our good eggs
basic_conjugator <- function(roon) {
  #roon stands for 'running out of names'
  orgdata <- roon
  #put in the same data that you get back from jamobreak()
  #let's do the ones where we add a base
  #we do this outside the if statement since it can be used in both conjugations
  if (roon$breakdown[2] == TRUE) {
    #this one is for when there is a bachim and you can't just add to the 
    ## bachim
    bridge <- paste(roon$scrim[3], roon$pst, sep = "")
    iip <-  paste(bridge, interconj, sep = "")
    #informal inpolite past
    ipp <- paste(iip, korpolend, sep = "")
    #informal polite past
    fpp <- paste(bridge, "습니다", sep = "")
    #formal polite past
    iipr <- paste(roon$scrim[3], roon$breakdown[4], sep = "")
    #informal polite present
    ippr <- paste(iipr, korpolend, sep = "")
    #informal polite present
    fppr <- paste(roon$scrim[3], "습니다", sep = "")
    #formal polite present
    #ostensibly the only reason there are two different functions is due to
    ## it being easier on my brain when trying to conjugate formal polite endings
    # they are so wonky so I need to streamline it.
    ## if I were going for code efficiency maybe I'd do this differently,
    ## but since we're not being graded on that, I'll do it this way
    # maybe someday I'll make it better
    fut_base <- paste(roon$scrim[3], "을", sep = "")
    #we have to make it here so that we can separate the 거XX endings with a space
    ## more easily
    iif <- paste(fut_base, koiipred, sep = " ")
    #informal impolite future
    ipf1 <- paste(fut_base, koippred1, sep = " ")
    ipf2 <- paste(fut_base, koippred2, sep = " ")
    #both forms of the informal polite
    fpf <- paste(fut_base, koifppred, sep = " ")
    #done. let's put them all in a list and return them
    conjling <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(conjling)
  }
  else if (roon$breakdown[2] == FALSE & roon$breakdown[8] == FALSE) {
    conjcho <- as.integer(orgdata$breakdown[5])
    #setting the choseong to the 5th value, which was the choseong deconstructed earlier
    conjmo <- as.integer(orgdata$breakdown[3])
    #setting the vowel the same as earlier
    conjbach <- 20
    #making the bachim "ㅆ"
    nbconjb <- (conjbach + ((conjmo-1)*28) + ((conjcho-1)*588)) + 44032
    #this is useless unless we change it into a character
    nbconjb <- intToUtf8(nbconjb)
    #builds the character, doing the inverse of what we did earlier in
    ## jamobreak()
    npb <- paste(orgdata$scrim[2], nbconjb, sep = "")
    #add the otherwheels (the rest of the verb) and the discriminant with
    ## the past conjugation on it
    #now let's build some past tenses
    #they will have the same labels as the above function, so instead of writing
    ## the keys twice, assume the same acronyms as above
    # saves my poor hands
    iip <- paste(npb, interconj, sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(npb, "습니다", sep = "")
    iipr <- as.character(orgdata$scrim[3])
    ippr <- paste(orgdata$scrim[3], korpolend, sep = "")
    #the formal polite is going to require some extra 'fun'
    fpprbase <- (17 + ((conjmo-1)*28) + ((conjcho-1)*588)) + 44032
    fpprbase <- intToUtf8(fpprbase)
    fpprbase <- ifelse(length(orgdata$scrim[2]) > 0, paste(orgdata$scrim[2], fpprbase, sep = ""), fpprbase)
    #creates the last jamo with a 'ㅂ' in the bachim, no longer just a
    ## number
    fppr <- paste(fpprbase, '니다', sep = "")
    #more 'fun' ahead: building the base for the future
    futbasedis <- utf8ToInt(as.character(orgdata$scrim[1])) + 8
    #time to make it into a jamo again
    futbasedis <- intToUtf8(futbasedis)
    #cool, let's conjugate some stuff
    futbase <- paste(orgdata$scrim[2], futbasedis, sep = "")
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    #now that's done, lists again
    conjling <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(conjling)
  }
}
#that function is for our well-behaving predicates. let's make some specific ones
## for our troublesome irregular fellas
irregsc <- function(stype) {
  #stands for 'irregular-s conjugation'
  orgdata <- stype
  #creating a backup, also helps keep it consistent within the function
  #argument is called 'stype' so I can see what kind of irregular it calls
  ## when I see my object list in my environment
  scrim <- orgdata$scrim[1]
  scrim <- as.character(scrim)
  scrimnum <- utf8ToInt(scrim)
  daeconj <- scrimnum - 19
  #recreates the discriminant without the bachim, since s-irregulars
  ## lose the bachim ㅅ. There are a few exceptions, but we'll
  ## ignore those for now
  daeconj <- intToUtf8(daeconj)
  daeconjbase <- paste(daeconj, orgdata$scrim[2], sep = "")
  #the dae stands for 대부분, which is Korean for 'most,' since most conjugations
  ##will use this base.
  #once again, same acronyms as the base_conjugator function
  iip <- paste(daeconjbase, orgdata$pst, orgdata$breakdown[4], sep = "")
  ipp <- paste(iip, korpolend, sep = "")
  fpp <- paste(daeconjbase, orgdata$pst, '습니다', sep = "")
  iipr <- paste(daeconjbase, orgdata$breakdown[4], sep = "")
  ippr <- paste(iipr, korpolend, sep = "")
  fppr <- paste(orgdata$scrim[3], "습니다", sep = "")
  futbase <- paste(orgdata$scrim[3], "을", sep = "")
  #doing this separately so we can separate things in the future tense
  iif <- paste(futbase, koiipred, sep = " ")
  ipf1 <- paste(futbase, koippred1, sep = " ")
  ipf2 <- paste(futbase, koippred2, sep = " ")
  fpf <- paste(futbase, koifppred, sep = " ")
  sconjfinal <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(sconjfinal)
}
irregbc <- function(btype) {
  #this function deals with the b-irregulars
  orgdata <- btype
  scrim <- as.character(orgdata$scrim[1])
  scrimnum <- utf8ToInt(scrim)
  debached <- scrimnum - 17
  daeconjbase <- intToUtf8(debached)
  daeconj <- paste(orgdata$scrim[2], daeconjbase, sep = "")
  #now we have the predicate base w/o the bachim
  wop <- '웠'
  #wo-past
  wo <- '워'
  #wo
  #these are the little conjugation particles for this kind of irregular
  #same acronyms as conj_basic
  iip <- paste(daeconj, wop, kort2b, sep = "")
  ipp <- paste(iip, korpolend, sep = "")
  fpp <- paste(daeconj, wop, '습니다', sep = "")
  iipr <- paste(daeconj, wo, sep = "")
  ippr <- paste(iipr, korpolend, sep = "")
  fppr <- paste(orgdata$scrim[3], '니다', sep = "")
  fut_part <- "울"
  futbase <- paste(daeconj, fut_part, sep = "")
  #once again, done separately for spacing
  iif <- paste(futbase, koiipred, sep = " ")
  ipf1 <- paste(futbase, koippred1, sep = " ")
  ipf2 <- paste(futbase, koippred2, sep = " ")
  fpf <- paste(futbase, koifppred, sep = " ")
  bregcon <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(bregcon)
}
#that's the b-irregulars out of the way
#next set of irregulars!
irregdc <- function(dtype) {
  #irregular d type predicates only get this treatment if they are a verb
  #but we'll treat them all the same. I'm ignoring exceptions and doing
  ## the more common types
  #to my knowledge there are more d-type irregular verbs than adjectives
  ## but I can't be sure
  # regardless, neither are really all that common
  orgdata <- dtype
  conjbach <- 8
  #8 in bachim represents ㄹ, which d-type irregular verbs get in their bachim
  ## instead of ㄷ
  conjmo <- as.integer(orgdata$breakdown[3])
  conjmo <- conjmo - 1
  conjcho <- as.integer(orgdata$breakdown[5])
  conjcho <- conjcho - 1
  latswitch <- (conjbach + ((conjmo)*28) + ((conjcho)*588)) + 44032
  #latswitch means 'lateral switch' because it rebuilds the jamo with
  ## ㄹ in the bachim
  latswitch <- intToUtf8(latswitch)
  latroot <- paste(orgdata$scrim[2], latswitch, sep = "")
  #same acronyms as earlier
  iip <- paste(latroot, orgdata$pst, orgdata$breakdown[4], sep = "")
  ipp <- paste(iip, korpolend, sep = "")
  fpp <- paste(latroot, orgdata$pst, '습니다', sep = "")
  iipr <- paste(latroot, orgdata$breakdown[4], sep = "")
  ippr <- paste(iipr, korpolend, sep = "")
  fppr <- paste(orgdata$scrim[3], "습니다", sep = "")
  futbase <- paste(latroot, '을', sep = "")
  iif <- paste(futbase, koiipred, sep = " ")
  ipf1 <- paste(futbase, koippred1, sep = " ")
  ipf2 <- paste(futbase, koippred2, sep = " ")
  fpf <- paste(futbase, koifppred, sep = " ")
  diconj <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(diconj)
}
#the last consonant irregular is going to be the same as basic except
## for a SINGLE conjugation. I could not get it to work in the basic,
##so I have to handle the exception separately
irregrlc <- function(rlirreg) {
  orgdata <- rlirreg
  pstbase <- paste(orgdata$scrim[3], orgdata$pst, sep = "")
  iip <- paste(pstbase, orgdata$breakdown[4], sep = "")
  ipp <- paste(iip, korpolend, sep = "")
  fpp <- paste(pstbase, '습니다', sep = "")
  iipr <- paste(orgdata$scrim[3], orgdata$breakdown[4], sep = "")
  ippr <- paste(iipr, korpolend, sep = "")
  #finally, the whole reason I have a separate conjugator
  fpprcho <- as.integer(orgdata$breakdown[5])
  fpprmo <- as.integer(orgdata$breakdown[3])
  fpprbase <- (17 + ((fpprmo - 1)*28) + ((fpprcho - 1) *588)) + 44032
  fpprbase <- intToUtf8(fpprbase)
  fpprbase <- paste(orgdata$scrim[2], fpprbase, sep = "")
  fppr <- paste(fpprbase, "니다", sep = "")
  futbase <- paste(orgdata$scrim[3], "을", sep = "")
  iif <- paste(futbase, koiipred, sep = " ")
  ipf1 <- paste(futbase, koippred1, sep = " ")
  ipf2 <- paste(futbase, koippred2, sep = " ")
  fpf <- paste(futbase, koifppred, sep = " ")
  rliconj <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(rliconj)
}
#now let's handle the vowel oddities, then the hardcore irregulars
vowel_exception <- function(copvow) {
  #copvow stands for 'Complicated OPen VOWel'
  #yeah i know not the best name
  copvowint <- as.integer(copvow$breakdown[3])
  if (copvowint == 9) {
    newvow <- 10
  } else if (copvowint == 14) {
    newvow <- 15
  } else if (copvowint == 12) {
    newvow <- 11
  } else if (copvowint == 21) {
    newvow <- 7
  } else {
    newvow <- copvowint
  }
  newbasecho <- as.integer(copvow$breakdown[5])
  newbasemo <- newvow
  newbase <- (((newbasemo - 1)*28) + ((newbasecho-1)*588)) + 44032
  #taking the old base and giving it a shiny new vowel
  newbase <- intToUtf8(newbase)
  copvow$scrim[4] <- newbase
  ifelse(length(copvow$scrim[3]) > 0, copvow$scrim[5] <- paste(copvow$scrim[2], copvow$scrim[4], sep = ""), copvow$scrim[5] <- newbase)
  #long ifelse statement that says to add the rest of the verb root back, and
  #if it doesn't exist, just leave the root as the full verb root
  copvow$breakdown[4] <- "어"
  copvow$pst <- "었"
  return(copvow)
}
#that handles what vowels we use to conjugate, now we need a new
##function to put it together
exvowconj <- function(tricky) {
  orgdata <- tricky
  vowel <- as.integer(orgdata$breakdown[3])
  #saves us from writing out the full thing each time
  baseroot <- ifelse(length(orgdata$scrim[3]) > 0, paste(orgdata$scrim[2], orgdata$scrim[4]), orgdata$scrim[4])
  if (vowel == 9 | vowel == 10 | vowel == 12 | vowel == 14 | vowel == 21) {
    #this function is for the vowels that become compounds when conjugated
    #9 is ㅗ, 10 is ㅘ, 12 is ㅚ, 14 is ㅜ, and 21 is ㅣ
    pstbase <- (utf8ToInt(as.character(orgdata$scrim[4]))) + 20
    pstbase <- intToUtf8(pstbase)
    #to debud my code I'd often run the function on a test predicate
    #i have forgotten to change the integer back into a character an
    ## embarrasing number of times
    pstbase <- ifelse(length(orgdata$scrim[2]) > 0, paste(orgdata$scrim[2], pstbase, sep = ""), pstbase)
    #another one of our if/else statements to handle if the verb root is larger
    ## than our discriminant
    iip <- paste(pstbase, orgdata$breakdown[4], sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(pstbase, '습니다', sep = "")
    iipr <- paste(orgdata$scrim[5])
    ippr <- paste(iipr, korpolend, sep = "")
    #time for some more fun with the formal polite
    fpprbase <- (utf8ToInt(as.character(orgdata$scrim[1]))) + 17
    fpprbase <- intToUtf8(fpprbase)
    fpprbase <- ifelse(length(orgdata$scrim[2]) > 0, paste(orgdata$scrim[2], fpprbase, sep = ""), fpprbase)
    fppr <- paste(fpprbase, '니다', sep = "")
    futbase <- (utf8ToInt(as.character(orgdata$scrim[1]))) + 8
    futbase <- intToUtf8(futbase)
    futbase <- ifelse(length(orgdata$scrim[2]) > 0, paste(orgdata$scrim[2], futbase, sep = ""), futbase)
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    compoundconj <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(compoundconj)
  } else if (vowel == 17) {
    #this handles if the vowel is ㅟ, which is a bit easier
    ## than the other ones, but still insists on being different.
    orgdata$pst <- "었"
    #the original function returns the '20' that it would for a
    ## non-exception function
    # because the ㅟ irregular verbs don't need to go through the 
    ## exception control (because they're not a compound and don't require
    ## a vowel change), the past vowel needs to be manually set
    base <- ifelse(length(orgdata$scrim[3]) > 0, paste(orgdata$scrim[2], orgdata$scrim[1], sep = ""), orgdata$scrim[1])
    iip <- paste(base, orgdata$pst, interconj, sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(base, orgdata$pst, '습니다', sep = "")
    iipr <- paste(base, orgdata$breakdown[4], sep = "")
    ippr <- paste(iipr, korpolend, sep = "")
    #FUN
    #adding more ㅂ to the bottom because fppr can't be normal
    fpprbase <- (utf8ToInt(as.character(orgdata$scrim[1]))) + 17
    fpprbase <- intToUtf8(fpprbase)
    fpprbase <- ifelse(length(orgdata$scrim[2]) > 0, paste(orgdata$scrim [2], fpprbase, sep = ""), fpprbase)
    fppr <- paste(fpprbase, '니다', sep = "")
    futbase <- utf8ToInt(as.character(orgdata$scrim[1])) + 8
    #for future, adds the ㄹ to the bottom
    futbase <- intToUtf8(futbase)
    #back to character
    futbase <- ifelse(length(orgdata$scrim[2]) > 0, futbase <- paste(orgdata$scrim[2], futbase, sep = ""), futbase)
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    #there. almost all of the tricky vowels handled.
    easierconj <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(easierconj)
    #if I had a dollar for every time I forgot to add a return function...
  }
}
filter <- function(hardjamo) {
  #this function filters out the 르 irregulars, since they are
  ## easier to deal with than the other irregulars
  disc2 <- as.character(hardjamo$scrim[1])
  reucheck <- ifelse(disc2 == "르", TRUE, FALSE)
  #르 irregulars work differently than the other ㅡ irregulars, so we'll
  ## add a check for our future function.
  hardjamo$filter <- reucheck
  return(hardjamo)
}
euconj <- function(euireg) {
  #this function conjugates the ㅡ irregulars, with an extra bit for 르
  ## irregulars
  diffdata <- euireg
  diffdata <- filter(diffdata)
  #stands for difficult data
  singcheck <- ifelse(length(diffdata$scrim[3]) > 1, FALSE, TRUE)
  if (singcheck == FALSE) {
    nsccheck <- paste(diffdata$scrim[1], "다", sep = "")
    #newscrimcheck
    newscrim <- str_extract(diffdata$org, "..[다]$")
    newscrim <- sub(nsccheck, "", newscrim)
    bta <- ifelse(length(diffdata$scrim[2]) > 0, sub(newscrim, "", diffdata$scrim[2]), "")
    #this takes the last
    holdint <- utf8ToInt(newscrim)
    #this stands for 'hold integer.' We won't do much with it so
    ## it's just a brief hold
    newscrimstripped <- holdint - 44032
    #subtracts so we get to the korean utf-8 section
    newscrimbach <- newscrimstripped%%28
    #we really don't to know the bachim, but we need the bachim to calculate
    ## the vowel value
    nsmo <- 1 + ((newscrimstripped - newscrimbach)%%588)/28
    nscho <- 1 + floor((newscrimstripped)/588)
    #the choseong of the previous syllable won't be needed for most, but
    ## the oh-so-special 르 ones will need it to build another character
    newbase <- ifelse(nsmo == 1 | nsmo == 9 | nsmo == 3 | nsmo == 13, 1, 5)
  } else {
    newbase <- 5
    ncho <- diffdata$breakdown[5]
  }
  if (diffdata$filter == FALSE) {
    ncho <- as.integer(diffdata$breakdown[5])
    #new choseong
    nmo <- newbase
    #new moeum
    nbint <- (((nmo-1)*28) + ((ncho-1)*588)) + 44032
    nb <- intToUtf8(nbint)
    #combine to make a new base
    #works even for the one syllable ㅡ irregulars
  } else {
    prebaseint <- utf8ToInt(as.character(newscrim)) + 8
    #adds a ㄹ to the bottom of the syllable before the 르
    prebase <- intToUtf8(prebaseint)
    prebase <- paste(bta, prebase, sep = "")
    nbint <- (((newbase-1)*28) + ((6-1)*588)) + 44032
    nb <- intToUtf8(nbint)
    #makes the last syllable before the vowel ㄹ + whichever vowel
    ## conjugates with the prebase's vowel
  }
  if (diffdata$filter == FALSE & singcheck == FALSE) {
    #handles the 2 syllable ㅡ irregulars
    pstbase1 <- nbint + 20
    bta <- diffdata$scrim[2]
    pstbase <- intToUtf8(pstbase1)
    pstbase <- paste(bta, pstbase, sep = "")
    iip <- paste(pstbase, interconj, sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(pstbase, '습니다', sep = "")
    iipr <- paste(bta, nb, sep = "")
    ippr <- paste(iipr, korpolend, sep="")
    #always with the formal polite endings...
    fpprbase1 <- utf8ToInt(as.character(diffdata$scrim[1])) + 17
    fpprbase2 <- intToUtf8(fpprbase1)
    fpprbase <- paste(bta, fpprbase2, sep = "")
    #no exceptions to handle, since we are dealing with the 2 syllable
    ## verbs separately from the 1 syllable exceptions
    fppr <- paste(fpprbase, '니다', sep = "")
    #future!
    futbase1 <- fpprbase1 - 17
    futbase2 <- futbase1 + 8
    futbase3 <- intToUtf8(futbase2)
    futbase <- paste(bta, futbase3, sep = "")
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    longbase <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(longbase)
    #guess who remembered this time
  } else if (singcheck == TRUE & diffdata$filter == FALSE) {
    #handles the single syllable ㅡ irregulars
    nbint <- ((5-1)*28) + ((ncho-1)*588) + 44032
    pstbase1 <- nbint + 20
    pstbase <- intToUtf8(pstbase1)
    iip <- paste(pstbase, interconj, sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(pstbase, '습니다', sep = "")
    iipr <- nb
    ippr <- paste(iipr, korpolend, sep = "")
    #i think this may be one of the last ㅂ insertions I have to do
    fpprbase1 <- utf8ToInt(as.character(diffdata$scrim[1])) + 17
    fpprbase <- intToUtf8(fpprbase1)
    fppr <- paste(fpprbase, "니다", sep = "")
    #more future conjugations
    futbase1 <- (fpprbase1-17) + 8
    futbase <- intToUtf8(futbase1)
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    shortbase <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    return(shortbase)
  } else {
    #handles the 르 irregulars. I see the light.
    pstbase1 <- nbint + 20
    pstbase2 <- intToUtf8(pstbase1)
    pstbase <- paste(prebase, pstbase2, sep = "")
    iip <- paste(pstbase, interconj, sep = "")
    ipp <- paste(iip, korpolend, sep = "")
    fpp <- paste(pstbase, '습니다', sep = "")
    iipr <- paste(prebase, nb, sep = "")
    ippr <- paste(iipr, korpolend, sep = "")
    #one. more. fppr
    fpprbase1 <- utf8ToInt(as.character(diffdata$scrim[1])) + 17
    fpprbase2 <- intToUtf8(fpprbase1)
    fpprbase <- paste(diffdata$scrim[2], fpprbase2, sep = "")
    fppr <- paste(fpprbase, '니다', sep = "")
    #the final future. yay
    futbase1 <- utf8ToInt(as.character(diffdata$scrim[1])) + 8
    futbase2 <- intToUtf8(futbase1)
    futbase <- paste(diffdata$scrim[2], futbase2, sep = "")
    iif <- paste(futbase, koiipred, sep = " ")
    ipf1 <- paste(futbase, koippred1, sep = " ")
    ipf2 <- paste(futbase, koippred2, sep = " ")
    fpf <- paste(futbase, koifppred, sep = " ")
    tblmkr <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
    #stands for troublemaker
    return(tblmkr)
  }
}
#now we'll build a function to rationalize how to conjugate the data.
solver <- function(input) {
  data <- conjreadybuild(input)
  if (data$easy == TRUE & data$irreg == "ntype" & data$scrim[1] == "하") {
    trivial <- easyconj(data)
    trivialcase <- frontend_compiler(trivial)
    return(trivialcase)
    #deals with the hada verbs
  } else if (data$easy == TRUE & data$scrim[1] == '이') {
    coco <- copconj(data)
    cocoreturn <- frontend_compiler(coco)
    return(cocoreturn) 
    } else if (data$breakdown[8] == FALSE & data$irreg == "ntype" & data$breakdown[7] == FALSE & data$easy == FALSE) {
    #had to add the easy check since restricted copula could get past without it
    basic <- basic_conjugator(data)
    basicresults <- frontend_compiler(basic)
    return(basicresults)
    #deals with the other normal verbs
  } else if (data$breakdown[8] == TRUE) {
    comp <- vowel_exception(data)
    compound <- exvowconj(comp)
    compdata <- frontend_compiler(compound)
    return(compdata)
    #deals with the compound vowel verbs
  } else if (data$irreg == "d-type") {
    dtrick <- irregdc(data)
    dreturn <- frontend_compiler(dtrick)
    return(dreturn)
    #deals with d-type irregulars
  } else if (data$irreg == "b-type") {
    btrick <- irregbc(data)
    breturn <- frontend_compiler(btrick)
    return(breturn)
    #deals with b-type irregulars
  } else if (data$irreg == "rl-type") {
    rltrick <- irregrlc(data)
    rlreturn <- frontend_compiler(rltrick)
    return(sreturn)
  } else if (data$irreg == "s-type") {
    strick <- irregsc(data)
    sreturn <- frontend_compiler(strick)
    return(sreturn)
  } else if (data$breakdown[7] == TRUE) {
    eudata <- filter(data)
    eudat <- euconj(eudata)
    eudatreturn <- frontend_compiler(eudat)
    return(eudatreturn)
  } else {
    return("Yeah, you broke me somehow...")
  }
  }
frontend_compiler <- function(conjdata) {
  #compiles the data together
  iip <- paste("Informal Impolite Past:", conjdata[1], sep = " ")
  ipp <- paste("Informal Polite Past:", conjdata[2], sep = " ")
  fpp <- paste("Formal Polite Past:", conjdata[3], sep = " ")
  iipr <- paste("Informal Impolite Present:", conjdata[4], sep = " ")
  ippr <- paste("Informal Polite Present:", conjdata[5], sep = " ")
  fppr <- paste("Formal Polite Present:", conjdata[6], sep = " ")
  iif <- paste("Informal Impolite Future", conjdata[7], sep = " ")
  ipf1 <- paste("Informal Polite Future Type 1:", conjdata[8], sep = " ")
  ipf2 <- paste("Informal Polite Future Type 2:", conjdata[9], sep = " ")
  fpf <- paste("Formal Polite Future:", conjdata[10], sep = " ")
  #that's why all of the functions conjugated in the same order
  results <- list(iip, ipp, fpp, iipr, ippr, fppr, iif, ipf1, ipf2, fpf)
  return(results)
}
#now for the input function
yourverbssir <- function() {
  verb <- readline(prompt = "Please enter a Korean predicate (in Hangul) in the infinitive: ")
  if (length(verb) > 0) {
    real <- predchecker(verb)
    if (real == FALSE) {
      print("Please run again, with an actual Korean in the infinitive")
      stop()
    } else {
      print("Thank you, we'll conjugate your verb shortly.")
      yourverbsir <- solver(verb)
      print(yourverbsir)
      print("There you go, sir, your verbs")
    }
  }
}