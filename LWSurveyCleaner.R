library(missForest)


df = read.csv("~/Dropbox/LWRelativeDiseaseBurden/2014forpublic.csv")
#See http://lesswrong.com/lw/lhg/2014_survey_results/ for a summary and a link to the survey
#The data was sort of garbled, necessitating stripping out non-answers

#Remove metadata
df = df[-4:0]


#Restricting consideration to those participants who answered the depression question
levs = levels(df$Depression)[6:8]
df = df[df$Depression %in% levs,]



#Removing probability calibration questions, 
#irrelevant variables, and questions with freeform answers, 
#as well as an example that was messed up
n = names(df)
drops = c(n[grep("Cal",n)], "Instructions", "TestQuestionToSeeIfYourePayingAttention", 
          "PublicKey", "filter_.", "WhatDifferent", "HowDifferent", "ChangesToRoutines", "ComplexAffiliation",
          "ReligiousDenomination", "ReligiousBackground", "Referrals", "BlogReferrals","Birthmonth",
          "IQType", "FavoritePost", "PreviousSurveys")

df = df[-735,!(n %in% drops)]

#Convert numeric variables from categorical variables
nums = c(1,12, 21:25,28:30, 35:46,49,50,67:79,83:85, 89,95,97:98)
df[nums] = lapply(df[nums], function(x){return(as.numeric(as.character(x)))})

#Restrict to most common answers and abbreviate names
df$Country = factor(df$Country, c("United States", "United Kingdom", "Canada", "Australia", "Germany"))
levels(df$Country) = c("US", "UK", "Canada", "Australia", "Germany")
df$Country = factor(ifelse(is.na(as.character(df$Country)), "Other", as.character(df$Country)))

df$Race = factor(df$Race, c("White (non-Hispanic)", 
                            "Asian (East Asian)", 
                            "Asian (Indian subcontinent)",
                            "Hispanic"))
levels(df$Race) = c("White", "Asian", "Indian", "Hispanic")

df$Sex = factor(df$Sex, c("Male", "Female"))

df$Gender = factor(df$Gender, c("M (cisgender)", "F (cisgender)","F (transgender m -> f)", "M (transgender f -> m)"))
levels(df$Gender) = c("CisMale", "CisFemale", "TransFemale", "TransMale")

df$SexualOrientation = factor(df$SexualOrientation, c("Heterosexual", "Bisexual", "Asexual", "homoSexual"))

df$Relationshipstyle = factor(df$Relationshipstyle, c("Prefer monogamous", "Prefer polyamorous"))
levels(df$Relationshipstyle) = c("Monogamous", "Polyamorous")

#Categorize 2+ as "many"
noneOneMultiple = function(feature){
  feature = as.numeric(as.character(feature))
  feature = ifelse(feature > 2, 2, feature)
  feature = factor(feature, 0:2, ordered = TRUE)
  levels(feature) =  c("None", "One", "Multiple")
  return(feature)
}
arr = c("NumberofCurrentPartners", "Children", "Youngersiblings", "Oldersiblings")
df[arr] = lapply(df[arr], function(x){noneOneMultiple(x)})



df$RelationshipGoals = factor(df$RelationshipGoals, c("...and currently not looking for more relationship partners",
                                                      "...and possibly open to more relationship partners",
                                                      "...and seeking more relationship partners"))


levels(df$RelationshipGoals) = c("NotLookin", "PossiblyLooking", "Looking")

df$RelationshipStatus = factor(df$RelationshipStatus, c("Married", "Relationship", "Single"))

df$LivingWith = factor(df$LivingWith, c("Alone", "With parents or other guardians", "With roommates","With partner and/or children"))
levels(df$LivingWith) = c("Alone", "Parents", "Roommates", "Partner")




df$MoreChildren = factor(df$MoreChildren, c("Yes", "No", "Uncertain"))

df$WorkStatus = factor(df$WorkStatus, c("For-profit work", 
                                        "Government work", 
                                        "Academics (on the teaching side)", 
                                        "Unemployed",
                                        "Self-employed",
                                        "Student",
                                        "Non-profit work"))

levels(df$WorkStatus) = c("ForProfit", 
                          "Government", 
                          "Academics", 
                          "Unemployed", 
                          "SelfEmployed", 
                          "Student", 
                          "NonProfit")


df$Profession = factor(df$Profession, c("Art", 
                                        "Biology", 
                                        "Business", 
                                        "Computers (AI)", 
                                        "Computers (practical: IT, programming, etc.)",
                                        "Engineering", "
                                        Finance / Economics", 
                                        "Law", 
                                        "Mathematics", 
                                        "Medicine",
                                        "Physics", 
                                        "Psychology", 
                                        "Statistics", 
                                        'Other "social science"'))
levels(df$Profession) = c("Art", 
                          "Biology", 
                          "Business", 
                          "ComputerAI", 
                          "ComputerPractical",
                          "Engineering", 
                          "financeEcon", 
                          "Law", 
                          "Mathematics", 
                          "Medicine",
                          "Physics", 
                          "Psychology", 
                          "Statistics", 
                          "OSocialSci")


df$Degree = factor(df$Degree, c("2 year degree", 
                                "High school",
                                "Bachelor's",
                                "Master's", 
                                "MD/JD/other professional degree", 
                                "Ph D."), ordered = TRUE)
levels(df$Degree) = c("TwoYear", 
                      "HighSchool",
                      "Bachelor", 
                      "Master", 
                      "Professional", 
                      "PhD")

df$Political = factor(df$Political, levels =c("Conservative, for example the US Republican Party and UK Tories: traditional values, low taxes, low redistribution of wealth",
                      "Liberal, for example the US Democratic Party or the UK Labour Party: socially permissive, more taxes, more redistribution of wealth",
                      "Libertarian, for example like the US Libertarian Party: socially permissive, minimal/no taxes, minimal/no distribution of wealth",
                      "Social democratic, for example Scandinavian countries: socially permissive, high taxes, major redistribution of wealth"))
levels(df$Political) = c("Conservative", "Liberal", "Libertarian", "SocialDemocratic")

df$ReligiousViews = factor(df$ReligiousViews,  levels = c("Agnostic", 
                                                          "Atheist and not spiritual", 
                                                          "Atheist but spiritual",
                                                          "Committed theist",
                                                          "Lukewarm theist"))
levels(df$ReligiousViews) = c("Agnostic", 
                              "Atheist", 
                              "Spiritual", 
                              "CommitedTheist", 
                              "LukeWarmTheist")

df$FamilyReligion = factor(df$FamilyReligion,  levels = c("Agnostic", 
                                                          "Atheist and not spiritual", 
                                                          "Atheist but spiritual",
                                                          "Committed theist",
                                                          "Lukewarm theist"))
levels(df$FamilyReligion) = c("Agnostic", "Atheist", "Spiritual", "CommitedTheist", "LukeWarmTheist")

df$MoralViews = factor(df$MoralViews, c("Accept / lean toward consequentialism", 
                                          "Accept / lean toward deontology",
                                          "Accept / lean toward natural law",    
                                          "Accept / lean toward virtue ethics",
                                          "Accept / lean towards contractualism"))
levels(df$MoralViews) = c("Consequentialism", "Deontology", "NaturalLaw", "VirtueEthics", "Contractualism")

df$LessWrongUse = factor(df$LessWrongUse,c("I lurk, but never registered an account",
                            "I've registered an account, but never posted",                                           
                           "I've posted a comment, but never a top-level post",
                           "I've posted in Discussion, but not Main",
                           "I've posted in Main"))
levels(df$LessWrongUse) = c("Lurk", "Registered","Comment", "Discussion", "Main")

df$Sequences = factor(df$Sequences, c("Never even knew they existed until this moment",
                                      "Know they existed, but never looked at them",
                                      "Some, but less than 25%",
                                      "About 25% of the Sequences",
                                      "About 50% of the Sequences",
                                      "About 75% of the Sequences",
                                      "About 100% of the Sequences"))
levels(df$Sequences) = c("-1","0","1", "25", "50", "75", "100", ordered = TRUE)


df$TimeinCommunity = factor(df$TimeinCommunity, 2008:2014, ordered = TRUE)

df$TimeonLW = ifelse(df$TimeonLW >40, median(df$TimeonLW, na.rm = TRUE), df$TimeonLW )

df$CFAREvents = factor(df$CFAREvents,  c("No, never", "Yes, both in 2014 and before that", "Yes, in 2013 or earlier", "Yes, in 2014"))
levels(df$CFAREvents) = c("No", "Before2014", "Before2013", "2014")

df$Meetups = factor(df$Meetups, c("Yes, regularly","Yes, once or a few times", "No"), ordered = TRUE)
levels(df$Meetups) = c("Yes", "Few", "No")

df$Community = factor(df$Community, c("Yes, all the time", "Yes, sometimes", "No"), ordered = TRUE)
levels(df$Community) = c("Yes", "Sometimes", "No")

df$Romance = factor(df$Romance, c("I didn't meet them through the community, but they're part of the community now",
                                  "Yes",
                                  "No"))
levels(df$Romance) = c("Now", "Yes", "No")


df$TypeofGlobalCatastrophicRisk = factor(df$TypeofGlobalCatastrophicRisk, c("Asteriod strike",
                                                                            "Economic / political collapse",
                                                                            "Environmental collapse (including global warming)",
                                                                            "Pandemic (bioengineered)",
                                                                            "Pandemic (natural)",
                                                                            "Nuclear war",
                                                                            "Unfriendly AI"))
levels(df$TypeofGlobalCatastrophicRisk) = c("Asteroid",
                                            "Economic", 
                                            "Environment", 
                                            "Bio", "Pandemic", 
                                            "Nuclear", 
                                            "UFAI")

df$CryonicsStatus = factor(df$CryonicsStatus, c("No - and do not want to sign up for cryonics", 
                                                "No - still considering it",
                                                "No - would like to sign up but haven't gotten around to it",
                                                "No - would like to sign up but unavailable in my area",
                                                "Yes - signed up or just finishing up paperwork"))

levels(df$CryonicsStatus) = c("No", "Considering", "NotYet", "Unavailable", "Yes")


mentals = c("Depression",
            "Obsessivecompulsivedisorder",
            "Autismspectrum",
            "Bipolardisorder",
            "Anxietydisorder",
            "Borderlinepersonalitydisorder",
            "Schizophrenia")

levs = c("Yes, I was formally diagnosed by a doctor or other mental health professional",
         "Not formally, but I personally believe I have (or had) it",
         "No")
for(m in mentals){
  df[[m]] = factor(df[[m]], levs, ordered = TRUE)
  levels(df[[m]]) = c("Yes", "Self", "No")
}

#Converting strength of agreement to numeric
cfars = c("Endeavor", "Challenges", "Tough", "Stuck", "Person", "Basic")
for(cfar in cfars){
  df[[cfar]] = factor(df[[cfar]], levels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
  levels(df[[cfar]])  = c(4,3,2,1)
  df[[cfar]] = as.numeric(as.character(df[[cfar]]))
}

df$Voting = factor(df$Voting, c("Yes", "No"))

df$AmericanParties = factor(df$AmericanParties, c("(option for non-Americans who want an option)", 
                                                  "Democratic Party",                            
                                                  "Libertarian Party",
                                                  "Other third party",
                                                  "Republican Party"))
levels(df$AmericanParties) = c("notAmerican", "Democrat", "Libertarian", "other", "Republican")


df$EffectiveAltruism = factor(df$EffectiveAltruism, c("Yes", "No"))

df$EffectiveAltruism2 = factor(df$EffectiveAltruism2, c("Yes", "No"))

df$Vegetarian = factor(df$Vegetarian, levels = c("No", 
                                                 "Yes, I restrict meat some other way (pescetarian, flexitarian, try to only eat ethically sourced meat)",
                                                 "Yes, I am vegetarian",
                                                 "Yes, I am vegan"), ordered = TRUE)
levels(df$Vegetarian) = c("No", "Restrict", "Vegetarian", "Vegan")

df$Handedness = factor(df$Handedness, c("Right hand", "Left hand", "Ambidextrous"))
levels(df$Handedness) =  c("Right", "Left", "Both")

df$GenderDefault = factor(df$GenderDefault, c("I only identify with my birth gender by default", "I strongly identify with my birth gender"))
levels(df$GenderDefault) = c("Yes", "No")

df$HPMOR = factor(df$HPMOR, levels = c("No", "Started it but haven't finished", "Yes"))
levels(df$HPMOR) = c("No", "Part", "Yes")

#Removing outliers
df$HoursOnline = ifelse(df$HoursOnline >= 100, 100, df$HoursOnline)


df$PaleoDiet = factor(df$PaleoDiet,c("I do not follow a paleo diet", 
                        "I try to follow some paleo principles some of the time",
                        "Yes, I stick to a paleo diet pretty well"))
levels(df$PaleoDiet) = c("No", "Some", "Yes", ordered = TRUE)

foodSubs = c("No, I have never tried these.",
    "I have tried them, but I don't use them regularly.",             
    "Yes, I use them regularly but also eat normal food.",
    "Yes, I get most of my calories from these")
df$FoodSubstitutes = factor(df$FoodSubstitutes, foodSubs)
levels(df$FoodSubstitutes) = c("No", "Tried", "Some", "Most", ordered = TRUE)

df$MetaEthics = factor(df$MetaEthics, levels(df$MetaEthics)[3:7])
levels(df$MetaEthics) = c("constructivism", "errorTheory", "nonCognitivism", "subjectivism", "realism")


df$Books = factor(df$Books, levels = c("< 5","5 - 10", "10 - 20", "20 - 50" ,"50 - 100", "> 100" ), ordered = TRUE)
levels(df$Books) =   c("0", "5", "10", "20", "50", "100")

df$CFARWorkshop = factor(df$CFARWorkshop, c("No", "Yes, I have been to a full (3+ day) workshop"))
levels(df$CFARWorkshop) = c("No", "Yes")

#Abbreviate long feature names
colnames(df)[c(45,47,52,56)] = c("PGCR", "typeGCR", "OCD", "BPD")

#Convert probability estimates to log-odds-ratios
probsToLORs = function(x){
  x = ifelse(x < 0.1, 0.1, x)
  x = ifelse(x > 99.9, 99.9, x)
  x = x/(100 - x)
  x = log(x)
  return(x)
}
probs = colnames(df[35:45])
df[probs] = lapply(df[probs],probsToLORs)

#Take logs of variables that vary by orders of magnitude
logged = c("KarmaScore", "Income", "Charity", "MIRICFAR", "XriskCharity")
toLog = function(x){
  x = ifelse(x < 1, 1, x)
  return(log(x))
}
df[logged] = lapply(df[logged], toLog)


write.csv(df, "cleanedNotFilled.csv", row.names = FALSE)

#Fill missing values using other features other than depression via random forest
#https://en.wikipedia.org/wiki/Random_forest
#https://www.quora.com/Random-Forests/How-do-random-forests-work-in-laymans-terms
features = df[names(df) != "Depression"]

set.seed(1); mrf = missForest(features, verbose = TRUE)
features = mrf$ximp
withDepression = cbind(df["Depression"],features)

write.csv(withDepression, "filled.csv", row.names = FALSE)
