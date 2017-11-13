setwd("C:/HCL/LikeMe")

#Loading the required packages.
library(shiny)
library(shinydashboard)
library(quanteda, irlba)
library(ggplot2)
library(e1071)
library(lattice)
library(quanteda)
library(zoo)
library(lubridate)
library(data.table) 
library(fiftystater)
library(forecast)
library(rvest)
library(maps)
library(mapproj)
library(tmap)
library(maptools)
library(dplyr)
library(openxlsx)
library(xml2)
library(sp)
library(plotly)
library(radarchart)
library(fmsb)
library(DT)

#Reading the required csv files.
demand <- read.csv("demand.csv", stringsAsFactors = FALSE)
demand.dump <- read.csv("dump2.csv", stringsAsFactors = FALSE)
demand.dump$quarter <- quarter(dmy(demand.dump$Approval.Date))
demand.dump$year <- year(dmy(demand.dump$Approval.Date))
demand.dump$month <- month(dmy(demand.dump$Approval.Date))
datasetexp<-read.csv("excel1.csv", stringsAsFactors = FALSE)
colors <- c('#4AC6B7', '#2457C5', '#DF0B0B',"#24C547", '#E71BB6')
indiadistance<-read.csv("indaiusa Distance1.csv")
demandda<-demand.dump
alternatives<-read.csv("alternatives.csv")
rowman<-read.csv("ronnames chan1.csv")
dd<-read.csv("consolidated_skills1.csv", stringsAsFactors = FALSE)
cons <- read.csv("Consolidated.csv", stringsAsFactors = F)

#Initial data preparation for skill radar.
row.names(indiadistance)<-rowman$actual
colnames(indiadistance)<-rowman$actual
customer<-as.data.frame(unique(demandda$Customer))
names(customer)<-"customer" 
colnames(dd)<-rowman$actual
dd1<-dd
dd1$customer<-demandda$Customer
skill<-colnames(dd)
tdd<- t(dd)
tdddataframe<-data.frame(tdd,stringsAsFactors=FALSE)
tdd1<-tdddataframe
cons$date <- dmy(cons$Req.Date)
cons$week <- quarter(cons$date)
cons$year <- year(cons$date)
dem <- cons


##################################################Newmancodes##########################################

#####Like Me Fuctions for first module: Skill Radar

#Function for automatic filtering of the UI input with respect to customer

list_customer<- function (customer){
  
  if (customer!=""){
    f<- as.data.frame( dd1[dd1$customer==customer,-1])
    d<-f[, colSums(f != 0) > 0]
    skill_list<-colnames(d) } #returns the skill list that correspond to the input customer
  
  else {
    skill_list<-c("",as.character(unique(colnames(dd1))))  
  }
  
  return(skill_list)  
  
}

#Function for automatic filtering of the UI input with respect to skill

act_customer<- function (skill){
  
  if (skill!=""){
    cust_list<- c("", as.character(unique( dd1$customer[ dd1[,skill]>0])))
  }
  
  else {
    cust_list<-c("",as.character(unique((demandda$Customer))))  
  }
  return (cust_list)#returns the customer list that correspond to the input skil
}

#Function for automatic filtering of the UI input with respect to skill

act_skill<- function (skill){
  
  if (skill!=""){
    cust_list<- c("", as.character(unique( demandda$Skill.Bucket[ dd1[,skill]>0])))
  }
  
  else {
    cust_list<-c("",as.character(unique((demandda$Skill.Bucket))))  
  }
  return (cust_list)#returns the skill bucket list that correspond to the input skil
}

#Function for automatic filtering of the UI input with respect to skill

act_location<- function (skill){
  
  if (skill!=""){
    cust_list<- c("", as.character(unique( demandda$Personal.SubArea[ dd1[,skill]>0])))
  }
  
  else {
    cust_list<-c("",as.character(unique((demandda$Personal.SubArea))))  
  }
  return (cust_list)#returns the subarea  list that correspond to the input skil
}

#Function for automatic filtering of the UI input with respect to customer

list_skillbucket<- function (customer){
  
  if (customer!=""){
    
    skill_list<-unique(demandda$Skill.Bucket[demandda$Customer==customer])
  }
  
  else {
    skill_list<-c("",as.character(unique(demandda$Skill.Bucket)))  
  }
  
  return(skill_list)  #returns the skill bucket list that correspond to the input customer
  
}

#Function for automatic filtering of the UI input with respect to customer
list_location<- function (customer){
  
  if(customer!=""){
    
    skill_list<-unique(demandda$Personal.SubArea[demandda$Customer==customer])
  }
  
  else {
    skill_list<-c("",as.character(unique(demandda$Personal.SubArea)))  
  }
  
  return(skill_list)  #returns the subarea list that correspond to the input customer
  
}


#Function for Skill radar computes distance by Pearson corelation and makes out the radar 

newman<-function(input, n, skillbucket, subarea,customer,raio){
  
  # Only if Skill is  mentioned by the user compute the closest skills 
  if (input!=""){
    
    #Receiving all the user input and filtering the job descriptions 
    A<-1:nrow(demandda)
    if (customer!=""){
      A<-which(demandda$Customer == customer)}
    B<-1:nrow(demandda)
    if (subarea!=""){
      B<-which(demandda$Personal.SubArea == subarea)}
    C<-1:nrow(demandda)
    if (skillbucket!="") {
      C<-which(demandda$Skill.Bucket==skillbucket)}
    
    D<-intersect(A,B)
    E<-intersect(D,C)
    tdddataframe<-as.data.frame(tdddataframe[,E]) #Final filtered table
    row.names(tdddataframe)<-skill
    
    #Adding an additional empty column
    no<-length(tdddataframe)+1
    tdddataframe[,no]<-0
    d<- tdddataframe[input,]
    #TO know how the total frequency of the word in all the job decsriptions
    coun<-d[, colSums(d == 0)== 0]
    freq<- length(coun)
    
    #Computing distance using Pearson's Correlation.
    dista <- function(x) ((1-cor(t(x)))/2)
    jd<-length(tdddataframe)-1
    
    #if no filters are applied to the dataframe then use the correlation matrix uploaded
    if (jd==31049){
      
      #print("using India Distance")
      distmatrix<-indiadistance
    }
    #Compution of the Pearson correlation between the skills for the filtered matrix
    else {
      d1 <- dista(tdddataframe)
      distmatrix<-as.data.frame(d1)    
    }
    
    
    #Seperate out the disatnce  of input  
    Skills_new<-as.data.frame(distmatrix[,input])
    str(Skills_new)
    names(Skills_new)<-"dist"
    Skills_new$skills<-skill
    
    #apply the threshold
    data1<-Skills_new$skills[(Skills_new$dist<=0.5)]
    data2<-head( (Skills_new[order(Skills_new$dist, decreasing=FALSE),]),n)
    data2<- data2[data2$skills!=input,]
    data<-intersect(data1,data2$skills)
    
    data2<-data2[is.element(data2$skills,data),]
    data2<- data2[order(data2$dist, decreasing=FALSE),]
    data2$dist<-as.numeric(lapply(data2$dist, function(x) 1-x)) #distance computation by perason colrrelation
    
    #Preparation of the table for displaying in Radar format
    d<-max(data2$dist)+0.02
    data2$max<-d
    f<-min(data2$dist)-0.02
    data2$min<-f
    data3<-data2[c(4,3,1)]
    tra<-data.frame(t(data3))
    
    names(tra)<- data2$skills
    
    
    return(list(tra, jd, freq))
  }
  # Only if Skill is not  mentioned by the user instead uses only customer/skill bucket/area
  else {
    #Receiving all the user input and filtering the job descriptions
    A<-1:nrow(demandda)
    if (customer!=""){
      A<-which(demandda$Customer == customer)}
    B<-1:nrow(demandda)
    if (subarea!=""){
      B<-which(demandda$Personal.SubArea == subarea)}
    C<-1:nrow(demandda)
    if (skillbucket!="") {
      C<-which(demandda$Skill.Bucket==skillbucket)}
    
    D<-intersect(A,B)
    E<-intersect(D,C)
    tdddataframe<-as.data.frame(tdddataframe[,E])
    row.names(tdddataframe)<-skill
    #Adding a row for a reference with all 1s 
    addition<-nrow(tdddataframe)+1
    tdddataframe[addition,]<-1
    #Adding an additional empty column to allow for the computation of statdard deviation 
    no<-length(tdddataframe)+1
    tdddataframe[,no]<-0
    
    freq<- length(tdddataframe)-1
    
    #Computing distance using Pearson's Correlation.
    dista <- function(x) ((1-cor(t(x)))/2)
    
    jd<-length(tdddataframe)-1
    #if no filters are applied to the dataframe then use the correlation matrix uploaded    
    if (jd==31049){
      
      #print("using India Distance")
      distmatrix<-indiadistance
    }
    else {
      d1 <- dista(tdddataframe)
      distmatrix<-as.data.frame(d1)   #computing Perason's correlation 
      
    }
    #Seperate out the disatnce  of the referece vector to the skills  
    Skills_new<-as.data.frame(distmatrix[,addition])
    str(Skills_new)
    names(Skills_new)<-"dist"
    Skills_new$skills<-row.names(tdddataframe)
    
    #apply the threshold
    data1<-Skills_new$skills[which(Skills_new$dist<=0.5)]
    data2<-head( (Skills_new[order(Skills_new$dist, decreasing=FALSE),]),n)
    data2<- data2[data2$skills!=addition,]
    data<-intersect(data1,data2$skills)
    
    data2<-data2[is.element(data2$skills,data),]
    data2<- data2[order(data2$dist, decreasing=FALSE),]
    data2$dist<-as.numeric(lapply(data2$dist, function(x) 1-x))#distance computation
   
    #preparation of the table for the radar output
    d<-max(data2$dist)+0.02
    data2$max<-d
    f<-min(data2$dist)-0.02
    data2$min<-f
    data3<-data2[c(4,3,1)]
    tra<-data.frame(t(data3))
    
    names(tra)<- data2$skills
    
    return(list(tra, jd, freq))
    
  }
  
  
  
  
}

#retrieve the alternatie skills  
alter<-function (name){
  
  return(alternatives$alternate[alternatives$Skillname==name])
}
#retrieve the definition
defin<-function (name){
  
  return(alternatives$definition[alternatives$Skillname==name])
}



##############################################Contextual Search################################
likeme <- function(skill1, job1, exp1, stype1, sk.ill, num1,clack, functional, systems){    
  
  
  setwd("C:/HCL/LikeMe")
  
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(caret)
  library(quanteda)
  
  skills <- read.csv("skillClustering.csv", header = TRUE, stringsAsFactors = FALSE)
  stp <- read.csv("stopwords.csv", header = TRUE, stringsAsFactors = FALSE)
  
  #reading the candidate profiles from internal and external databases as per the user input
  if(stype1 == "eser"){
    candidates <- read.csv("external.csv", stringsAsFactors = FALSE)
    original <-  read.csv("external.csv", stringsAsFactors = FALSE)
    if(sk.ill == "I have already entered the skills"){
      candidates$requirement <- candidates$Profile#Add the skills
    }else{
      candidates$requirement <- paste("",candidates$Profile )
    }
  }else if(stype1 == "iser"){
    candidates <- read.csv("internal.csv", stringsAsFactors = FALSE)
    original <-  read.csv("internal.csv", stringsAsFactors = FALSE) 
    if(sk.ill == "I have already entered the skills"){
      candidates$requirement <- candidates$Profile#Add the skills
    }else{
      candidates$requirement <- paste("",candidates$Profile )
    }
  }
  
  if(exp1 == "No Preference"){
    candidates <- candidates
    original <- original
  }else{
    candidates <- subset(candidates, candidates$experience == exp1)
    original <- subset(original, original$experience == exp1)
  }
  
  
  if(sk.ill == "I have already entered the skills"){
    new_requirement <- data.frame(X = nrow(candidates)+1,File_Name = "",Mobile.Number = 9999999999,Email = "",Profile = job1, Education = "",Skills = skill1, TProfile = "")
    new_requirement$requirement <- paste(new_requirement$Skills, new_requirement$Profile)
  }else{
    new_requirement <- data.frame(X = nrow(candidates)+1,File_Name = "",Mobile.Number = 999999999,Email = "",  Profile = job1, Education = "",Skills = paste(colnames(newman(sk.ill, num1, "","",clack)),collapse = ","), TProfile = "")
    new_requirement$requirement <- paste(new_requirement$Skills, new_requirement$Profile)
  }
  
  
  candidates <- rbind(new_requirement, candidates)
  
  #functions for tf idf computation
  term.frequency <- function(row) {
    
    row / sum(row)
    
  }
  
  
  inverse.doc.freq <- function(col) {
    corpus.size <- length(col)
    doc.count <- length(which(col > 0))
    
    log10(corpus.size / doc.count)
  }
  
  
  
  tf.idf <- function(x, idf) {
    x * idf
  }
  
  candidates$TProfile <- as.character(candidates$TProfile)
  candidates$TProfile[1] <- skill1
  
  #tokenisation of the profiles
  
  tokens <- tokens(as.character(new_requirement$Skills), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  tokens <- tokens_tolower(tokens)
  
  
  tokens <- tokens_select(tokens, stp$TEXT, selection = "remove")
  
  train.tokens.dfm <- dfm(tokens, tolower = FALSE)
  
  tokens <- tokens_wordstem(tokens, language = "english")
  
  tokens <- tokens_ngrams(tokens, n = 1)
  
  skills.tokens <- tokens(skills$value, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  skills.tokens <- tokens_tolower(skills.tokens)
  skills.tokens <- tokens_select(skills.tokens, stp$TEXT, selection = "remove")
  skills.tokens <- tokens_ngrams(skills.tokens, n = 1:5)
  skills.tokens <- tokens_select(tokens, unlist(as.list(skills.tokens)), selection = "keep")
  skills.tokens <- tokens_select(skills.tokens, stopwords(), selection = "remove")
  tokens.set <- append(tokens, skills.tokens)
  tokens1 <- tokens(as.character(candidates$TProfile), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  tokens1 <- tokens_tolower(tokens1)
  tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
  tokens1 <- tokens_ngrams(tokens1, n = 1)
  
  tokens1 <- tokens_select(tokens1, unlist(as.list(tokens)), selection = "keep")
  
  tokens.dfm <- dfm(tokens1, tolower = FALSE)
  tokens.matrix <- as.matrix(tokens.dfm)
  
  tokens.matrix[tokens.matrix>0]<-1
  tokens.df <- as.data.frame(tokens.matrix)
  
  
  tokens <- as.matrix(tokens.df)
  
  tokens <- t(tokens)
  
  
  library(lsa)
  start.time <- Sys.time()
  if(nrow(candidates)>1){
    
    #Finding Cosine Similarity.
    cos <- cosine(tokens)
    cos <- as.data.frame(cos)
    score1 <- data.frame(File = candidates$File_Name,Mobile.Number = candidates$Mobile.Number,Email = candidates$Email, score = cos$text1)
    
    score1 <- score1[order(score1$score, decreasing = TRUE),]
    names <- data.frame(File = original$File_Name, Email = original$Email, Mobile.Number = original$Email, Skill = original$Skills)
    score1 <- left_join(score1, names, by = "File")
    colnames(score1) <- c("File","Mobile.Number", "Email", "Score", "em"," em1","Skill")
    if(nrow(score1)==0){
      score1 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
    }
  }else{
    score1 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
  }
  total.time <- Sys.time() - start.time
  total.time
  score1$Score[is.nan(score1$Score)] <- 0
  score1 <- score1[order(score1$Email, decreasing = TRUE),]
  
  if(grepl("^\\s*$", job1)){
    score2 <- data.frame(File = candidates$File_Name,Mobile.Number = candidates$Mobile.Number,Email = candidates$Email, Score = rep(0,nrow(candidates)))
  }else{
    tokens1 <- tokens(candidates$requirement, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens1 <- tokens_tolower(tokens1)
    tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
    new.tokens <- tokens(as.character(new_requirement$Profile), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    new.tokens <- tokens_tolower(new.tokens)
    new.tokens <- tokens_select(new.tokens, stopwords(), selection = "remove")
    new.tokens <- tokens_ngrams(new.tokens, n = 1:5)
    tokens1 <- tokens_ngrams(tokens1, n = 1:5)
    
    tokens1 <- tokens_select(tokens1, unlist(as.list(new.tokens)), selection = "keep")
    new.tokens1 <- tokens(as.character(new_requirement$Skills), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    new.tokens1 <- tokens_tolower(new.tokens1)
    new.tokens1 <- tokens_select(new.tokens1, stopwords(), selection = "remove")
    new.tokens1 <- tokens_ngrams(new.tokens1, n = 1:5)
    tokens1 <- tokens_select(tokens1, unlist(as.list(new.tokens1)), selection = "remove")
    
    tokens.dfm <- dfm(tokens1, tolower = FALSE)
    tokens.matrix <- as.matrix(tokens.dfm)
    tokens.df <- as.data.frame(tokens.matrix)
    
    tokens.df <- apply(tokens.matrix, 1, term.frequency)
    tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
    if(length(tokens.idf)>1){
      tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
    }else{
      tokens.tfidf <- tokens.df*tokens.idf
    }
    tokens.tfidf <- t(tokens.tfidf)
    incomplete.cases <- which(!complete.cases(tokens.tfidf))
    tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(tokens.tfidf))
    tokens.df <- as.data.frame(tokens.tfidf)
    
    tokens <- as.matrix(tokens.df)
    
    tokens <- t(tokens)
    
    library(lsa)
    start.time <- Sys.time()
    if(nrow(candidates)>1){
      cos <- cosine(tokens)
      cos <- as.data.frame(cos)
      score2 <- data.frame(File = candidates$File_Name,Mobile.Number = candidates$Mobile.Number,Email = candidates$Email, score = cos$text1)
      score2 <- score2[order(score2$score, decreasing = TRUE),]
      names <- data.frame(File = original$File_Name,Email = original$Email, Mobile.Number = original$Email, Skill = original$Skills)
      score2 <- left_join(score2, names, by = "File")
      colnames(score2) <- c("File","Mobile.Number", "Email", "Score", "em"," em1","Skill")
      if(nrow(score2)==0){
        score2 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
      }
    }else{
      score2 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
    }
    total.time <- Sys.time() - start.time
    total.time
    
    score2$Score[is.nan(score2$Score)] <- 0
    score2 <- score2[order(score2$Email, decreasing = TRUE),]
  }
  score1$scores <- score2$Score
  score1$cumulative <- score1$Score+score1$scores
  scoring <- function(candidates, context){
    candidates$Profile <- as.character(candidates$Profile)
    candidates$Profile[1] <- context
    
    tokens1 <- tokens(candidates$Profile, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens1 <- tokens_tolower(tokens1)
    tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
    new.tokens <- tokens(as.character(context), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    new.tokens <- tokens_tolower(new.tokens)
    new.tokens <- tokens_select(new.tokens, stopwords(), selection = "remove")
    new.tokens <- tokens_ngrams(new.tokens, n = 1:5)
    tokens1 <- tokens_ngrams(tokens1, n = 1:5)
    
    tokens1 <- tokens_select(tokens1, unlist(as.list(new.tokens)), selection = "keep")
    
    tokens.dfm <- dfm(tokens1, tolower = FALSE)
    tokens.matrix <- as.matrix(tokens.dfm)
    tokens.df <- as.data.frame(tokens.matrix)
    tokens.df <- apply(tokens.matrix, 1, term.frequency)
    tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
    tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
    tokens.tfidf <- t(tokens.tfidf)
    incomplete.cases <- which(!complete.cases(tokens.tfidf))
    tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(tokens.tfidf))
    tokens.df <- as.data.frame(tokens.tfidf)
    
    tokens <- as.matrix(tokens.df)
    
    tokens <- t(tokens)
    
    if(nrow(candidates)>1){
      cos <- cosine(tokens)
      cos <- as.data.frame(cos)
      score <- data.frame(File = candidates$File_Name,Mobile.Number = candidates$Mobile.Number,Email = candidates$Email, score = cos$text1)
      score <- score[order(score$score, decreasing = TRUE),]
      names <- data.frame(File = original$File_Name,Email = original$Email, Mobile.Number = original$Email, Skill = original$Skills)
      score <- score[,c(1,4)]
      if(nrow(score)==0){
        score <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
      }
    }else{
      score <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
    }
    
    return(score)
  }
  
  if(grepl("^\\s*$", functional)){
    functional_score <- data.frame(File = score1$File, score = rep(0,nrow(score1)))
  }else{
    functional_score <- scoring(candidates, functional)
  }
  if(grepl("^\\s*$", systems)){
    systems_score <- data.frame(File = score1$File, score = rep(0,nrow(score1)))
  }else{
    systems_score <- scoring(candidates, systems)
  }
  
  score1 <- left_join(score1,functional_score,by = 'File')
  score1 <- left_join(score1,systems_score,by = 'File')
  
  score1$cscores <- score1$score.x+score1$score.y
  score1$cumulative <- score1$cumulative+score1$cscores
  score1 <- score1[order(score1$cumulative, decreasing = TRUE),]
  score1 <- subset(score1, score1$File!="")
  score1 <- subset(score1, score1$Score>0.5)
  score1$Mob <- NULL
  score1$Skill<-NULL
  colnames(score1) <- c("File","Mobile Number","Email","Skill Score         (Out of 1)", 
                        "Skill","em","Context Score        (Out of 1)",
                        "Cumulative Score          (Out of 5)",
                        "Functional Score          (Out of 1)",
                        "Systems Score          (Out of 1)",
                        "FSC Score          (Out of 3)")
  score1$Skill<-NULL
  score1$em<-NULL
  score1 <- score1[1:5,]
  score1$`Skill Score         (Out of 1)` <- round(score1$`Skill Score         (Out of 1)`, digits = 2)
  score1$`Context Score        (Out of 1)` <- round(as.numeric(score1$`Context Score        (Out of 1)`), digits = 2)
  score1$`Cumulative Score          (Out of 5)` <- round(as.numeric(score1$`Cumulative Score          (Out of 5)`), digits = 2)
  score1$`Functional Score          (Out of 1)`<- round(as.numeric(score1$`Functional Score          (Out of 1)`),digits = 2)
  score1$`Systems Score          (Out of 1)`<- round(as.numeric(score1$`Systems Score          (Out of 1)`),digits = 2)
  score1$`FSC Score          (Out of 3)`<- round(as.numeric(score1$`FSC Score          (Out of 3)`),digits = 2)
  if(grepl("^\\s*$", job1)){
    score1$`Context Score        (Out of 1)`<-NULL
  }
  if(grepl("^\\s*$", functional)){
    score1$`Functional Score          (Out of 1)`<-NULL
  }
  if(grepl("^\\s*$", systems)){
    score1$`Systems Score          (Out of 1)`<-NULL
  }
  if(grepl("^\\s*$", functional) & grepl("^\\s*$", systems) ){
    score1$`FSC Score          (Out of 3)`<- NULL
  }
  if(grepl("^\\s*$", functional) & grepl("^\\s*$", systems)  & grepl("^\\s*$", job1)){
    score1$`Cumulative Score          (Out of 5)`<- NULL
  }
  
  if(nrow(score1)>0){
    tokens <- tokens(as.character(new_requirement$Skills), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens <- tokens_tolower(tokens)
    tokens <- tokens_select(tokens, stp$TEXT, selection = "remove")
    train.tokens.dfm <- dfm(tokens, tolower = FALSE)
    tokens <- tokens_wordstem(tokens, language = "english")
    tokens <- tokens_ngrams(tokens, n = 1)
    
    tokens1 <- tokens(as.character(candidates$TProfile), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens1 <- tokens_tolower(tokens1)
    tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
    tokens1 <- tokens_ngrams(tokens1, n = 1)
    
    skilltokens <- list()
    aaa <- character(0)
    for(i in 1:nrow(candidates)){
      if(!identical(aaa,unlist(tokens1[i]))){
        skilltokens[i] <- paste(tokens_select(tokens, unlist(as.list(tokens1[i])), selection = "remove"),collapse = ",")
      }else{
        skilltokens[i]<-"" 
      }
    }
    
    score3 <- data.frame(File = candidates$File_Name, Skills.not.present = unlist(skilltokens))
    
    
    
    score1 <- left_join(score1, score3, by = "File")
  }
  return(score1)
}

######################################Like me - Job board search#################################################
#Function to search the job board for alternative skills.
jobboard<-function(skill1,skill2,skill3) {
  
  #Receiving all the inputs as a list
  l<-{}
  l<-append(l,skill1)
  l<-append(l,skill2)
  l<-append(l,skill3)
  l<-l[l!=""]
  
  len<-length(l)
  
  
  #creation of a dataframe
  a_dummy<-data.frame(l)
  names(a_dummy)<-"keywords"
  a_dummy$no_of_searches<-0
  a_dummy$closely_related_skill_Dice_Insights_Dice_Insights<-0
  a_dummy$link<-0
  
  #web scrapping from dice insights
  for (i in 1:len){
    d<-gsub(" ", "+", l[i], fixed=TRUE)
    if (d=="Cascading+Style+Sheets+(CSS)"){
      d<-gsub("(.*? )", "", a_dummy$l[i])}
    if (d=="c"){d<-"C+C%2B%2B"}
    if (d=="c++"){d<-"C+C%2B%2B"}
    if (d=="vc++"){d<-"vc%2B%2B"}
    if (d=="embedded"){d<-"embedded+system"}
    if (d=="c#"){d<-"c%23"}
    
    #closest skill module
    url2  <- paste("https://www.dice.com/skills/",d,".html", sep="")
    movie2<-read_html(url2)
    g1 <- movie2 %>% html_node(".col-md-7") %>% html_text()
    s1<-gsub("\\\t", "", g1)
    s1<-gsub("\\\n", " ", s1)
    s1<-gsub("\\Related Skills", "", s1)
    a_dummy$closely_related_skill_Dice_Insights[i]<-s1
    a_dummy$link[i]<-url2
    
  }
  
  
  ddd<-a_dummy[,c("keywords", "closely_related_skill_Dice_Insights")]
  
  
  return(ddd)
}



############################################Customer Forecast#################################################
#Function used to forecast the demand for customer
cust.forecast <- function(a,b,c, country){
  cust.forecast <- Sys.time()
  
  #Selecting data based on the region selected.
  if(country=="India"){
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="INDIA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")
  }else{
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="USA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")  
  }
  
  
  setwd("C:/HCL/LikeMe/Demand")
  master.demand <- read.csv("dump.csv")
  #print("Start Maps")
  demand.area <- master.demand
  demand.area$quarter <- quarter(dmy(demand.area$Approval.Date))
  demand.area$year <- year(dmy(demand.area$Approval.Date))
  demand.area$month <- month(dmy(demand.area$Approval.Date))
  if(a!="All"){
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket, demand.area$Customer), FUN = sum)
    colnames(demand.area) <- c("Quarter", "Year", "Skill", "Customer", "Demand")
  }else{
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year, demand.area$Customer), FUN = sum)  
    colnames(demand.area) <- c("Quarter", "Year", "Customer", "Demand")
  }
  demand.area$time <- paste("Q",demand.area$Quarter,"-",demand.area$Year)
  
  
  if(a!="All"){
    Total <- subset(demand.area, demand.area$Skill == a & Year == c & Quarter ==b)
  }else{
    Total <- subset(demand.area,  Year == c & Quarter ==b)  
  }
  
  Total$Demand[is.na(Total$Demand)] <- 0
  Total <- data.frame(Customer = Total$Customer, Demand = Total$Demand)
  Total <- Total[order(Total$Demand,decreasing = TRUE),]
  Total <- subset(Total, Total$Demand!=0)
  Total <- Total[1:10,]
  
  forecasting <- function(cust){
    setwd("C:/HCL/LikeMe/Demand")
    demand <- read.csv("dump.csv",stringsAsFactors = F)
    
    demand$date <- dmy(demand$Req.Date)
    demand$quarter <- quarter(demand$date)
    demand$month <- month(demand$date)
    demand$year <- year(demand$date)
    demand$week <- week(demand$date)
    
    dates <- demand
    
    if(a!="All"){
      demand <- demand %>% filter(demand$Skill.Bucket == a)
    }
    location.demand <- aggregate(demand$InitialDemand, by=list(demand$Customer), FUN = sum)
    location.demand <- location.demand[order(location.demand$x, decreasing = T),]
    location.demand <- location.demand[1:3,]$Group.1
    
    demand <- demand %>% filter(tolower(demand$Customer) == tolower(cust))
    if(nrow(demand)==0){
      return(0)
    }else{
      demand <- aggregate(demand$InitialDemand, by = list(demand$week, demand$year), FUN = sum)
      colnames(demand) <- c("Week","Year","Demand")
      
      setwd("C:/HCL/LikeMe")
      template <- read.csv("template2015.csv")
      colnames(template) <- c("Year", "Week")
      demand <- merge(template, demand, all = TRUE)
      demand$Demand[is.na(demand$Demand)] <- 0
      
      if(month(max(dates$date)) %in% c(1,2,3)){
        n <- length(unique(dates$year))-1
        n <- n*52
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = n))
      }
      if(month(max(dates$date)) %in% c(4,5,6)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+13
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(7,8,9)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+26
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(10,11,12)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+38
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      
      
      return(round(sum(forecast(auto.arima(demand.ts),h=12)$mean[1:12])))
    }
    
  }
  
  
  
  toplocation <- Total$Customer
  toplocation <- lapply(toplocation,function(x)forecasting(x))
  Total$'Forecast for the Next Quarter' <- unlist(toplocation)
  return(Total)
  
  print(Sys.time() - cust.forecast.time)
}




############################################Combination Forecast############################################
#Function to forecast the demand for the different combinations of location and customers.
combopred <- function(a,b,c, country){    
  if(country=="India"){
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="INDIA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")
  }else{
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="USA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")  
  }
  
  setwd("C:/HCL/LikeMe/Demand")
  master.demand <- read.csv("dump.csv")
  demand.area <- master.demand
  demand.area$date <- dmy(demand.area$Approval.Date)
  demand.area$quarter <- quarter(dmy(demand.area$Approval.Date))
  demand.area$year <- year(dmy(demand.area$Approval.Date))
  demand.area$month <- month(dmy(demand.area$Approval.Date))
  demand.area$week <- week(dmy(demand.area$Approval.Date))
  dem <- demand.area
  
  if(a!="All"){
    demand.location <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket,demand.area$Personal.SubArea), FUN = sum)
    colnames(demand.location) <- c("Quarter","Year", "Skill", "Location", "Demand")
    demand.customer <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket,demand.area$Customer), FUN = sum)
    colnames(demand.customer) <- c("Quarter","Year", "Skill", "Customer", "Demand")
  }else{
    demand.location <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket,demand.area$Personal.SubArea), FUN = sum)
    colnames(demand.location) <- c("Quarter","Year", "Skill", "Location", "Demand")
    demand.customer <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket,demand.area$Customer), FUN = sum)
    colnames(demand.customer) <- c("Quarter","Year", "Skill", "Customer", "Demand")
  }
  
  demand.location$time <- paste("Q",demand.area$Quarter,"-",demand.area$Year)
  demand.customer$time <- paste("Q",demand.area$Quarter,"-",demand.area$Year)
  
  
  if(a!="All"){
    Total.location <- subset(demand.location, Year == c & Quarter ==b & demand.location$Skill == a)
    Total.location <- Total.location[order(Total.location$Demand, decreasing = T),]
    Total.location <- Total.location$Location[1:5]
    Total.customer <- subset(demand.customer, Year == c & Quarter ==b & demand.customer$Skill == a)
    Total.customer <- Total.customer[order(Total.customer$Demand, decreasing = T),]
    Total.customer <- Total.customer$Customer[1:5]
  }else{
    Total.location <- subset(demand.location, Year == c & Quarter ==b)
    Total.location <- Total.location[order(Total.location$Demand, decreasing = T),]
    Total.location <- Total.location$Location[1:5]
    Total.customer <- subset(demand.customer, Year == c & Quarter ==b)
    Total.customer <- Total.customer[order(Total.customer$Demand, decreasing = T),]
    Total.customer <- Total.customer$Customer[1:5]
  }
  
  grid <- expand.grid(Total.location, Total.customer)
  colnames(grid) <- c("Location","Customer")
  
  combination.forecasting<-function(Locat,Custo){
    demand <- dem
    dates <- dem
    
    if(a!="All"){
      demand <- subset(demand, demand$Skill.Bucket==a)
    }
    demand <- subset(demand, demand$Personal.SubArea==Locat)
    demand <- subset(demand, demand$Customer==Custo)
    if(nrow(demand)==0){
      return("No Such Combination")
    }else{
      demand <- aggregate(demand$InitialDemand, by = list(demand$week, demand$year), FUN =sum)
      colnames(demand) <- c("Week","Year","Demand")
      setwd("C:/HCL/LikeMe")
      template <- read.csv("template2015.csv")
      colnames(template) <- c("Year", "Week")
      demand <- merge(template, demand, all = TRUE)
      demand$Demand[is.na(demand$Demand)] <- 0
      
      if(month(max(dates$date)) %in% c(1,2,3)){
        n <- length(unique(dates$year))-1
        n <- n*52
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = n))
      }
      if(month(max(dates$date)) %in% c(4,5,6)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+13
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(7,8,9)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+26
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(10,11,12)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+38
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(round(sum(forecast(auto.arima(demand.ts),h=12)$mean[1:12]))<0){
        return("Not Predictable")
      }else{
        return(round(sum(forecast(auto.arima(demand.ts),h=12)$mean[1:12])))
      }
      
    }
    
  }
  if(a!="All"){
    Total <- data.frame(Skill = rep(a,nrow(grid)), grid, Forecast = mapply(combination.forecasting, grid$Location, grid$Customer))
    Total <- subset(Total, Total$Forecast != "No Such Combination")
    Total <- subset(Total, Total$Customer != "Others")
  }else{
    Total <- data.frame(No_Skill_Selected = "No Skill Selected so the predictions cannot be made for Customer and Location combinations if a Skill was not selected")
  }
  return(Total)
}    


###########################################DSM+################################################################
#Function to forecast the overall demand
forecaster <- function(skill.input, country){

  require(tibble)
  require(dplyr)
  require(caret)
  require(randomForest)
  require(lubridate)
  require(tseries)
  require(forecast)
  require(quanteda)
  require(zoo)
 
  if(country=="India"){
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="INDIA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")
  }else{
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="USA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")  
  }
  setwd("C:/HCL/LikeMe/Demand")
  
  #Read the demand data file from the folder.
  master <- read.csv("demand.csv", header = TRUE, stringsAsFactors = FALSE)
  
  #Create a variable called Total Fulfilled.
  master$Total.Fulfilled <- master$Internal_Filled+master$External_Joined
  
  #Create a variable called Unfulfilled Overdue.
  master$Unfulfilled.Overdue <- master$InitialDemand-(master$Internal_Filled+master$External_Joined+master$DroppedPos)
  
  #Select columns that is needed for analysis and import them.
  master <- master[,c("X", "ReqNo", "Joining.Level.2","Customer","Segment",
                      "Req.Date","Skill.Bucket","Primary.Skill.Area","Requisition.Source",
                      "Internal_Filled","External_Joined","Total.Fulfilled",
                      "Unfulfilled.Overdue","Vacancy","DroppedPos","InitialDemand","vAdditionalRemarks","Personal.SubArea")]
  
  #Remove observations from the data that do not have any requisition date.
  master <- master[complete.cases(master$Req.Date),]
  
  
  #Modifying the column names.
  colnames(master) <- c("data.src","srn","l2","customer","segment","date","skill","sr.skill","req.sor",
                        "int.ful","ext.ful","tot.ful","un.od","net.demand","demand.drop",
                        "overall.demand","job.desc","Location")
  
  #Changing the classes of the variables.
  master$date <- dmy(master$date)
  master$data.src <- factor(master$data.src)
  master$l2 <- factor(master$l2)
  master$segment <- factor(master$segment)
  master$skill <- factor(master$skill)
  master$req.sor <- factor(master$req.sor)
  
  
  master1 <- master
  master1$month <- month(master1$date)
  master1$year <- year(master1$date)
  
  #Removing duplicates.
  master <- master[!duplicated(master),]
  
  new.demand <- master
  master.length <- nrow(master)
  new.length <- nrow(new.demand)
  master.demand <- rbind(master, new.demand)
  master.demand <- master.demand[!duplicated(master.demand),]
  master.demand$requirement <- paste(master.demand$sr.skill,master.demand$job.desc)
  
  
  
  #Use the package "quanteda" to work with the text data.
  #tokenize the requirements.
  full.tokens <- tokens(master.demand$requirement, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  
  
  
  #Lower case the tokens.
  full.tokens <- tokens_tolower(full.tokens)
  
  
  
  #Removing stop words.
  full.tokens <- tokens_select(full.tokens, stopwords(), selection = "remove")
  
  
  
  #performing stemming on the requirement text.
  full.tokens <- tokens_wordstem(full.tokens, language = "english")
  
  
  
  #Create bag of words.
  full.tokens.dfm <- dfm(full.tokens, tolower = FALSE)
  
  
  
  #Transform to matrix.
  full.tokens.matrix <- as.matrix(full.tokens.dfm)
  
  
  
  #Convert to dataframe.
  full.tokens.df <- data.frame(full.tokens.matrix)
  
  
  #Binding the skill bucket as the class label 
  full.tokens.df$class.label <- master.demand$skill

  skills.list <- skill.input

  #Check the whether there is any new demand that has been added. If present,
  #1. Bucket the demand or,
  #2. Forecast the demand directly
  if(1==2){
    #Split and bucket the new demand.
    #train <- full.tokens.df[1:master.length,]
    
    #Separate the new demand.
    test <- full.tokens.df[master.length+1:nrow(full.tokens.df),]
    
    
    #Load the model that was created.
    load("C:/Users/varun/Desktop/jije.RData")
    #Train Random Forest
    #rf.train <- randomForest(class.label~.-req.no-l2.name, data = train)
    
    #Predict the buckets using the model that was cre.
    rf.predict <- predict(model, test)
    
    #Add the predictions to the test dataset.
    test$class.label <- rf.predict
    
    #Bind the train and test.
    train.test <- rbind(train,test)
    
    #Add the skills back to the master demand.
    master.demand$skill <- train.test$class.label
    
    
    #Creating "month" and "year"
    master.demand$week <- week(master.demand$date)
    master.demand$month <- month(master.demand$date)
    master.demand$year <- year(master.demand$date)
    master.demand$mon_year <- as.yearmon(master.demand$date)
    master.demand$quarter <- quarter(master.demand$date)
    
    
    #Subset data after 2016 and subset the demands A & C.
    demand.2016 <- subset(master.demand, year>2015)
    demand.2016 <- subset(demand.2016, segment == "A" | segment == "C")
    
    #Creating a skill list.
    net.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    ovr.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    tot.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    
    for(i in 1:1){
      
      #Function used for predicting the demand.
      prediction <- function(ovrdemand.agg, ext.agg,int.agg, totful.agg){
        
        colnames(ovrdemand.agg) <- c("month","year","demand")
        colnames(ext.agg) <- c("month","year","demand")
        colnames(int.agg) <- c("month","year","demand")
        colnames(totful.agg) <- c("month","year","demand")
        
        #Finding the last month and year.
        ovrdemand.agg <- ovrdemand.agg[-c(nrow(ovrdemand.agg))]
        ext.agg <- ext.agg[-c(nrow(ext.agg))]
        int.agg <- int.agg[-c(nrow(int.agg))]
        totful.agg <- totful.agg[-c(nrow(totful.agg))]
        
        
        #Convert data to time series.
        
        ovr.demandseries <- ts(ovrdemand.agg$demand, frequency = 52)
        ext.demandseries <- ts(ext.agg$demand, frequency = 52)
        int.demandseries <- ts(int.agg$demand, frequency = 52)
        tot.demandseries <- ts(totful.agg$demand, frequency = 52)
        
        order <- read.csv("order.csv")
        if(skills.list!="All"){
          order <- subset(order, order$skill == skills.list)
        }else{
          order<-order
        }
        
        ovr.forecast <- forecast(auto.arima(ovr.demandseries), h = 12)
        
        final.results <- data.frame(month = c("Month 1", "Month 2","Month 3"), 
                                    overall = c(sum(ovr.forecast$mean[1:4]),sum(ovr.forecast$mean[5:8]),sum(ovr.forecast$mean[9:12]))) 
                                   
        
        return(final.results)
      }
      
      #subset the demand by skill.
      skill.demand <- subset(demand.2016, demand.2016$skill == skills.list)
      
      
      #Aggregate the demand.
     
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      ovrdemand.agg <- ovrdemand.agg[1:52,]
      ext.agg <- ext.agg[1:52,]
      int.agg <- int.agg[1:52,]
      totful.agg <- totful.agg[1:52,]
      
      #Predict for JFM
      jfm <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      ovrdemand.agg <- ovrdemand.agg[1:64,]
      ext.agg <- ext.agg[1:64,]
      int.agg <- int.agg[1:64,]
      totful.agg <- totful.agg[1:64,]
      
      #Predict for AMJ
      amj <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      ovrdemand.agg <- ovrdemand.agg[1:76,]
      ext.agg <- ext.agg[1:76,]
      int.agg <- int.agg[1:76,]
      totful.agg <- totful.agg[1:76,]
      
      #Predict the JAS
      jas <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      jfm <- rbind(jfm,amj)
      jfm <- rbind(jfm,jas)
      
    }
  }else{
    #Creating "month" and "year"
    master.demand$week <- week(master.demand$date)
    master.demand$month <- month(master.demand$date)
    master.demand$year <- year(master.demand$date)
    master.demand$mon_year <- as.yearmon(master.demand$date)
    master.demand$quarter <- quarter(master.demand$date)
    #write.csv(master.demand, "master.csv")
    
    
    #Subset data after 2016 and subset the demands A & C.
    demand.2016 <- subset(master.demand, year>2015)
    demand.2016 <- subset(demand.2016, segment == "A" | segment == "C")
    
    #Creating a skill list.
    
    net.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    ovr.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    tot.results = data.frame( month = character() , Skill = character(), Seasonal_Naive = numeric())
    
    for(i in 1:1){
      
      prediction <- function(ovrdemand.agg, ext.agg,int.agg, totful.agg){
       
        
        #Finding the last month and year.
        
        ovrdemand.agg <- ovrdemand.agg[-c(nrow(ovrdemand.agg))]
        ext.agg <- ext.agg[-c(nrow(ext.agg))]
        int.agg <- int.agg[-c(nrow(int.agg))]
        totful.agg <- totful.agg[-c(nrow(totful.agg))]
        
        
        #Convert data to time series.
        
        ovr.demandseries <- ts(ovrdemand.agg$demand, frequency = 52)
        ext.demandseries <- ts(ext.agg$demand, frequency = 52)
        int.demandseries <- ts(int.agg$demand, frequency = 52)
        tot.demandseries <- ts(totful.agg$demand, frequency = 52)
        
        order <- read.csv("order.csv")
        order <- subset(order, order$skill == as.character(skills.list))
        
        ovr.forecast <- forecast(auto.arima(ovr.demandseries), h = 12)
        
       
        
        final.results <- data.frame(month = c("Month 1"), 
                                    overall = c(sum(ovr.forecast$mean[1:12]))) 
        
        return(final.results)
      }
      
      if(skills.list!="All"){
        skill.demand <- subset(demand.2016, demand.2016$skill == as.character(skills.list))
      }else{
        skill.demand <- demand.2016
      }
      
      #Aggregate the demand.
     
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      colnames(ovrdemand.agg) <- c("week","year","demand")
      colnames(ext.agg) <- c("week","year","demand")
      colnames(int.agg) <- c("week","year","demand")
      colnames(totful.agg) <- c("week","year","demand")
      
      setwd("C:/HCL/LikeMe")
      template <- read.csv("template2015.csv")
      colnames(template) <- c("year", "week")
      ovrdemand.agg <- merge(template, ovrdemand.agg, all = TRUE)
      ovrdemand.agg$demand[is.na(ovrdemand.agg$demand)] <- 0
      
      
      ovrdemand.agg <- ovrdemand.agg[1:52,]
      ext.agg <- ext.agg[1:52,]
      int.agg <- int.agg[1:52,]
      totful.agg <- totful.agg[1:52,]
      
      #Prediction in JFM
      jfm <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      colnames(ovrdemand.agg) <- c("week","year","demand")
      colnames(ext.agg) <- c("week","year","demand")
      colnames(int.agg) <- c("week","year","demand")
      colnames(totful.agg) <- c("week","year","demand")
      
      template <- read.csv("template2015.csv")
      colnames(template) <- c("year", "week")
      ovrdemand.agg <- merge(template, ovrdemand.agg, all = TRUE)
      ovrdemand.agg$demand[is.na(ovrdemand.agg$demand)] <- 0
      
      ovrdemand.agg <- ovrdemand.agg[1:64,]
      ext.agg <- ext.agg[1:64,]
      int.agg <- int.agg[1:64,]
      totful.agg <- totful.agg[1:64,]
      
      #Prediction for April, May and June.
      amj <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$week, skill.demand$year), FUN = sum)
      
      colnames(ovrdemand.agg) <- c("week","year","demand")
      colnames(ext.agg) <- c("week","year","demand")
      colnames(int.agg) <- c("week","year","demand")
      colnames(totful.agg) <- c("week","year","demand")
      
      
      template <- read.csv("template2015.csv")
      colnames(template) <- c("year", "week")
      ovrdemand.agg <- merge(template, ovrdemand.agg, all = TRUE)
      ovrdemand.agg$demand[is.na(ovrdemand.agg$demand)] <- 0
      
      if(month(max(master.demand$date)) %in% c(1,2,3)){
        n <- length(unique(master.demand$year))-1
        n <- n*52
        ovrdemand.agg <- ovrdemand.agg[1:n,]
        ext.agg <- ext.agg[1:n,]
        int.agg <- int.agg[1:n,]
        totful.agg <- totful.agg[1:n,]
      }
      if(month(max(master.demand$date)) %in% c(4,5,6)){
        n <- length(unique(master.demand$year))-1
        n <- (n*52)+(13)
        ovrdemand.agg <- ovrdemand.agg[1:n,]
        ext.agg <- ext.agg[1:n,]
        int.agg <- int.agg[1:n,]
        totful.agg <- totful.agg[1:n,]
      }
      if(month(max(master.demand$date)) %in% c(7,8,9)){
        n <- length(unique(master.demand$year))-1
        n <- (n*52)+(26)
        ovrdemand.agg <- ovrdemand.agg[1:n,]
        ext.agg <- ext.agg[1:n,]
        int.agg <- int.agg[1:n,]
        totful.agg <- totful.agg[1:n,]
      }
      if(month(max(master.demand$date)) %in% c(10,11,12)){
        n <- length(unique(master.demand$year))-1
        n <- (n*52)+(38)
        ovrdemand.agg <- ovrdemand.agg[1:n,]
        ext.agg <- ext.agg[1:n,]
        int.agg <- int.agg[1:n,]
        totful.agg <- totful.agg[1:n,]
      }
      #Prediction for July, August and September.
      jas <- prediction(ovrdemand.agg, ext.agg, int.agg, totful.agg)
      
      jfm <- rbind(jfm,amj)
      jfm <- rbind(jfm,jas)
      
    }
  }
  
  #Condidtion to check whether the prediction is for a skill or the complete data.
  if(skills.list!="All"){
    skill.demand <- subset(master1, master1$skill == as.character(skills.list))
  }else{
    skill.demand <- master1
  }
  
  #Subset the demand for years greater than 2015.
  skill.demand <- subset(skill.demand, skill.demand$year >2015)
  skill.demand$quarter <- quarter(skill.demand$date)
  
  #Subset the demand for the segments A and C.
  skill.demand <- subset(skill.demand, skill.demand$segment == "A" | skill.demand$segment == "C")
  
  #Merge all the results together into one dataframe.
  if(nrow(skill.demand)!= 0){
    #Aggregate the overall demand, external fulfillment, internal fulfillment and total fulfillment.
    ovrdemand.agg <- aggregate(skill.demand$overall.demand, by = list(skill.demand$quarter, skill.demand$year), FUN = sum)
    ext.agg <- aggregate(skill.demand$ext.ful, by = list(skill.demand$quarter, skill.demand$year), FUN = sum)
    int.agg <- aggregate(skill.demand$int.ful, by = list(skill.demand$quarter, skill.demand$year), FUN = sum)
    totful.agg <- aggregate(skill.demand$tot.ful, by = list(skill.demand$quarter, skill.demand$year), FUN = sum)
    
    #Bind all the aggregations together.
    ovrdemand.agg <- cbind(ovrdemand.agg, ext.agg$x)
    ovrdemand.agg <- cbind(ovrdemand.agg, int.agg$x)
    ovrdemand.agg <- cbind(ovrdemand.agg, totful.agg$x)
    colnames(ovrdemand.agg) <- c("quarter","year","overall","external","internal","total")
  } else{
    ovrdemand.agg <- data.frame(month = rep("month", 5), year = rep("2017",5), overall = rep("0",5), external = rep("0",5), internal = rep("0",5), total = rep("0",5))
  }
  #write.csv(ovrdemand.agg, "original.csv")
  
  jfm$year <- rep(2017,3)
  colnames(jfm) <- c("Quarter","Demand","Year")
  jfm$Demand <- round(jfm$Demand)
  setwd("C:/HCL/LikeMe")
  qy <- read.csv("quarteryear.csv")
  ovrdemand.agg <- merge(qy,ovrdemand.agg, all=TRUE)
  ovrdemand.agg <- ovrdemand.agg[order(ovrdemand.agg$quarter),]
  ovrdemand.agg <- ovrdemand.agg[order(ovrdemand.agg$year),]
  
 
  if(month(max(master.demand$date)) %in% c(1,2,3)){
    jfm$Quarter<- "Q1 - JFM"
  }else if(month(max(master.demand$date)) %in% c(4,5,6)){
    jfm$Quarter <- "Q2 - AMJ"
  }else if(month(max(master.demand$date)) %in% c(7,8,9)){
    jfm$Quarter <- "Q3 - JAS"
  }else if(month(max(master.demand$date)) %in% c(10,11,12)){
    jfm$Quarter <- "Q4 - OND"
  }
  
  return(jfm)
}

#Create the data for maps for ploting data.
maptable <- function(a,b,c, country){
  if(country=="India"){
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="INDIA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")
  }else{
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="USA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")  
  }
  #Set the working directory to the Demand folder.
  setwd("C:/HCL/LikeMe/Demand")
  master.demand <- read.csv("dump.csv")

  demand.area <- master.demand
  demand.area$quarter <- quarter(dmy(demand.area$Approval.Date))
  demand.area$year <- year(dmy(demand.area$Approval.Date))
  if(a!="All"){
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket, demand.area$Personal.SubArea), FUN = sum)
    colnames(demand.area) <- c("Quarter", "Year", "Skill", "Location", "Demand")
  }else{
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year, demand.area$Personal.SubArea), FUN = sum)  
    colnames(demand.area) <- c("Quarter", "Year", "Location", "Demand")
  }
 
  demand.area$time <- paste("Q",demand.area$Quarter,"-",demand.area$Year)
  
  #Getting the list of Ststes in the Unites Sates of America.
  all_states <- map_data("county")
  
  #Renaming the columns
  colnames(all_states) <- c("long","lat", "group", "order", "Location", "subregion")
  
  #Converting the location to lower case
  demand.area$Location <- tolower(demand.area$Location)
  if(a!="All"){
    Total <- subset(demand.area, demand.area$Skill == a & Year == c & Quarter ==b)
  }else{
    Total <- subset(demand.area,  Year == c & Quarter ==b)  
  }
  #Total <- merge(all_states, demand.area,all = TRUE)
  Total <- Total[Total$Location!="district of columbia",]
  
  setwd("C:/HCL/LikeMe")
  states <- read.csv("states.csv")
  colnames(states) <- c("Column1", "long", "lat", "order", "hole", "piece", "Location", "group")
  st <- data.frame(Location = unique(map_data('county')$region))
  Total <- merge(st, Total, all = TRUE)
  Total$Demand[is.na(Total$Demand)] <- 0
  Total <- merge(states, Total, all = TRUE)
  Total$Demand[is.na(Total$Demand)] <- 0
  Total <- data.frame(State = Total$Location, Demand = Total$Demand)
  Total <- subset(Total, Total$State != "district of columbia")
  
  #Demand for all the states have been calculated.
  Total <- Total[1:50,]
  #print("stop maptable")
  return(Total)
  
}

#Function to create a heat map
maps <- function(a,b,c, country){
  if(country=="India"){
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="INDIA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")
  }else{
    setwd("C:/HCL/LikeMe") 
    demand <- read.csv("dump2.csv", header = TRUE, stringsAsFactors = FALSE)
    demand <- subset(demand, demand$country=="USA")
    setwd("C:/HCL/LikeMe/Demand") 
    write.csv(demand,"demand.csv")
    write.csv(demand, "dump.csv")  
  }
  setwd("C:/HCL/LikeMe/Demand")
  master.demand <- read.csv("dump.csv")
  #print("Start Maps")
  demand.area <- master.demand
  demand.area$quarter <- quarter(dmy(demand.area$Approval.Date))
  demand.area$year <- year(dmy(demand.area$Approval.Date))
  demand.area$month <- month(dmy(demand.area$Approval.Date))
  if(a!="All"){
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year,demand.area$Skill.Bucket, demand.area$Personal.SubArea), FUN = sum)
    colnames(demand.area) <- c("Quarter", "Year", "Skill", "Location", "Demand")
  }else{
    demand.area <- aggregate(demand.area$InitialDemand, by = list(demand.area$quarter,demand.area$year, demand.area$Personal.SubArea), FUN = sum)  
    colnames(demand.area) <- c("Quarter", "Year", "Location", "Demand")
  }
  demand.area$time <- paste("Q",demand.area$Quarter,"-",demand.area$Year)
  
  
  all_states <- map_data("county")
  colnames(all_states) <- c("long","lat", "group", "order", "Location", "subregion")
  demand.area$Location <- tolower(demand.area$Location)
  if(a!="All"){
    Total <- subset(demand.area, demand.area$Skill == a & Year == c & Quarter ==b)
  }else{
    Total <- subset(demand.area,  Year == c & Quarter ==b)  
  }
  Total <- Total[Total$Location!="district of columbia",]
  
  setwd("C:/HCL/LikeMe")
  states <- read.csv("states.csv")
  colnames(states) <- c("Column1", "long", "lat", "order", "hole", "piece", "Location", "group")
  st <- data.frame(Location = unique(map_data('county')$region))
  Total <- merge(st, Total, all = TRUE)
  Total$Demand[is.na(Total$Demand)] <- 0
  Total <- merge(states, Total, all = TRUE)
  Total$Demand[is.na(Total$Demand)] <- 0
  Total <- data.frame(State = Total$Location, Demand = Total$Demand)
  Total <- subset(Total, Total$State != "district of columbia")
  Total <- Total[order(Total$Demand,decreasing = TRUE),]
  Total <- subset(Total, Total$Demand!=0)
  Total <- Total[1:5,]
  
  
  forecasting <- function(loca){
    setwd("C:/HCL/LikeMe/Demand")
    #print(loca)
    demand <- read.csv("dump.csv",stringsAsFactors = F)
    
    demand$date <- dmy(demand$Req.Date)
    demand$quarter <- quarter(demand$date)
    demand$month <- month(demand$date)
    demand$year <- year(demand$date)
    demand$week <- week(demand$date)
    
    dates <- demand
  
    if(a!="All"){
      demand <- demand %>% filter(demand$Skill.Bucket == a)
    }
    location.demand <- aggregate(demand$InitialDemand, by=list(demand$Personal.SubArea), FUN = sum)
    location.demand <- location.demand[order(location.demand$x, decreasing = T),]
    location.demand <- location.demand[1:3,]$Group.1
    
    demand <- demand %>% filter(tolower(demand$Personal.SubArea) == tolower(loca))
    if(nrow(demand)==0){
      return("No forecast available.")
    }else{
      demand <- aggregate(demand$InitialDemand, by = list(demand$week, demand$year), FUN = sum)
      colnames(demand) <- c("Week","Year","Demand")
      
      setwd("C:/HCL/LikeMe")
      template <- read.csv("template2015.csv")
      colnames(template) <- c("Year", "Week")
      demand <- merge(template, demand, all = TRUE)
      demand$Demand[is.na(demand$Demand)] <- 0
      
      if(month(max(dates$date)) %in% c(1,2,3)){
        n <- length(unique(dates$year))-1
        n <- n*52
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = n))
      }
      if(month(max(dates$date)) %in% c(4,5,6)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+13
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(7,8,9)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+26
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      if(month(max(dates$date)) %in% c(10,11,12)){
        n <- length(unique(dates$year))-1
        n <- (n*52)+38
        demand.ts <- tsclean(ts(demand[1:n,]$Demand,frequency = 52))
      }
      
      return(round(sum(forecast(auto.arima(demand.ts),h=12)$mean[1:12])))
    }
    
  }
  
  
  
  toplocation <- Total$State
  toplocation <- lapply(toplocation,function(x)forecasting(x))
  Total$'Forecast for Next Quarter' <- unlist(toplocation)
  Total <- subset(Total, !is.na(Total$Demand))
  return(Total)
  
  
}

##############################################Customer Trend####################################

customer <- function(cid, year, quarter, number){
  
  demand <- read.csv("master.csv")
  
  customer.demand <- aggregate(demand$overall.demand, 
                               by = list(demand$quarter, demand$year, demand$customer),
                               FUN = sum)
  
  customer.select <- filter(customer.demand, 
                            customer.demand$Group.1 == quarter, customer.demand$Group.2 == year, customer.demand$Group.3 == customer)
  
  customer.notselect <- filter(customer.demand, 
                               customer.demand$Group.1 == quarter, customer.demand$Group.2 == year, customer.demand$Group.3 != customer)
  
  customer.notselect <- customer.notselect[order(customer.notselect$x, decreasing = TRUE),]
  
  customer.together <- rbind(customer.select, customer.notselect)
  
  colnames(customer.together) <- c("Quarter", "Year", "Customer", "Demand")
  
  
  return(customer.together[1:number,])
  
}


##############################################Skill Vs Customer#########################################
custskill1 <- function(c, d, e){
  setwd("C:/HCL/Likeme/Demand")
  library(lubridate)
  library(ggplot2)
  library(plotly)
  custmaster <- read.csv("demand.csv", header = TRUE, stringsAsFactors = FALSE)
  #Create a variable called Total Fulfilled.
  custmaster$Total.Fulfilled <- custmaster$Internal_Filled+custmaster$External_Joined
  
  #Create a variable called Unfulfilled Overdue.
  custmaster$Unfulfilled.Overdue <- custmaster$InitialDemand-(custmaster$Internal_Filled+custmaster$External_Joined+custmaster$DroppedPos)
  
  #Select columns that is needed for analysis and import them.
  custmaster <- custmaster[,c("X", "ReqNo", "Joining.Level.2","Customer","Segment",
                              "Req.Date","Skill.Bucket","Primary.Skill.Area","Requisition.Source",
                              "Internal_Filled","External_Joined","Total.Fulfilled",
                              "Unfulfilled.Overdue","Vacancy","DroppedPos","InitialDemand","vAdditionalRemarks","Personal.SubArea")]
  
  #Remove observations from the data that do not have any requisition date.
  custmaster <- custmaster[complete.cases(custmaster$Req.Date),]
  
  
  #Modifying the column names.
  colnames(custmaster) <- c("data.src","srn","l2","customer","segment","date","skill","sr.skill","req.sor",
                            "int.ful","ext.ful","tot.ful","un.od","net.demand","demand.drop",
                            "overall.demand","job.desc","Location")
  
  
  custmaster$date <- dmy(custmaster$date)
  custmaster$data.src <- factor(custmaster$data.src)
  custmaster$l2 <- factor(custmaster$l2)
  custmaster$segment <- factor(custmaster$segment)
  custmaster$skill <- factor(custmaster$skill)
  custmaster$req.sor <- factor(custmaster$req.sor)
  custmaster$quarter <- as.numeric(quarter(custmaster$date))
  custmaster$year <- year(custmaster$date)
  
  if(c!="All"){
    fil.year <- subset(custmaster, custmaster$year == d & custmaster$quarter == e & custmaster$skill == c)
  }else{
    fil.year <- subset(custmaster, custmaster$year == d & custmaster$quarter == e ) 
  }
  agg.year <- aggregate(fil.year$overall.demand, by = list(fil.year$customer, fil.year$segment), FUN = sum)
  colnames(agg.year) <- c("Customer","Segement", "Demand")
  agg.year <- agg.year[order(agg.year$Demand, decreasing = TRUE),]
  agg.year <- agg.year[1:10,]
  if(sum(is.na(agg.year$Demand))>0){
    agg.year <- agg.year[!is.na(agg.year$Demand),]
  }
  return(agg.year)
  
  
  
}

###############################################Dashboard tabs#####################################
tabs <- function(f,g,h){
  setwd("C:/HCL/LikeMe/Demand")
  library(lubridate)
  library(ggplot2)
  library(plotly)
  master <- read.csv("demand.csv", header = TRUE, stringsAsFactors = FALSE)
  master$Total.Fulfilled <- master$Internal_Filled+master$External_Joined
  master$Unfulfilled.Overdue <- master$InitialDemand-(master$Internal_Filled+master$External_Joined+master$DroppedPos)
  
  master <- master[,c("X", "ReqNo", "Joining.Level.2","Customer","Segment",
                      "Req.Date","Skill.Bucket","Primary.Skill.Area","Requisition.Source",
                      "Internal_Filled","External_Joined","Total.Fulfilled",
                      "Unfulfilled.Overdue","Vacancy","DroppedPos","InitialDemand","vAdditionalRemarks","Personal.SubArea")]
  master <- master[complete.cases(master$Req.Date),]
  colnames(master) <- c("data.src","srn","l2","customer","segment","date","skill","sr.skill","req.sor",
                        "int.ful","ext.ful","tot.ful","un.od","net.demand","demand.drop",
                        "overall.demand","job.desc","Location")
  
  
  master$date <- dmy(master$date)
  master$data.src <- factor(master$data.src)
  master$l2 <- factor(master$l2)
  master$segment <- factor(master$segment)
  master$skill <- factor(master$skill)
  master$req.sor <- factor(master$req.sor)
  master$quarter <- quarter(master$date)
  master$year <- year(master$date)
  
  if(f!="All"){
    fil.year <- subset(master, master$year == g & master$quarter == h & master$skill == f)
    ovr.demand <- aggregate(fil.year$overall.demand, by = list(fil.year$skill), FUN = sum)
    ful.demand <- aggregate(fil.year$tot.ful, by = list(fil.year$skill), FUN = sum)
    drop.demand <- aggregate(fil.year$demand.drop, by = list(fil.year$skill), FUN = sum)
    unful.demand <- aggregate(fil.year$un.od, by = list(fil.year$skill), FUN = sum)
  }else{
    fil.year <- subset(master, master$year == g & master$quarter == h)
    ovr.demand <- aggregate(fil.year$overall.demand, by = list(fil.year$year,fil.year$quarter), FUN = sum)
    ful.demand <- aggregate(fil.year$tot.ful, by = list(fil.year$year,fil.year$quarter), FUN = sum)
    drop.demand <- aggregate(fil.year$demand.drop, by = list(fil.year$year,fil.year$quarter), FUN = sum)
    unful.demand <- aggregate(fil.year$un.od, by = list(fil.year$year,fil.year$quarter), FUN = sum)
  }
  table.demaned <- data.frame(Overall = ovr.demand$x, Ful = ful.demand$x, drop = drop.demand$x, un.ud =unful.demand$x )
  table.demaned$ful.per <- round((table.demaned$Ful/table.demaned$Overall)*100)
  table.demaned$drop.per <- round((table.demaned$drop/table.demaned$Overall)*100)
  table.demaned$od.per <- round((table.demaned$un.ud/table.demaned$Overall)*100)
  
  return(table.demaned)
}


############################################Recommendation System#########################################
candidate_recommendation <- function(j){  
  setwd("C:/HCL/Demand Forecast")
  library(quanteda)
  library(dplyr)
  
  demand <- read.csv("demand.csv", stringsAsFactors = FALSE)    
  demand$date <- as.Date(demand$Req.Date, "%m/%d/%Y")
  demand$open.days <- as.Date(Sys.Date(), "%m/%d/%Y")-demand$date
  demand <- subset(demand, demand$Skill.Bucket == j)
  demand <- subset(demand, demand$Data.Source == "Due or Overdue demands at the end of the month")
  demand <- demand[order(demand$open.days, decreasing = TRUE),]
  demand <- demand[!duplicated(demand$SR.No),]
  demand <- demand[1:10,]
  demand$rqrmt <- paste(demand$SR.Skill, demand$Requirement)
  
  recommendations <- function(rqrmt){
    setwd("C:/HCL/LikeMe")
    library(dplyr)
    library(lubridate)
    library(stringr)
    library(caret)
    library(quanteda)
    
    skills <- read.csv("skillClustering.csv", header = TRUE, stringsAsFactors = FALSE)
    stp <- read.csv("stopwords.csv", header = TRUE, stringsAsFactors = FALSE)
    setwd("C:/HCL/LikeMe/Resumes/External")
    candidates <- read.csv("external.csv", stringsAsFactors = FALSE)
    original <-  read.csv("external.csv", stringsAsFactors = FALSE)
    
    candidates$requirement <- paste(candidates$Skills, candidates$TProfile)
    
    
    candidates <-  select(candidates,File_Name, Skills, TProfile, requirement)#, Customer.Flag, experience.flag, designation.flag, l2.flag, Employee.Code)
    
    #print("Adding Requirement")
    new_requirement <- data.frame(File_Name = "999999",Skills = "sndmnvs",TProfile = "sajshdb", requirement = rqrmt)
    
    new_requirement <-  select(new_requirement,File_Name, Skills,TProfile,  requirement)#, Customer.Flag, experience.flag, designation.flag, l2.flag, Employee.Code)
    
    candidates <- rbind(new_requirement, candidates)
    term.frequency <- function(row) {
      
      row / sum(row)
      
    }
    
    
    inverse.doc.freq <- function(col) {
      corpus.size <- length(col)
      doc.count <- length(which(col > 0))
      
      log10(corpus.size / doc.count)
    }
    
    
    
    tf.idf <- function(x, idf) {
      x * idf
    }
    
    
    tokens <- tokens(as.character(new_requirement$requirement), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens <- tokens_tolower(tokens)
    
    
    tokens <- tokens_select(tokens, stp$TEXT, selection = "remove")
    
    train.tokens.dfm <- dfm(tokens, tolower = FALSE)
    
    tokens <- tokens_wordstem(tokens, language = "english")
    
    tokens <- tokens_ngrams(tokens, n = 1:5)
    
    skills.tokens <- tokens(skills$value, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    skills.tokens <- tokens_tolower(skills.tokens)
    skills.tokens <- tokens_select(skills.tokens, stp$TEXT, selection = "remove")
    skills.tokens <- tokens_ngrams(skills.tokens, n = 1:5)
    skills.tokens <- tokens_select(tokens, unlist(as.list(skills.tokens)), selection = "keep")
    skills.tokens <- tokens_select(skills.tokens, stopwords(), selection = "remove")
    tokens.set <- append(tokens, skills.tokens)
    
    tokens1 <- tokens(as.character(candidates$requirement), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens1 <- tokens_tolower(tokens1)
    tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
    tokens1 <- tokens_ngrams(tokens1, n = 1:5)
    
    tokens1 <- tokens_select(tokens1, unlist(as.list(skills.tokens)), selection = "keep")
    
    tokens.dfm <- dfm(tokens1, tolower = FALSE)
    tokens.matrix <- as.matrix(tokens.dfm)
    tokens.df <- as.data.frame(tokens.matrix)
  
    tokens.df <- apply(tokens.matrix, 1, term.frequency)
    tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
    tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
    tokens.tfidf <- t(tokens.tfidf)
    incomplete.cases <- which(!complete.cases(tokens.tfidf))
    tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(tokens.tfidf))
    tokens.df <- as.data.frame(tokens.tfidf)
    
    tokens <- as.matrix(tokens.df)
    
    tokens <- t(tokens)
    
    
    library(lsa)
    #print("Scoring")
    start.time <- Sys.time()
    if(nrow(candidates)>1){
      cos <- cosine(tokens)
      cos <- as.data.frame(cos)
      score1 <- data.frame(File_Name = candidates$File_Name, score = cos$text1)
      
      score1 <- score1[order(score1$score, decreasing = TRUE),]
      names <- data.frame(File_Name = original$File_Name, Name = original$Full_Name, skill = original$Skills, experience = original$Years.Exp, previous.employer = original$TProfile)
      score1 <- left_join(score1, names, by = "File_Name")
      colnames(score1) <- c("File Name", "Score", "Candidate Name", "Skills"," Experience", "Current Employer")
      if(nrow(score1)==0){
        score1 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
      }
    }else{
      score1 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
    }
    total.time <- Sys.time() - start.time
    total.time
    
    score1 <- score1[order(score1$`Candidate Name`, decreasing = TRUE),]
    
    tokens1 <- tokens(as.character(candidates$requirement), what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    tokens1 <- tokens_tolower(tokens1)
    tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
    tokens1 <- tokens_ngrams(tokens1, n = 1:5)
    
    tokens1 <- tokens_select(tokens1, unlist(as.list(tokens.set)), selection = "keep")
    
    tokens.dfm <- dfm(tokens1, tolower = FALSE)
    tokens.matrix <- as.matrix(tokens.dfm)
    tokens.df <- as.data.frame(tokens.matrix)
    tokens.df <- apply(tokens.matrix, 1, term.frequency)
    tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
    tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
    tokens.tfidf <- t(tokens.tfidf)
    incomplete.cases <- which(!complete.cases(tokens.tfidf))
    tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(tokens.tfidf))
    tokens.df <- as.data.frame(tokens.tfidf)
    
    tokens <- as.matrix(tokens.df)
    
    tokens <- t(tokens)
    
    
    library(lsa)
    
    start.time <- Sys.time()
    if(nrow(candidates)>1){
      cos <- cosine(tokens)
      cos <- as.data.frame(cos)
      score2 <- data.frame(File_Name = candidates$File_Name, score = cos$text1)
      
      score2 <- score2[order(score2$score, decreasing = TRUE),]
      names <- data.frame(File_Name = original$File_Name, Name = original$Full_Name, skill = original$Skills, experience = original$Years.Exp, previous.employer = original$TProfile)
      score2 <- left_join(score2, names, by = "File_Name")
      colnames(score2) <- c("Employee Code", "Score", "Candidate Name", "Skills"," Experience", "Current Employer")
      if(nrow(score2)==0){
        score2 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
      }
    }else{
      score2 <- data.frame(NO = character(), MATCHING = character(), PROFILE = character(), FOUND = character())
    }
    total.time <- Sys.time() - start.time
    total.time
    
    score2 <- score2[order(score2$`Candidate Name`, decreasing = TRUE),]
    
    score1$scores <- score2$Score
    score1$cumulative <- score1$Score+score1$scores
    score1 <- score1[order(score1$cumulative, decreasing = TRUE),]
    score1 <- subset(score1, score1$Score<1.0 & score1$scores<1.0)
    
    return(score1$`Candidate Name`[1:5])
  }
  
  
  
    demand$int.names <- lapply(demand$rqrmt,function (x) unlist( recommendations(x)))
  demand$int.names <- vapply(demand$int.names, paste, collapse = ", ", character(1L))
  demand$ext.names <- lapply(demand$rqrmt,function (x) unlist( recommendations(x)))
  demand$ext.names <- vapply(demand$ext.names, paste, collapse = ", ", character(1L))
  demand <- demand[,c("SR.No","Skill.Bucket","Customer.Name","open.days","Requirement","int.names","ext.names")]
  colnames(demand) <- c("SR NO", "Skill Bucket","Customer","Open Days", "Job Description","Internal Candidates","External Candidates")
  return(demand)
}

################################################Clue#############################################
#Meaning and atternate skills pulled from the alternate keywords
clue<- function(skillword){
  
  if (length(tech$path[tolower(tech$Titile)==tolower(skillword)])==0){return("NA") }
  else {
    return(as.character(tech$path[tolower(tech$Titile)==tolower(skillword)]))
  }
  
}

#Creation of UI 

ui <- dashboardPage(#skin = "blue",
  
  dashboardHeader(title = "Recruitment Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Like - Me", menuSubItem("Skill Radar", tabName = "skill", icon = icon("puzzle-piece")),
               menuSubItem("Job Board Search", tabName = "search3", icon = icon("search")),
               #menuSubItem("Content Based Search", tabName = "search1", icon = icon("search")),
               menuSubItem("Context Based Search", tabName = "search2", icon = icon("search-plus")),
               menuSubItem("Candidate Radar", tabName = "reco", icon = icon("search-plus")),icon = icon("id-card")
      ),
      menuItem("DSM +", 
               #menuSubItem("Demand Forecast", tabName = "demand", icon = icon("line-chart")),
               #menuSubItem("Location based Demand", tabName = "location"),
               menuSubItem("Skill based Insights", tabName = "customer"),
               menuSubItem("Skill Popularity", tabName = "popularity"),
               icon = icon("bar-chart"))
      
    )
  ),
  dashboardBody(tags$head(tags$style(HTML('.content{
                                          background-color: white;
                                          } 
                                          .skin-blue .main-header .navbar{
                                          background-color:#003da5}
                                          .skin-blue .main-header .logo{
                                          background-color:#003da5                                  
                                          }
                                          .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a{
                                          border-left-color:#003da5                                        
                                          }
                                          h1{
                                          font-family:"Cambria"
                                          }'))),
      
                tabItems(
                  tabItem(tabName = "reco",
                          tags$h1("Candidate Radar"),
                          fluidRow(
                            box(
                              title = "Get recommendations for the oldest Job Descriptions that are open.",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              selectInput("recoskill","Select Skill Bucket",choices = sort(unique(demand$Skill.Bucket))), 
                              actionButton(inputId = "recogo",label = "Recommend",color="red")
                              
                            ),
                            mainPanel( DT::dataTableOutput("recoresults"))
                          )
                          
                  ),
                  tabItem(tabName = "search3",
                          tags$h1("Job Board Search"),
                          fluidRow(
                            box(
                              title = "Search the web for alternative skills.",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              textInput("kill1","Keyword 1",""),
                              textInput("kill2","Keyword 2",""),
                              textInput("kill3","Keyword 3",""),
                              actionButton(inputId = "go6",label = "generate Keywords",color="red")
                            ),
                            mainPanel( DT::dataTableOutput("results2"))
                          )),
                  tabItem(tabName = "demand",
                          tags$h1("Forecast Demand"),
                          fluidRow(
                            box(
                              title = "Demand Forecast Input",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              selectInput("1skill","Select Skill",choices = sort(unique(demand$Skill.Bucket))), 
                              actionButton(inputId = "go2", label = "Forecast Demand")
                            ),
                            
                            box(
                              title = "Actual Vs Forecast Plot",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              collapsed = TRUE,
                              plotOutput("coolplot")),
                            mainPanel(DT::dataTableOutput("results"))
                            
                          )),
                  
                  tabItem(tabName = "location",
                          tags$h1("Location based Demand"),
                          fluidRow(
                            box(
                              title = "Select Skill, Year and Quarter.",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              selectInput("skill1","Select Skill",choices = sort(unique(demand$Skill.Bucket))), 
                              selectInput("year","Select Year",choices = c(2014,2015,2016,2017)), 
                              selectInput("quarter","Select Quarter",choices = c(1,2,3,4)),
                              actionButton(inputId = "go3", label = "Get Demand", color = "red")
                            ),
                            
                            box(
                              title = "Demand based on Location in the US",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("map1")),
                            
                            box(
                              title = "Demand Statistics based on Location",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              collapsed = TRUE,
                              DT::dataTableOutput("maptable1"))
                          )
                  ),
                  tabItem(tabName = "about",
                          tags$h1("HCL's Recruitment Analytics Tool"),
                          tags$h3("A project undertaken to enhance recruiting and insert analytics for futureproofing Talent acquisition "),
                          tags$br(),
                          tags$h1("Like - Me:"),
                          tags$h3("Creating sourcing queries and striving to get a" ,tags$em("Content and Context"), "based results .
                                  "),
                          tags$br(),
                          tags$h1("DSM+"),
                          tags$h3("Forecasting demand for On time fulfillment and create supply for",tags$em("heterogeneous"), "demand.")
                          
                          ),
                  tabItem(tabName = "skill",
                          tags$h1("Skill Radar"),
                          tags$h3("Data : 31049 Job descriptions (Jan 2016 to Aug 2017)"),
                          tags$h4("Results available for 582 Customers,33 Skill buckets,
65 different locations,2835 Technological keywords and all their combinations "),
                          fluidRow(
                            box(
                              title = "Input for skill radar",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              uiOutput("series"),
                              uiOutput("varun1"),
                              uiOutput("Box3"),
                              uiOutput("Box4"),
                              uiOutput("Box5"),
                              uiOutput("Box6"),
                              uiOutput("Box7"),
                              valueBoxOutput("frequency")
                              
                              
                            ),
                            box(
                              title = "Skill Radar",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("skills")
                            ),
                            box(
                              title = "Boolean Strings",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              dataTableOutput("skills3")
                              
                            ),
                            # box(
                            #   title = "Customer Radar",
                            #   status = "danger",
                            #   solidHeader = TRUE,
                            #   collapsible = TRUE,
                            #   plotlyOutput("skills2")
                            # ),
                            mainPanel( dataTableOutput("links"))
                            
                            
                          )),
                 tabItem(tabName = "search2",
                          tags$h1("Context Based Search"),
                          fluidRow(
                            box(
                              title = "Search for Candidates based on Skills and Context",
                              status = "danger",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              radioButtons("stype","Where do you want to search?", c("Outside HCL" = "eser", "Inside HCL" = "iser")),
                              textAreaInput("ski.ll", "Enter Skills*"),
                              tags$h3("OR"),
                              selectInput("sk.ill", "Select the Primary Skill*", choices = c("I have already entered the skills",as.character(unique(rowman$actual)))),
                              sliderInput(inputId = "num1", label = "Select the maximum number of skills to be used", value = 6, min=1, max = 50),
                              textAreaInput("job", "Job Description"),
                              textAreaInput("functional", "What are the functional requirements?"),
                              textAreaInput("systems", "What are the system requirements?"),
                              #textAreaInput("composition", "What are the composition requirements?"),
                              selectInput("exp", "How Experienced must the Candidate be?", choices = c("No Preference",unique(datasetexp$experience)[c(1:6,8)])),
                              selectInput("clack","Which customer are you hiring for?",choices = c(" ",unique(demandda$Customer))),
                              actionButton(inputId = "go", label = "Find Profiles")
                            ),
                            
                            
                            mainPanel( DT::dataTableOutput("score"))
                            
                          )
                  ),
                  tabItem(
                    tabName = "customer",
                    tags$h1("Demand Dashboard"),
                    fluidRow(
                      fluidRow(
                        valueBoxOutput("overall",width = 2),
                        valueBoxOutput("fulfillment", width = 2),
                        valueBoxOutput("drop", width = 2),
                        valueBoxOutput("od", width = 2),
                        valueBoxOutput("frcst", width = 2),
                        valueBoxOutput("revenue", width = 2)
                      ),
                      box(
                        title = "Select Skill bucket, year and quarter.",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        radioButtons("custloc","Select the Region for Forecast.", c("India" = "India", "USA" = "USA")),
                        uiOutput("forecast.skill"),
                        uiOutput("forecast.year"),
                        uiOutput("forecast.quarter"),
                        actionButton(inputId = "cust", label = "Go", color = "red")
                      ),
                      box(
                        title = "Top Customers",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("custplot"))
                    ),
                    fluidRow(
                      box(title = "Demand in the United States.",
                          status = "danger",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotlyOutput("plot")
                      ),
                      box(
                        title = "Initial Demand Report for Various Locations.",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        
                        DT::dataTableOutput("maptable")
                      )
                    ),
                    fluidRow(
                      box(title = "Fulfillment for different Location",
                          status = "danger",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotlyOutput("ful.loc")
                      ),
                      box(
                        title = "Fulfillment Percentage for different Customer",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        plotlyOutput("ful.cust")
                        
                      )
                    ),
                    fluidRow(
                      box(title = "Forecast for Combination of Top Skills and Top Customers for the selected skills",
                          status = "danger",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          DT::dataTableOutput("combforecast")
                      ),
                      box(
                        title = "Forecast for the top customers for the selected skill",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        DT::dataTableOutput("custforecast")
                        
                      )
                    )
                  ),
                  tabItem(
                    tabName = "popularity",
                    tags$h1("Popularity Dashboard"),
                    fluidRow(
                      box(
                        title = "Select the country and customer",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        radioButtons("poploc","Select region", c("INDIA" = "INDIA", "USA" = "USA")),
                        uiOutput("varun"),
                        uiOutput("skill.varun"),
                        actionButton(inputId = "popularity", label = "GO", color = "red")
                      )
                      
                      
                    ),
                    fluidRow(
                      box(
                        title = "Top 10 gainers.",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        DT::dataTableOutput("top10gainers")
                      ),
                      box(
                        title = "Top 10 losers.",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        DT::dataTableOutput("top10losers")
                      )
                    ),
                    fluidRow(
                      plotlyOutput("pop.plot")
                    ),
                    fluidRow(
                      DT::dataTableOutput("pop.table")
                    )
                  )
                  
                ),
                tags$a(tags$img(src = "http://www.oneindia.com/img/2015/05/25-1432549894-hcl-logo.jpg", height = 200, width = 400), href= "https://www.hcltech.com/geo-presence/united-states")
                
  ))




#############################Fulfillment Percentage##############################
fulfillment.customer <- function(skill){
  setwd("C:/HCL/LikeMe/Demand")
  master <- read.csv("dump.csv", stringsAsFactors = FALSE)
  
  master$filled <- master$External_Joined+master$Internal_Filled
  master <- subset(master, master$Skill.Bucket!="#N/A")
  master$quarter <- quarter(dmy(master$Approval.Date))
  master$month <- month(dmy(master$Approval.Date))
  master$year <- year(dmy(master$Approval.Date))
  
  if(skill!="All"){
    master.skill <- subset(master, master$Skill.Bucket==skill)
  }else{
    master.skill <- master
  }

  master.skill.initial <- aggregate(master.skill$InitialDemand,by = list(master.skill$Customer, master.skill$Personal.SubArea), FUN = sum)
  master.skill.filled <- aggregate(master.skill$filled,by = list(master.skill$Customer, master.skill$Personal.SubArea), FUN = sum)
  
  master.skill.initial$fulfillment <- (master.skill.filled$x/master.skill.initial$x)*100
  master.skill.initial <- master.skill.initial[order(master.skill.initial$fulfillment,decreasing = TRUE),]
  if(nrow(master.skill.initial)>1){
    master.skill.initial <- subset(master.skill.initial, master.skill.initial$x > mean(master.skill.initial$x))
  }
  master.skill.initial.customer <- aggregate(master.skill.initial$fulfillment, by = list(master.skill.initial$Group.1), FUN = mean)
  master.skill.initial.customer <- master.skill.initial.customer[order(master.skill.initial.customer$x,decreasing = TRUE),]
  return(master.skill.initial.customer)                                                                                                          
}
fulfillment.location <- function(skill){
  setwd("C:/HCL/LikeMe/Demand")
  master <- read.csv("dump.csv", stringsAsFactors = FALSE)
  
  master$filled <- master$External_Joined+master$Internal_Filled
  master <- subset(master, master$Skill.Bucket!="#N/A")
  master$quarter <- quarter(dmy(master$Approval.Date))
  master$month <- month(dmy(master$Approval.Date))
  master$year <- year(dmy(master$Approval.Date))
  
  if(skill!="All"){
    master.skill <- subset(master, master$Skill.Bucket==skill)
  }else{
    master.skill <- master
  }
  master.skill.initial <- aggregate(master.skill$InitialDemand,by = list(master.skill$Customer, master.skill$Personal.SubArea), FUN = sum)
  master.skill.filled <- aggregate(master.skill$filled,by = list(master.skill$Customer, master.skill$Personal.SubArea), FUN = sum)
  
  master.skill.initial$fulfillment <- (master.skill.filled$x/master.skill.initial$x)*100
  master.skill.initial <- master.skill.initial[order(master.skill.initial$fulfillment,decreasing = TRUE),]
  
  if(nrow(master.skill.initial)>1){
    master.skill.initial <- subset(master.skill.initial, master.skill.initial$x > mean(master.skill.initial$x))
  }
  
  master.skill.initial.customer <- aggregate(master.skill.initial$fulfillment, by = list(master.skill.initial$Group.2), FUN = mean)
  master.skill.initial.customer <- master.skill.initial.customer[order(master.skill.initial.customer$x,decreasing = TRUE),]

  library(ggplot2)
  library(plotly)
  
  return(master.skill.initial.customer)                                                                                                        
}

############################################POPULARITY#######################################################
popularity <- function(country,cust, skillbucket){
  cons <- dem
  colnames(cons)[which(names(cons) == "C..")] <- "C++"
  colnames(cons)[which(names(cons) == "C.")] <- "C#"
  #colnames(cons)[which(names(cons) == "C..")] <- "C++"
  
  cons[,137:2972] <- as.data.frame(lapply(cons[,137:2972], function(x){replace(x, x>1,1)}))
  cons[,137:2972] <- cons[,137:2972]*cons$InitialDemand
  
  cons <- cons %>% filter(cons$country==country)
  cons <- cons %>% filter(cons$Customer==cust)
  cons <- cons %>% filter(cons$Skill.Bucket==skillbucket)
  
  max.year <- cons %>% filter(cons$year == max(cons$year))
  min.year <- cons %>% filter(cons$year == max(cons$year))
  
  cq <- quarter(Sys.Date())
  if(cq==1){
    cq = 4
    pq = 3
  }else{
    cq = cq-1
    pq = cq-1
  }
  
  max.year <- max.year %>% filter(max.year$week == cq)
  min.year <- min.year %>% filter(min.year$week == pq)
  
  max.year <- data.frame(colSums(max.year[,137:2972]))
  max.year$skills <- row.names(max.year)
  colnames(max.year) <- c("Value","skills")
  max.year <- max.year[order(max.year$skills, decreasing = T),]
  
  min.year <- data.frame(colSums(min.year[,137:2972]))
  min.year$skills <- row.names(min.year)
  colnames(min.year) <- c("Value","skills")
  min.year <- min.year[order(min.year$skills, decreasing = T),]
  
  skilllist <- cbind(max.year,min.year$Value)
  skilllist$PercentageChange <- ((skilllist$Value- skilllist$`min.year$Value`)/skilllist$`min.year$Value`)*100
  External2 <- skilllist
  
  
  col.sums <- data.frame(colSums(cons[,137:2972]))
  col.sums$skills <- row.names(col.sums)
  colnames(col.sums) <- c("Value","skills")
  col.sums <- col.sums[order(col.sums$Value, decreasing = T),]
  topskills <- col.sums$skills[1:10]
  #col.sums <- head(col.sums$skills,20)
  col.sums <- col.sums$skills[1:5]
  skill.aggregate <- aggregate(cons[,c(col.sums)], by = list(cons$week, cons$year), FUN = sum)
  totalweeks <- ((max(cons$year)-min(cons$year))+1)*52
  weeks <- data.frame(Week = rep(1:4,((max(cons$year)-min(cons$year))+1)))
  years <- data.frame(Year = rep(min(cons$year), 4))
  for(i in 2:((max(cons$year)-min(cons$year))+1)){
    years <- rbind(years, data.frame(Year = rep(min(cons$year)+1,4)))
  }
  weeks <- cbind(years,weeks)
  
  colnames(skill.aggregate) <- c("Week","Year", col.sums)
  weeks <- merge(weeks, skill.aggregate, all = TRUE)
  colnames(weeks) <- c("Year","Week", col.sums)
  weeks[is.na(weeks)] <- 0
  year.today <- year(Sys.Date())
  week.today <- quarter(Sys.Date())
  weeks <- weeks[1:6,]
  weeks$year.quarter<- paste(weeks$Year," - " ,weeks$Week)
  
  More.100 <- subset(External2,External2$PercentageChange>=100 )
  Stable <- subset(External2,External2$PercentageChange==0)
  No.Popularity <- subset(External2,(External2$PercentageChange)*(-1) >=100)
  Top10 <- subset(External2, External2$skills %in% topskills)
  Top10.gainers <- subset(Top10,Top10$PercentageChange >0)
  Top10.losers <- subset(Top10,Top10$PercentageChange < 0 )
  Gainers.Losers <- data.frame(Category = c("More than 100% popularity gain","No Loss No Gain",
                                            "Forgotten Skills", "Highest gain in the top 10 list",
                                            "Highest loss in the top 10 list"))
  Gainers.Losers$Skills <- c(paste(subset(External2,External2$PercentageChange>=100 )$skills, collapse=", "),
                             paste(subset(External2,External2$PercentageChange==0)$skills, collapse=", "),
                             paste(subset(External2,(External2$PercentageChange)*(-1) >=100)$skills, collapse=", "),
                             paste(subset(Top10,Top10$PercentageChange >0)$skills, collapse=", "),
                             paste(subset(Top10,Top10$PercentageChange < 0 )$skills, collapse=", "))
  
  More.100[,c(1,2,3)] <- NULL
  More.100$PercentageChange[is.infinite(More.100$PercentageChange)] <- 100
  More.100$PercentageChange <- round(More.100$PercentageChange)
  Stable[,c(1,2,3)] <- NULL
  Stable$PercentageChange <- round(Stable$PercentageChange)
  No.Popularity[,c(1,2,3)] <- NULL
  No.Popularity$PercentageChange <- round(No.Popularity$PercentageChange)
  Top10.gainers[,c(1,2,3)] <- NULL
  Top10.gainers$PercentageChange <- round(Top10.gainers$PercentageChange)
  if(nrow(Top10.gainers)>0){
    Top10.gainers$PercentageChange <- paste(Top10.gainers$PercentageChange,"%")
  }
  Top10.losers[,c(1,2,3)] <- NULL
  Top10.losers$PercentageChange <- round(Top10.losers$PercentageChange)
  if(nrow(Top10.losers)>0){
    Top10.losers$PercentageChange <- paste(Top10.losers$PercentageChange,"%")
  }
  return(list(weeks,Gainers.Losers, Top10.gainers, Top10.losers))
}


server <- function(input, output, session) {
  
  
  output$series<-renderUI({
    radioButtons("radio","Start Deep Dive with either,", c("Skill" = "Skill","Customer" = "Customer"))
  })
  output$varun1 <- renderUI({
    if (is.null(input$radio))
      return()
    switch(input$radio,"Customer" = selectInput("custa", "Select the Customer",
                                                choices = c("",as.character( unique(demandda$Customer))),
                                                selected = "option2"),
           "Skill" = selectInput("skilla", "Select Skill",
                                 choices = c("",as.character(unique(colnames(dd)))),
                                 selected = "option2"
           )
           
    )
  })
  
  

  output$Box3 = renderUI({
    if ((input$radio=="Skill"))
      return(selectInput("custa", "Select Customer", choices= act_customer(input$skilla)))
    
    selectInput("skilla", 
                "Select Skill", 
                choices = c("", list_customer(input$custa))
    )})
  
  output$Box4 = renderUI({
    if ((input$radio=="Skill"))
      return(selectInput("bucks", "Select Skill Bucket", choices =c("", act_skill(input$skilla))))
    selectInput("bucks","Select Skill Bucket",choices = c( "",list_skillbucket(input$custa)))
  })
  
  output$Box5 = renderUI({
    
    if ((input$radio=="Skill"))
      return(selectInput("subarea", "Select Location", choices =c("", act_location(input$skilla))))
    selectInput("subarea","Select Location",choices = c("",list_location(input$custa)))
  })
  
  output$Box6 = renderUI(
    sliderInput(inputId = "num", label = "Choose a number", value = 20, min=1, max = 50)
  )
  
  
  output$Box7 = renderUI(
    actionButton(inputId = "go4", label = "Radar", color = "red")  )
  data <- eventReactive(input$go, {likeme(input$ski.ll[1], input$job[1], input$exp[1], input$stype[1], input$sk.ill[1], input$num1[1], input$clack[1],input$functional[1],
                                          input$systems[1])})
  
  output$score <- DT::renderDataTable({
    data()
  })
  
  data1 <- eventReactive(input$cust, {forecaster(input$forecast.ss[1],input$custloc[1])})
  data2 <- eventReactive(input$cust, {maps(input$forecast.ss[1],input$forecast.qq[1],input$forecast.yy[1],input$custloc[1])})
  data3 <- eventReactive(input$cust, {maptable(input$forecast.ss[1],input$forecast.qq[1],input$forecast.yy[1],input$custloc[1])})
  data4 <- eventReactive(input$go4, {newman(input$skilla[1], input$num, input$bucks, input$subarea, input$custa, input$radio)})
  data5 <- eventReactive(input$go5,{manji(input$skills1,input$Experience, input$Customer, input$Job_family,input$Designation,input$Skill_category, input$L2, input$L3, input$Band, input$Sub_band, input$Personal_subarea)})
  data6 <- eventReactive(input$go6,{jobboard(input$kill1,input$kill2, input$kill3)})
  data7 <- eventReactive(input$cust,{custskill1(input$forecast.ss, input$forecast.yy, input$forecast.qq)})
  data8 <- eventReactive(input$cust,{tabs(input$forecast.ss, input$forecast.yy, input$forecast.qq)})
  recodata <- eventReactive(input$recogo, {candidate_recommendation(input$recoskill)})
  data9 <- eventReactive(input$go4,{customer(input$skilla[1])})
  data10 <- eventReactive(input$cust,{fulfillment.customer(input$forecast.ss[1])})
  data11 <- eventReactive(input$cust,{fulfillment.location(input$forecast.ss[1])})
  data.popularity <- eventReactive(input$popularity,{popularity(input$poploc,input$dynamic,input$dyna)})
  data.combforecast <- eventReactive(input$cust,{combopred(input$forecast.ss[1],input$forecast.qq[1],input$forecast.yy[1],input$custloc[1])})
  data.custforecast <- eventReactive(input$cust,{cust.forecast(input$forecast.ss[1],input$forecast.qq[1],input$forecast.yy[1],input$custloc[1])})
  

  output$custforecast <- DT::renderDataTable({
    data.custforecast()
    
  })
  
  
  output$combforecast <- DT::renderDataTable({
    data.combforecast()
    
  })
  
  
  
  output$recoresults <- DT::renderDataTable({
    recodata()
  })
  output$varun <- renderUI({
    if (is.null(input$poploc))
      return()
    
    switch(input$poploc,
           
           "INDIA" = selectInput("dynamic", "Select the Customer",
                                 choices = unique(subset(demand.dump, demand.dump$country=="INDIA")$Customer),
                                 selected = "option2"
           ),
           "USA" = selectInput("dynamic", "Select the customer",
                               choices = unique(subset(demand.dump, demand.dump$country=="USA")$Customer),
                               selected = "option2"
           )
           
    )
  })
  
  output$forecast.skill <- renderUI({
    selectInput("forecast.ss", "Select Skill Bucket",
                choices = unique(subset(demand.dump, tolower(demand.dump$country)==tolower(input$custloc))$Skill.Bucket),
                selected = "option3"
    )
  })
  
  output$forecast.year <- renderUI({
    selectInput("forecast.yy", "Select Year",
                choices = unique(subset(demand.dump, tolower(demand.dump$country)==tolower(input$custloc) & tolower(demand.dump$Skill.Bucket)==tolower(input$forecast.ss))$year),
                selected = "option3"
    )
  })
  
  output$forecast.quarter <- renderUI({
    selectInput("forecast.qq", "Select Quarter",
                choices = unique(subset(demand.dump, tolower(demand.dump$country)==tolower(input$custloc) & tolower(demand.dump$Skill.Bucket)==tolower(input$forecast.ss) & demand.dump$year == input$forecast.yy)$quarter),
                selected = "option3"
    )
  })
  
  output$skill.varun <- renderUI({
    selectInput("dyna", "Select Skill Bucket",
                choices = unique(subset(subset(demand.dump, demand.dump$country==input$poploc),
                                        subset(demand.dump, demand.dump$country==input$poploc)$Customer==input$dynamic)$Skill.Bucket),
                selected = "option3"
    )
  })
  
  output$pop.plot <- renderPlotly({
    External1 <- data.frame(data.popularity()[1])
    External1$year.quarter <- factor(External1$year.quarter, levels = External1[["year.quarter"]])
    
    plot_ly(External1, x = ~year.quarter, y = ~External1[,3], name = colnames(External1)[3], type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(155, 9, 9)', width = 4)) %>%
      add_trace(y = ~External1[,4], name = colnames(External1)[4], line = list(color = 'rgb(5, 14, 109)', width = 4)) %>%
      add_trace(y = ~External1[,5], name = colnames(External1)[5], line = list(color = 'rgb(20, 109, 4)', width = 4)) %>%
      add_trace(y = ~External1[,6], name = colnames(External1)[6], line = list(color = 'rgb(244, 244, 97)', width = 4)) %>%
      add_trace(y = ~External1[,7], name = colnames(External1)[7], line = list(color = 'rgb(93, 7, 158)', width = 4)) %>%
      layout(title = "The Popularity of top 5 skills over time",
             xaxis = list(title = "Year - Quarter"),
             yaxis = list (title = "Popularity in Numbers"))
  })
  
  
  output$pop.table <- DT::renderDataTable({
    data.frame(data.popularity()[2])
    
  })
  
  output$top10losers <- DT::renderDataTable({
    data.frame(data.popularity()[3])
    
  })
  
  output$top10gainers <- DT::renderDataTable({
    data.frame(data.popularity()[4])
    
  })
  
  
  output$plot <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = data3()$Demand, text = data3()$State, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g) 
    
      
  })
  
  output$ful.loc <- renderPlotly({
    
    plot_ly(
      x = data11()$Group.1,
      y = data11()$x,
      name = "",
      type = "bar"
    )    
  })
  
  #Plot to display statistics about the customer. Currently displayed.
  output$ful.cust <- renderPlotly({
    
    plot_ly(
      x = data10()$Group.1,
      y = data10()$x,
      name = "",
      type = "bar"
    )    
  })
  
  #Displays a table with skills separated with commas.
  output$links<-  DT::renderDataTable({
    #data4()
    datatable((data.frame(Skill = colnames(data.frame(data4()[1], check.names =FALSE )),
                          Definition = unlist(lapply(colnames(data.frame(data4()[1], check.names = FALSE)), function (x) {defin(x)})),
                          Alternatives= unlist(lapply(colnames(data.frame(data4()[1], check.names = FALSE)), function (x) {alter(x)})))), options = list(columnDefs = list(list(
                            targets = 3,
                            render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data.length > 400 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 400) + '...</span>' : data;",
                              "}")
                          ))), callback = JS('table.page(3).draw(false);'))
    
  })
  
  #Displays a box with the Overall demand for the quarter and year selected.
  output$overall <- renderValueBox({
    valueBox(
      paste0(data8()$Overall), "Overall Demand", icon = icon("group"),
      color = "yellow"
    )
  })
  
  
  #Displays a box with the Fulfillment percentage for the quarter and year selected.
  output$fulfillment <- renderValueBox({
    valueBox(
      paste0(data8()$ful.per, "%"), "Fulfillment", icon = icon("thumbs-up"),
      color = "olive"
    )
  })
  
  #Displays a box with the Drop percentage for the quarter and year selected.
  output$drop <- renderValueBox({
    valueBox(
      paste0(data8()$drop.per, "%"), "Drop", icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  output$od <- renderValueBox({
    valueBox(
      paste0(data8()$od.per, "%"), "Unfulfilled Overdue", icon = icon("list"),
      color = "orange"
    )
  })
  
  #Displays a box with the Forecast for the next quarter.
  output$frcst <- renderValueBox({
    valueBox(
      data1()[3,]$Demand, paste0(data1()[3,]$Quarter," ","Forecast"), icon = icon("line-chart"),
      color = "blue"
    )
  })
  
  output$revenue <- renderValueBox({
    valueBox(
      paste0("$",data1()[3,]$Demand*65*2080), paste0(data1()$quarter[nrow(data1())],"-",data1()$year[nrow(data1())],"Revenue"), icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$custplot <- renderPlotly(
    {
      plot_ly(data7(), x = ~Customer, y = ~Demand,  type = 'scatter',color = ~Segement,
              size = ~Demand, 
              mode = 'markers',colors = colors,
              
              marker = list(symbol = "circle", sizemode = 'diameter',
                            line = list(width = 3, color = '#FFFFFF'))) %>%
        layout(title = paste(""),
               xaxis = list(title = '',
                            gridcolor = 'rgb(255, 255, 255)',
                            
                            
                            
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2,
                            showticklabels = FALSE),
               yaxis = list(title = '',
                            gridcolor = 'rgb(255, 255, 255)',
                            
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)')
      
    }
  )
  
  output$coolplot <- renderPlot({
    ggplot(data1(), aes(x = paste(year,"-",quarter), y = Demand.Forecast, group = 1))+
      geom_line(aes(color = "green"))+
      geom_line(aes(y = Actual.Demand,color = "red"))+
      theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))+
      scale_size_manual(values = c(0.1, 1))+
      xlab("Year - Quarter") + ylab("Demand in Numbers") +  scale_fill_discrete(name="Type of Demand",
                                                                                breaks=c("Forecast", "Actual"),labels=c("Forecast", "Actual"))+ggtitle(paste("Forecast for",input$skill[1]))
    
  })
  
  output$results <- DT::renderDataTable({
    data1()
    
  })
  
  output$map <- renderPlot({
    spplot(data2()['value'], title = paste("Demand throughout the US for",input$skill[1], "in Quarter",input$quarter[1],"of", input$year[1]))
   
  })
  
  output$maptable <- DT::renderDataTable({
    data2()
  })
  
  output$skills <- renderPlot({
    radarchart(data.frame(data4()[1], check.names = FALSE),pcol = "red")
  })
  ##############################################indicator########################  
  output$skills2 <-    renderPlotly({
    if ((input$skilla==""))
      return()
    plot_ly(data=data9(),x = as.factor(data9()$custo),y = data9()$total,   type = "bar")%>%layout(xaxis = list(categoryorder = "array",
                                                                                                               categoryarray = (data9()$custo)))
    
  })
  
  output$skills3 <- renderDataTable({
    datatable( data.frame(Boolean=paste(colnames(data.frame(data4()[1], check.names = FALSE)),collapse = ",")))
  })
  #newmanvalue box like e radar
  output$frequency <- renderValueBox({
    valueBox(
      paste0(unlist(data4()[3])), "Job Description(s) used to generate the Skill Radar", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$results1 <- DT::renderDataTable({
    data5()
  })
  
  output$results2 <- DT::renderDataTable({
    data6()
  })
  
}

shinyApp(ui = ui, server = server)
