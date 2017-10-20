library(pdftools)
library(quanteda)
library(stringdist)
library(stringr)
library(data.table)
library(dplyr)
library(zoo)
library(lubridate)
require(tm)
setwd("C:\\HCL\\LikeMe\\Attachments")
folder<-file.path("C:\\HCL\\LikeMe\\Attachments")
len<-length(dir(folder)); len
dirpdf<-dir(folder)
cat(dirpdf[1])
resumes<-list()
#resumes$profile<-"0"
mobile <- list()
email <- list()
recruiter <- list()
date <- list()
skills1 <- list() 
filename <- list()
workex<-list()
Areas_of_exp<-list()
education<-list()
noofyears <- list()
front_end_skills<-list()
Backend_skills<-list()
full_name <- list()

setwd("C:\\HCL\\LikeMe")
headings<-read.csv("headings.csv")
year <- read.csv("timeframe.csv")
skills <- read.csv("skillsets23.csv", stringsAsFactors = FALSE)
tokens <- tokens(skills$A..1, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
tokens <- tokens_tolower(tokens)
tokens <- tokens_select(tokens, stopwords(), selection = "remove")
tokens <- tokens_ngrams(tokens, n = 1:5)
setwd("C:\\HCL\\LikeMe\\Attachments")
pdf_file <- pdf_text(dirpdf[1])

for (i in 1:len)
{
  pdf_file1 <-as.list( pdf_text(dirpdf[i]))
  pdfinfo <- pdf_info(dirpdf[2])
  pdf_file1<-paste(pdf_file1, collapse = " ")
  pdf_file<-pdf_file1
  pdf_file <- tolower(pdf_file)
  
  pdf_file<-gsub("\\\n"," ",pdf_file)
  pdf_file<-gsub("\\\r"," ",pdf_file)
  #mobile[i] <- unlist(str_extract_all(pdf_file, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"))
  if(length(unlist(str_extract_all(pdf_file, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}")))==0){
    mobile[i] <- ""
  }else{
    mobile[i] <-unlist(str_extract_all(pdf_file, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"))
  }
  if(length(unlist(regmatches(pdf_file, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", pdf_file))))==0){
    email[i] <- ""
  }else{
    email[i] <- unlist(regmatches(pdf_file, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", pdf_file)))
  }
 
  full_name[i] <- gsub("\\s+"," ",(str_extract(pdf_file, "Name.+|: [a-zA-Z]*|^[a-zA-Z ]*")[1]))
  stopwords1 = c("resume", "address", "name", "mobile", "application", "cv", "curriculum","vitae", "no", "email")
  full_name[i] <- removeWords(unlist(full_name[i]), stopwords1)
  
  resumes[i]<-pdf_file
  date[i]<-as.Date(parse_date_time(pdfinfo$created,orders = "ymd,hms", tz = "PST"), "%m/%d/%Y")
  tokens1 <- tokens(pdf_file, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  tokens1 <- tokens_tolower(tokens1)
  tokens1 <- tokens_select(tokens1, stopwords(), selection = "remove")
  tokens1 <- tokens_ngrams(tokens1, n = 1:5)
  tokens1 <- tokens_select(tokens1, tokens, selection = "keep")
  skills1[i] <- paste(unique(unlist(tokens1)),collapse = ",")
  filename[i] <- dirpdf[i]
  fes <- unique( unlist( str_extract_all(resumes[i],"ajax|ajax-truclient|angular|angular 2|angular cli|aurelia|backbone (network)|backbone.js|bootstrap|bower|browserify|canvas|chart fx|citrix|clojurescript|compass|css|css-related|css modules|css preprocessor|d3.js|directshow|dojo|dom|ecmascript|electron|elm|ember.js|flux|haml|html|html-related|htmlpad|hyper-v|graphql|grunt|gulp|ioc|infragistics|itil|isa|isomorphic apps|jade|jasmine|javascript testing tools|javascript frameworks|jface|json|jsx|jtable|jquery|jwt|karma|kendo|less|masonry|mercury|microsoft visio|mocha|polymer|postcss|reactjs|relay|rxjs (the reactive extensions for javascript)|saml|sass|shadow dom|socket.io|sso|stylus|surefire|turret|typescript|underscore.js|vmware|vsphere|front end technologies|vue|watchkit|webgl|webkit|webpack|webrx|websphere|webstorm|winforms|wsdl|yaml|xhtml|zurb" )))
  bes <- unique( unlist( str_extract_all(resumes[i],"java script|node.js| sails| express.|broccoli|casperjs|chai|core.js|dust.js|ext js|express.js|gsap|handlebars.js|hapi|lodash|node.js-based build tools|npm|knockout|koa|mean|meteor|pixijs|phantomjs|redux|requirejs|prototype|sails|sinon|spine js|vanilla js|vuex|zombie|php|php keywords|php-fpm|php frameworks|bitrix framework|back-end|cakephp|codeception|composer|cms|codeigniter|doctrine|drupal|joomla|hhvm|gearman|lamp|laravel|magento|memcached|mockery|modx|kohana|phalcon|phpspec|phpstorm|silex|smarty|symfony|twig|zend framework|wordpress|yii|ruby|ruby frameworks|rails/ruby on rails| sinatra| padrino| jruby.|ruby testing tools|rspec| capybara| watir| cucumber.|ruby deployment|passenger| capistrano.|active record|capistrano|capybara|crystal|elixir|eventmachine|grape|jruby|padrino|phoenix|rake|ruby arrays|rubygems|ruby on rails|rvm|sidekiq|sinatra|watir|java|java ee frameworks|jsp| servlets| ejb| jmx| jaf.|java se frameworks|java orm frameworks|hibernate| jpa.|java testing tools|java build tools|ant| maven| gradle.|java deployment|tomcat| jboss| glassfish| weblogic.|adf|apache ant|apache camel|glassfish|dwr|eclipselink|ejb|freemarker|hybris|jaf|jasperreports|jackson|java ee|java me|javafx|java se|jax-rs|jboss|jbpm|jersey|jdbc|jdeveloper|jest|jetty|jfr|jmc|jms|jmx|jni|jpa|jpql|jsf|junit|jvm| |gradle|groovy|gwt|kotlin|lambda|lucene|maven|meshcms|osgi|powermock|resteasy|roboguice|servlets|spring|spring integration|spring mvc|spring boot|struts|struts 2|swing|tomee|weblogic|wildfly|vaadin|zxing|c#|c# (.net)|.net|accord.net framework|asp.net|asp.net core|ado.net|asp.net mvc|autofac|c# testing tools|c# frameworks|.net 1.0 - .net 4.5.*|entity framework|octopus|orm frameworks|owin|f# 4.0|mono|.net core|nancy|nuget|nunit|kentico|linq|msbuild|mstest|nhibernate|razor|roslyn (.net compiler platform)|signalr|sitecore cms|specflow|ssrs|vb.net|wcf|wpf|xamarin|xamarin.forms|xamarin studio|python|python frameworks|beeware|cherrypy|django|docutils|jupyter (ipython)|flask|kivy|matplotlib|numpy|pandas|pybrain|pygtk|pylons|pyston|pyramid|sqlalchemy|tensorflow|theano|tornado|virtualenv|web2py|web.py|wxpython|c++|c++ frameworks|ace|boost|clang|cppcheck|glsl|gperf|loki|mfc|poco|pwc (contract programming)|qt|qt creator|rust|stl|valgrind|vba|visual studio|wxwidgets|zeromq|c language|c|kore|scala frameworks|scala|akka|akka http|apache kafka|cats|colossus|finatra|finch|http4s|lift 3.0-rc1|play|play 2.5.0|sbt|scalaz|sbt 0.13.12|shapeless|slick|spray|other programming languages|actionscript|ada|assembler|assembly language|ccl|clojure|coffeescript|coldfusion mx|dart|delphi|hack|haskell|go|lua|maple|matlab|perl|r language|rpg|thrift|vbscript|xtend")))
  if(length(fes)!=0){
    front_end_skills[i]<-paste(unique( unlist( str_extract_all(resumes[i],"ajax|ajax-truclient|angular|angular 2|angular cli|aurelia|backbone (network)|backbone.js|bootstrap|bower|browserify|canvas|chart fx|citrix|clojurescript|compass|css|css-related|css modules|css preprocessor|d3.js|directshow|dojo|dom|ecmascript|electron|elm|ember.js|flux|haml|html|html-related|htmlpad|hyper-v|graphql|grunt|gulp|ioc|infragistics|itil|isa|isomorphic apps|jade|jasmine|javascript testing tools|javascript frameworks|jface|json|jsx|jtable|jquery|jwt|karma|kendo|less|masonry|mercury|microsoft visio|mocha|polymer|postcss|reactjs|relay|rxjs (the reactive extensions for javascript)|saml|sass|shadow dom|socket.io|sso|stylus|surefire|turret|typescript|underscore.js|vmware|vsphere|front end technologies|vue|watchkit|webgl|webkit|webpack|webrx|websphere|webstorm|winforms|wsdl|yaml|xhtml|zurb" ))),collapse = ",")
  }else{
    front_end_skills[i]=""  
  }
  if(length(bes)!=0){
  Backend_skills[i]<-paste(unique( unlist( str_extract_all(resumes[i],"java script|node.js|sails|express.|broccoli|casperjs|chai|core.js|dust.js|ext js|express.js|gsap|handlebars.js|hapi|lodash|node.js-based build tools|npm|knockout|koa|mean|meteor|pixijs|phantomjs|redux|requirejs|prototype|sails|sinon|spine js|vanilla js|vuex|zombie|php|php keywords|php-fpm|php frameworks|bitrix framework|back-end|cakephp|codeception|composer|cms|codeigniter|doctrine|drupal|joomla|hhvm|gearman|lamp|laravel|magento|memcached|mockery|modx|kohana|phalcon|phpspec|phpstorm|silex|smarty|symfony|twig|zend framework|wordpress|yii|ruby|ruby frameworks|rails/ruby on rails| sinatra| padrino| jruby.|ruby testing tools|rspec| capybara| watir| cucumber.|ruby deployment|passenger| capistrano.|active record|capistrano|capybara|crystal|elixir|eventmachine|grape|jruby|padrino|phoenix|rake|ruby arrays|rubygems|ruby on rails|rvm|sidekiq|sinatra|watir|java|java ee frameworks|jsp| servlets| ejb| jmx| jaf.|java se frameworks|java orm frameworks|hibernate| jpa.|java testing tools|java build tools|ant| maven| gradle.|java deployment|tomcat| jboss| glassfish| weblogic.|adf|apache ant|apache camel|glassfish|dwr|eclipselink|ejb|freemarker|hybris|jaf|jasperreports|jackson|java ee|java me|javafx|java se|jax-rs|jboss|jbpm|jersey|jdbc|jdeveloper|jest|jetty|jfr|jmc|jms|jmx|jni|jpa|jpql|jsf|junit|jvm| |gradle|groovy|gwt|kotlin|lambda|lucene|maven|meshcms|osgi|powermock|resteasy|roboguice|servlets|spring|spring integration|spring mvc|spring boot|struts|struts 2|swing|tomee|weblogic|wildfly|vaadin|zxing|c#|c# (.net)|.net|accord.net framework|asp.net|asp.net core|ado.net|asp.net mvc|autofac|c# testing tools|c# frameworks|.net 1.0 - .net 4.5.*|entity framework|octopus|orm frameworks|owin|f# 4.0|mono|.net core|nancy|nuget|nunit|kentico|linq|msbuild|mstest|nhibernate|razor|roslyn (.net compiler platform)|signalr|sitecore cms|specflow|ssrs|vb.net|wcf|wpf|xamarin|xamarin.forms|xamarin studio|python|python frameworks|beeware|cherrypy|django|docutils|jupyter (ipython)|flask|kivy|matplotlib|numpy|pandas|pybrain|pygtk|pylons|pyston|pyramid|sqlalchemy|tensorflow|theano|tornado|virtualenv|web2py|web.py|wxpython|c++|c++ frameworks|ace|boost|clang|cppcheck|glsl|gperf|loki|mfc|poco|pwc (contract programming)|qt|qt creator|rust|stl|valgrind|vba|visual studio|wxwidgets|zeromq|c language|c|kore|scala frameworks|scala|akka|akka http|apache kafka|cats|colossus|finatra|finch|http4s|lift 3.0-rc1|play|play 2.5.0|sbt|scalaz|sbt 0.13.12|shapeless|slick|spray|other programming languages|actionscript|ada|assembler|assembly language|ccl|clojure|coffeescript|coldfusion mx|dart|delphi|hack|haskell|go|lua|maple|matlab|perl|r language|rpg|thrift|vbscript|xtend"))),collapse = ",")
  }else{
    Backend_skills[i]<-""
  }
  #work experience#################################################################################
  dc<-corpus(pdf_file1)
  d<-corpus_segment(dc, what = "other", delimiter = "\n")
  bdd<-  d$documents$texts
  bdd_dat<-data.frame(d$documents$texts)
  names(bdd_dat)<- "Text"
  bdd_dat$Text<-as.character(bdd_dat$Text)
  bdd_dat$Text<-trimws(bdd_dat$Text, "both")
  bdd_dat$noofchar<-nchar(as.character(bdd_dat$Text))  
  bdd_dat<-filter(bdd_dat, noofchar<=28)
  bdd_dat$word<- strsplit(bdd_dat$Text, '')
  bdd_dat$firstletter<-lapply(bdd_dat$word, function(x) x[1])
  bdd_dat$Trueorfalse<-grepl("^[[:upper:]]+$",  bdd_dat$firstletter)
  bdd_dat<- filter(bdd_dat, Trueorfalse==TRUE)
  bdd_dat$Text1<-str_replace_all(bdd_dat$Text,"[[:punct:]]"," ")
  
  m <- stringdistmatrix(tolower(bdd_dat$Text1), tolower(headings$Name), method = "lv")
  
  a <- data.frame(Text= bdd_dat$Text, Identified_as= headings$Header[apply(m, 1, which.min)],dista= apply(m, 1, min))
  
  a<- filter(a, dista<=5)
  
  bdd_<-data.frame(d$documents$texts)
  names(bdd_)<- "Text"
  #bdd_$identified<-0
  #bdd_$Text%in%a$Resume_text
  bdd_<-left_join( bdd_ ,a[c(1,2)], by="Text")
  
  
  dt<-data.table(bdd_)
  
  dt[, y_forward_fill := Identified_as[1], .(cumsum(!is.na(Identified_as)))]
  
  workex[i]<-paste(dt$Text[dt$y_forward_fill=="Work Experience" & !is.na(dt$y_forward_fill)],collapse = " " )
  education[i]<-paste(dt$Text[dt$y_forward_fill=="Education" & !is.na(dt$y_forward_fill)],collapse = " " )
  Areas_of_exp[i]<-paste(dt$Text[dt$y_forward_fill=="Credentials" & !is.na(dt$y_forward_fill)],collapse = " " )
  
  Format_mm <-unlist(str_extract_all(unlist(workex[i]), "\\(?\\d{2}/\\)? *\\d{2} "))
  
  Format_mm <-unlist(str_extract_all(Format_mm, "\\(?\\/\\)? *\\d{2} "))
  
  Format_mm<-trimws(Format_mm, which = "both")
  
  Format_YYYY <-unlist(str_extract_all(unlist(workex[i]), "\\(?\\d{2}\\)? *\\d{2} "))
  
  list_year<- year$act[is.element(year$Year,Format_mm) ]
  
  Format_YYYY<-trimws(Format_YYYY, which = "both")
  
  list_year1<- year$act[is.element(year$Year,Format_YYYY) ]
  
  #list_year<-append(list_year,list_year1)
  
  FOrmat_MMYY<- unlist(str_extract_all(unlist(workex[i]),"(January|Jan|JAN|February|Feb|FEB|March|Mar|MAR|April|Apr|APR|May|May|MAY|June|Jun|JUN|July|Jul|JUL|August|Aug|AUG|September|Sep|SEP|October|Oct|OCT|November|Nov|NOV|December|Dec|DEC) \\d{2}"))
  
  FOrmat_MMYY<-unlist(str_extract_all(FOrmat_MMYY, "\\d{2}"))
  
  FOrmat_MMYY<-trimws(FOrmat_MMYY, which = "both")
  
  list_year2<- year$act[is.element(year$Year,FOrmat_MMYY) ]
  
  #list_year<-append(list_year,list_year2)
  
  
  FOrmat_MMYY2<- unlist(str_extract_all(unlist(workex[i]),"(January|Jan|JAN|February|Feb|FEB|March|Mar|MAR|April|Apr|APR|May|May|MAY|June|Jun|JUN|July|Jul|JUL|August|Aug|AUG|September|Sep|SEP|October|Oct|OCT|November|Nov|NOV|December|Dec|DEC)-\\d{2}"))
  
  FOrmat_MMYY2<-unlist(str_extract_all(FOrmat_MMYY2, "\\d{2}"))
  
  FOrmat_MMYY2<-trimws(FOrmat_MMYY2, which = "both")
  
  list_year3<- year$act[is.element(year$Year,FOrmat_MMYY2) ]
  
  #list_year<-append(list_year,list_year3)
  
  
  yearlist<- union(list_year3, union(list_year2, union(list_year,list_year1)))
  
  print(yearlist)
  noofyears[i]<- 2017 - min(yearlist)
  
}


#library(lubridate)
#parse_date_time(pdfinfo$created,orders = "ymd,hms", tz = "PST")

profiles <- data.frame(Full_Name = unlist(full_name), File_Name = unlist(filename),Mobile.Number = unlist(mobile), Email = unlist(email), Profile = unlist(workex), 
                       Education = unlist(education), Skills = unlist(Areas_of_exp),
                       TProfile = unlist(resumes),Years.Exp = unlist(noofyears), Front.End.Skills = unlist(front_end_skills),
                       Back.End.Skills = unlist(Backend_skills))
profiles$Years.Exp[profiles$Years.Exp <= 3] <- "1-3 Years"
profiles$Years.Exp[profiles$Years.Exp > 3 & profiles$Years.Exp <6] <- "3-5 Years"
profiles$Years.Exp[profiles$Years.Exp > 5 & profiles$Years.Exp <8] <- "5-7 Years"
profiles$Years.Exp[profiles$Years.Exp > 7 & profiles$Years.Exp <10] <- "7-9 Years"
profiles$Years.Exp[profiles$Years.Exp > 9 & profiles$Years.Exp <12] <- "9-12 Years"
profiles$Years.Exp[profiles$Years.Exp > 11] <- ">11 Years"
write.csv(profiles,"external.csv")
