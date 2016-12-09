library(XML)


#read lines error preventing
catch_txt = function(URL) {
  strURLCont = tryCatch(readLines(URL, warn = FALSE), error = function(e){})
  strURLCont 
}

##parse error preventing
catch_parse = function(URL) {
  strURLCont = tryCatch(htmlParse(URL), error = function(e){})
  strURLCont 
}


#function for waiting 
getpage  = function(URL) {
  time = 0
  status = catch_txt(URL)
  count = as.numeric(length(status))
  while((class(status) == "NULL") && (time < 6)){
    Sys.sleep(5)
    status = catch_txt(URL)
    time = time + 1
  }
  catch_txt(URL)
}

##function for waiting until it works
getParse  = function(URL) {
  time = 0
  status = catch_parse(URL)
  count = as.numeric(length(status))
  while((class(status) == "NULL") && (time < 2)){
    Sys.sleep(20)
    status = catch_parse(URL)
    time = time + 1
  }
  catch_parse(URL)
}

getinfo = function(getdoc){
  # define an end to the page range for the search
  getitle <- unlist(lapply(getdoc['//h3[@class ="gs_rt" ]'],xmlValue))
  getnote <- unlist(lapply(getdoc['//div[@class ="gs_a" ]'],xmlValue))
  notes = sapply(getnote,function(x) strsplit(x,'-'))
  n = length(getitle)
  df <- data.frame(Title = character(n), Author = character(n),locORyear =character(n), Notes = character(n),stringsAsFactors = FALSE)
  for (j in 1:n){
    df$Title[j] =getitle[j]
    df$Author[j] = notes[[j]][1]
    df$locORyear[j] = notes[[j]][2]
    df$Notes[j] = notes[[j]][3]
  }
  df
}


var_searchURL_character = 'http://scholar.google.com/scholar?cites=15888883762379490553&as_sdt=2005&sciodt=0,5&hl=en'

page = 1
getdoc <- getParse(var_searchURL_character)

while(length(getdoc['//td[@align ="left" ]/a[@href]']) != 0){
  grab = floor(runif(1,min = 4,max = 6))
  print(grab)
  for(i in 1:grab){
    print(page)
    write.table(getinfo(getdoc),file = "googlescholar",append = TRUE,row.names = FALSE, col.names = FALSE)
    if(length(getdoc['//td[@align ="left" ]/a[@href]']) == 0){
      break
    }
    var_searchURL_character = paste0("http://scholar.google.com",xmlAttrs(getdoc['//td[@align ="left" ]/a[@href]'][[1]]))
    i = i + 1
    Sys.sleep(floor(runif(1,min = 300,max = 600)))
    page = page + 1
    getdoc <- getParse(var_searchURL_character)
  }
  Sys.sleep(floor(runif(1,min = 3600,max = 5400 )))
  
  
  if(as.numeric(strsplit(strsplit(as.character(Sys.time())," ")[[1]][2],":")[[1]][1]) > 23){
    Sys.sleep(21600)
  }
  
  
}

#==============================================download the source code for each page and then scrape those downloaded html files. This is to avoid IP address being blocked================================================
                 
library(RCurl)

getinfo = function(getdoc){
  # define an end to the page range for the search
  getitle <- unlist(lapply(getdoc['//h3[@class ="gs_rt" ]'],xmlValue))
  getnote <- unlist(lapply(getdoc['//div[@class ="gs_a" ]'],xmlValue))
  notes = sapply(getnote,function(x) strsplit(x,'-'))
  n = length(getitle)
  df <- data.frame(Title = character(n), Author = character(n),locORyear =character(n), Notes = character(n),stringsAsFactors = FALSE)
  for (j in 1:n){
    df$Title[j] =getitle[j]
    df$Author[j] = notes[[j]][1]
    df$locORyear[j] = notes[[j]][2]
    df$Notes[j] = notes[[j]][3]
  }
  df
}



for(k in 1:length(list.files())){
  getdoc = readHTMLTable(list.files()[k],stringsAsFactors = FALSE)
  getdf = getdoc[[1]]
  
  v = c("")
  
  for(i in 1:nrow(getdf)){
    for(j in 1:ncol(getdf)){
      v = c(v,getdf[i,j])
    }
  }
  
  html_doc = paste(v, collapse = "")
  newdoc = htmlParse(html_doc)
  write.table(getinfo(newdoc),file = "googlescholar.txt",append = TRUE,row.names = FALSE, col.names = FALSE)
  print(k)
}



googlescholar = read.table("googlescholar.txt")
names(googlescholar)= c('Title','Author','locORyear','Notes')
write.csv(googlescholar,'googlescholar.csv',row.names = FALSE)

                 
                 
                 
 #=================================Another way to scrape more efficiently ======Use Relenium=================================================
library('RSelenium')


var_searchURL_character ="https://scholar.google.com/scholar?start=0&hl=en&as_sdt=2005&sciodt=0,5&cites=26185891145257329&scipsc="

checkForServer()
startServer()
mybrowser = remoteDriver(remoteServerAddr = "localhost" 
                         , port = 4444
                         , browserName = "firefox"
)
mybrowser$open()
mybrowser$navigate(var_searchURL_character)



for (var_page_int in 1:100) {
  
  
  titlet = character(0)
  authort = character(0)
  notet = character(0)
  locORyeart= character(0)
  
  gettitle = mybrowser$findElements(using = 'xpath','//h3[@class ="gs_rt" ]')
  getarticle = mybrowser$findElements(using = 'xpath', '//div[@class ="gs_a" ]')
  for(i in 1:length(gettitle)){
    print(i)
    title = unlist(gettitle[[i]]$getElementText())
    article = unlist(getarticle[[i]]$getElementText())
    article = unlist(strsplit(article,'-'))
    
    author = article[1]
    locORyear = article[2]
    note = article[3]
    
    ####save the text

    titlet = c(titlet, title)
    authort = c(authort,author)
    locORyeart = c(locORyeart, locORyear)
    notet = c(notet,note)
  }
  
  gs = data.frame(titlet, authort, locORyeart,notet)
  write.table(gs,'gstest',row.names = FALSE,append = TRUE,col.names = FALSE)
  
    Sys.sleep(2)
    pagebox <- mybrowser$findElement(using = 'xpath', '//td[@align ="left" ]/a[@href]')
    pagebox$clickElement()
  
    Sys.sleep(floor(runif(1,min = 30,max = 40 )))
  
  
  print(var_page_int)
  
}



#===================================Use Relenium to query publician year(1970-2016)=================================================
library('RSelenium')

var_searchURL_character ="https://scholar.google.com/scholar?as_q=&as_epq=silent+spring&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=&as_ylo=1970&as_yhi=1970&btnG=&hl=en&as_sdt=0%2C5"


checkForServer()
startServer()
mybrowser = remoteDriver(remoteServerAddr = "localhost" 
                         , port = 4444
                         , browserName = "firefox"
)
mybrowser$open()

for (yr in 1970:2016){
  var_searchURL_character = paste0("https://scholar.google.com/scholar?as_q=&as_epq=silent+spring&as_oq=&as_eq=&as_occt=any&as_sauthors=&as_publication=&as_ylo=",yr,"&as_yhi=",yr,"&btnG=&hl=en&as_sdt=0%2C5")
  
  mybrowser$navigate(var_searchURL_character)
  Sys.sleep(8)
  
  #get page
  ifnext = mybrowser$findElements(using = 'xpath','//td[@align ="left" ]/a[@href]')
  
  var_page_int = 1
  while(length(ifnext) != 0) {
    
    
    titlet = character(0)
    authort = character(0)
    notet = character(0)
    locORyeart= character(0)
    
    gettitle = mybrowser$findElements(using = 'xpath','//h3[@class ="gs_rt" ]')
    getarticle = mybrowser$findElements(using = 'xpath', '//div[@class ="gs_a" ]')
    for(i in 1:length(gettitle)){
      print(i)
      title = unlist(gettitle[[i]]$getElementText())
      article = unlist(getarticle[[i]]$getElementText())
      article = unlist(strsplit(article,'-'))
      
      author = article[1]
      locORyear = article[2]
      note = article[3]
      
      ####save the text
      
      titlet = c(titlet, title)
      authort = c(authort,author)
      locORyeart = c(locORyeart, locORyear)
      notet = c(notet,note)
    }
    
    gs = data.frame(titlet, authort, locORyeart,notet,rep(yr,length(titlet)))
    write.table(gs,'queryyear',row.names = FALSE,append = TRUE,col.names = FALSE)
    
    Sys.sleep(2)
    ifnext = mybrowser$findElements(using = 'xpath','//td[@align ="left" ]/a[@href]')
    if(length(ifnext) == 0){
      break
    }
    
    
    pagebox <- mybrowser$findElement(using = 'xpath', '//td[@align ="left" ]/a[@href]')
    pagebox$clickElement()
    
    Sys.sleep(floor(runif(1,min = 15,max = 30 )))
    
    
    print(var_page_int)
    var_page_int = var_page_int + 1
    
    
  }
  
  print(yr)
}





queryyear <- read.csv2("C:/Users/bian0553/Desktop/newspaper/googlescholar/queryyear", header=FALSE, sep="")

dim(queryyear)

names(queryyear) = c("Title","Author","Locoryear","Note","QueryYear")

write.csv(queryyear,"googlescholar1970-2016.csv",row.names = FALSE)



