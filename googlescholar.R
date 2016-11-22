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






