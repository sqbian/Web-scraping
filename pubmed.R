#====================================Pubmed=============================================================
library('RSelenium')
library(RCurl)
library(httr)
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
  while(class(status) == "NULL"){
    Sys.sleep(5)
    status = catch_parse(URL)
  }
  catch_parse(URL)
}




pubmedscrape = function(object){
  var_searchURL_character <- "http://www.ncbi.nlm.nih.gov/pubmed/?term="
  if(length(object)>1){
    object = paste(object,collapse = "+")
  }
  var_searchURL_character <- paste(var_searchURL_character, object, sep="")
  getdoc <- getParse(rawToChar(GET(var_searchURL_character)$content))
  # define an end to the page range for the search
  getpageno <- xmlAttrs(getdoc['//input[@id="pageno"]'][[1]])['last']
  var_searchEndRange_int <- as.numeric(unname(getpageno))

  checkForServer()
  startServer()
  mybrowser = remoteDriver(remoteServerAddr = "localhost" 
                           , port = 4444
                           , browserName = "firefox"
  )
  mybrowser$open()
  mybrowser$navigate(var_searchURL_character)



  for (var_page_int in 26:var_searchEndRange_int) {

    linkt = character(0)
    titlet = character(0)
    authort = character(0)
    abstractt = character(0)


    if(var_page_int > 1){
      Sys.sleep(2)
      pagebox <- mybrowser$findElement(using = 'xpath', "//h3[@class = 'page']/input[@value]")
      pagebox$clearElement()
      pagebox$sendKeysToElement(list(as.character(var_page_int),key ='enter'))
    }

    article = mybrowser$findElements(using = 'xpath','//p[@class ="title"]/a[@href]')
    for(i in 1:length(article)){
      print(i)
      Sys.sleep(8)
      article = mybrowser$findElements(using = 'xpath','//p[@class ="title"]/a[@href]')
      Sys.sleep(3)
      article[[i]]$clickElement()
      Sys.sleep(5)
      ####save the text

      link = unlist(mybrowser$getCurrentUrl())
      link = paste0('http://',strsplit(link,'://')[[1]][2])
      getabtr <- getParse(rawToChar(GET(link)$content))
      if(length(getabtr['//div[@class ="abstr"]']) == 0){
       abstract = 'NULL' 
      }else{
        abstract = xmlValue(getabtr['//div[@class ="abstr"]'][[1]])
      }
      author = paste(unlist(xpathApply(getabtr,'//div[@class ="auths"]/a',xmlValue)),collapse = ", ")
      title = xmlValue(getabtr['//h1'][[2]])

      linkt = c(linkt,link)
      titlet = c(titlet, title)
      authort = c(authort,author)
      abstractt = c(abstractt, abstract)

      mybrowser$goBack()
    }

    pubmed = data.frame(linkt, titlet, authort, abstractt)
    write.table(pubmed,'pubmed',row.names = FALSE,append = TRUE,col.names = FALSE)
    print(var_page_int)

  }
}




object <- c("oocyte", "cryopreservation")
pubmedscrape(object)



