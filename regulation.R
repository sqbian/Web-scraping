library(XML)
library(RCurl)
library(httr)

var_searchURL_character = "https://www.regulations.gov/docketBrowser?rpp=50&so=DESC&sb=commentDueDate&po=5150&dct=PS&D=FDA-2014-N-1207"



checkForServer()
startServer()
mybrowser = remoteDriver(remoteServerAddr = "localhost" 
                         , port = 4444
                         , browserName = "firefox"
)
mybrowser$open()
mybrowser$navigate(var_searchURL_character)

getlast = mybrowser$findElements(using = 'xpath','//div[@class="GIY1LSJILB GIY1LSJIK GIY1LSJCL"]')
  

for  (var_page_int in 1:147){
  
  if(var_page_int > 1){
    pagebox <- mybrowser$findElement(using = 'xpath', "//div[@class='GIY1LSJHQ']/input[@type='text']")
    pagebox$clearElement()
    pagebox$sendKeysToElement(list(as.character(var_page_int),key ='enter'))
    Sys.sleep(3)
  }
  
  comments = mybrowser$findElements(using = 'xpath','//div[@class="GIY1LSJFYC GIY1LSJMXC"]/div[@id="left"]/div[@class="gwt-Hyperlink"]/a[@href]')
  
  URL = character(0)
  Comment = character(0)
  Name_of_PDF = character(0)

  for(i in 1:length(comments)){
    Sys.sleep(3)
    comments[[i]]$clickElement()
    Sys.sleep(5)
    #scrape comments
    url = unlist(mybrowser$getCurrentUrl())
    gettext = mybrowser$findElements(using = 'xpath','//div[@class="GIY1LSJIXD"]/div[@class]')
    text = unlist(gettext[[2]]$getElementText())
    URL = c(URL, url)
    Comment = c(Comment,text)
    getpdf = mybrowser$findElements(using = 'xpath','//div[@class="GIY1LSJE1D"]/h3[@class="h2 GIY1LSJL1D"]')
    #if there is pdf, save pdf
    if(length(getpdf)>0){
      download = mybrowser$findElements(using = 'xpath','//div[@class="GIY1LSJE1D"]/div[@class="GIY1LSJA1D floatLeft"]/a')
      if(length(download) == 0){
        Name_of_PDF = c(Name_of_PDF,paste(unlist(getpdf[[1]]$getElementText()),"No attachment",sep = "_"))
        
      }else{
      Name_of_PDF = c(Name_of_PDF,unlist(getpdf[[1]]$getElementText()))
        for(j in 1:length(download)){
          download[[j]]$clickElement()
          Sys.sleep(5)
        }
      }
    }else{
      Name_of_PDF = c(Name_of_PDF,' ')
    }
    mybrowser$goBack()
    Sys.sleep(3)
    print(paste0(i,'th results in this page scraped'))
  
  }
  
  write.table(data.frame(URL,Comment,Name_of_PDF),'regulation_result2',row.names = FALSE,append = TRUE,col.names = FALSE)  

  print(var_page_int)
 
}


finalform = read.table('regulation_result')
dim(finalform)  
names(finalform) = c('URL','Comment','Name_of_PDF')
write.csv(finalform,'regulation.csv',row.names = FALSE)



===================================================================entity extraction==============================================
library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(XML)
library(methods)


regulation = read.csv('regulation.csv')

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()


person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")

organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

formdf = function(rownum, kind,a_doc,regulation){
  entity = entities(a_doc,kind = kind)
  if(length(entity) == 0){
    entity = NA
  }
  if(length(entity) > 1){
    entity = paste(unique(entity),collapse = "; ")
  }
  regulation[kind][rownum,] = entity
  regulation
}

regulation['location'] = rep(NA,dim(regulation)[1])
regulation['person'] = rep(NA,dim(regulation)[1])
regulation['organization'] = rep(NA,dim(regulation)[1])


  for(i in 1:dim(regulation)[1]){
  a = as.character(regulation[i,'Comment'])
  if(a == ""){
    next
  }
  a_annotations <- annotate(a, pipeline)
  a_doc <- AnnotatedPlainTextDocument(a, a_annotations)
  regulation = formdf(i,'person',a_doc,regulation)
  regulation = formdf(i,'location',a_doc,regulation)
  regulation = formdf(i,'organization',a_doc,regulation)
  print(i)
}


write.csv(regulation,'regulation_entity.csv',row.names = FALSE)



























