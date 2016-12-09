# Web-scraping

I put the codes for web-scraping projects here.

pubmed.R is the program for web-scraping the search results from pubmed http://www.ncbi.nlm.nih.gov/pubmed/. The program will go through each result on each page, click the link to open it and then write the link, title, author and abstract into a local table. 

googlescholar.R is the program for web-scraping the search results from google scholar. Since Google will block the IP address if you web-scraped like a robot, the program will firstly scrape 4 or 5 pages (randomly choosing), save each search result's Title, Author, Location or Year, Notes into a local table. The program will stop 300 to 600 seconds after scraping one page. After finishing scraping the 4 or 5 pages, the program will stop 3600 to 5400 seconds. Then continue the scraping process like before until the local time is 11:00pm. After 11:00pm, the program will stop for 6 hours.   

googlescholar.R also contained another way to web-scrape which turned out to be more efficient, which is, using R-selenium. And also, use R-selenium to web-scrape when querying the publication year.
