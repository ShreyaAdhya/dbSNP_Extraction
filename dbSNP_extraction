#Load libraries
libraries = c("tidyverse", "stringr",  "rvest", "XML", "purrr", "data.table", "DBI", "httr")
lapply(libraries, require, character.only = TRUE)

#setting wd
setwd("C:/Users/DELL/OneDrive/Desktop/Annotations/Automation")

pos = c(116715567,116736050,116736652,116736721,116741017,116741111,116752497,116752943,116753437,116753987,116754011,116760991,116768388,116769521,116776891,116778201,116780399,116781707,116781775,116782580,116786072,116786845,116789970,116791110,116791863,116792991,116796367,116799960,116800289,116800760,116801107,116801108,116801297,116804578,116809702)

#defining an empty dataframe
mydf = data.frame()
#counter variable
c=1
#working on a fixed chromosome number
chr=10
ID_range <- character() 



#---------------------------------Part1 -> extraction of rsIDs---------------------------------
for (i in seq_along(pos)) {
  URL = (paste0("https://www.ncbi.nlm.nih.gov/snp/?term=11%5BChromosome%5D+AND+",pos[i],"%5BCHRPOS%5D"))
  link = url(URL, "rb")
  page = read_html(link)
  
  Position = pos[i]
  Information = page %>% html_elements(".snpsum_dl_left_align")%>% html_text()
  rsID = page %>% html_elements(".supp > span a")%>% html_text()
  
  my_out <- rsID           
  ID_range <- c(ID_range, my_out)
  
  print(c)
  print(rsID)
  #print(Information)
  #print("---------------------------------------------------------------------------------------------------------------------------")
  

  #creating df 
  mydf = rbind(mydf,data.frame(chr,Position,rsID,Information,URL))  
  
  #counting
  c=c+1
  print("Step",c)
}




#---------------------------------Part2 -> Extraction of SNP information---------------------------------
#introducing the second dataframe
mydf2 = data.frame()
#Gene <- character()


for (i in seq_along(ID_range)) {
  print(ID_range[i])
  #infolink =("https://www.ncbi.nlm.nih.gov/snp/rs180327#variant_details")
  infolink=(paste0("https://www.ncbi.nlm.nih.gov/snp/",ID_range[i],"#variant_details"))
  link2 = url(infolink, "rb")
  page = read_html(link2)


  #Extracting information based on HTML elemnts 
  Allele = str_trim(gsub("[\r\n]", "", page %>% html_elements(".usa-width-one-half:nth-child(1) dd:nth-child(6)")%>% html_text()))
  print(Allele)
  Variant = str_trim(gsub("[\r\n]", "", page %>% html_elements(".usa-width-one-half:nth-child(1) dd:nth-child(8)")%>% html_text()))
 #frequency = page %>% html_elements("dd:nth-child(10)")%>% html_text()
  Frequency = str_trim(gsub("[\r\n]", "", page %>% html_elements("dd:nth-child(10)")%>% html_text()))
  Clinvar = str_trim(gsub("[\r\n]", "", page %>% html_elements(".usa-width-one-half+ .usa-width-one-half dd:nth-child(2)")%>% html_text()))
  Gene = page %>% html_elements("div.gray")%>% html_text()
  l = length(Gene)
  if (l==0) {
    Gene = str_trim(gsub("[\r\n]", "",page %>% html_elements(".usa-width-one-half+ .usa-width-one-half dd:nth-child(4)")%>% html_text()))
  }
  gp = page %>% html_elements("#genomics_placements_table")%>% html_table()
  Gene = str_trim(gsub("[\r\n]", "",page %>% html_elements(".usa-width-one-half+ .usa-width-one-half dd:nth-child(4)")%>% html_text()))

  #Storing extracted data in a dataframe
  mydf2 = rbind(mydf2,data.frame(Allele,Variant,Frequency,Clinvar,Gene,infolink))
 }

#merging both the dataframes 
DF = merge(mydf, mydf2, by="row.names", all.x=TRUE) 


#Converting the data into .csv file
write.csv(DF, "dbSNP_complete.csv", row.names=FALSE)
