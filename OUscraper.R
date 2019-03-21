library("rvest")

#loop trough sites to get all basketball history
sites=c("https://www.basketball-reference.com/leagues/NBA_2019_games-october.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-november.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-december.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-january.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-february.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-march.html",
        "https://www.basketball-reference.com/leagues/NBA_2019_games-april.html")

#here I scrape basic over/under data for each nba game this season
df=data.frame()
for (xx in 1:NROW(sites)){
  bref=read_html(sites[xx])
  
  Tabledata=html_nodes(bref,xpath = '//*[@id="schedule"]')
  
  htmldf=html_table(Tabledata)[[1]]
  htmldf$pos=c(1:NROW(htmldf))
  htmldf$WS=sites[xx]
  df=rbind(df,htmldf)
}
df=df[,-ncol(df)+2]
colnames(df)[7]="Box Score"
df=df[df$`Box Score`=="Box Score",]
df=df[is.na(df$Date)==F,]
df$q1=0
df$q2=0
df$q3=0
df$q4=0

#follow href and game quarter data-------------
for (j in 888:NROW(df)){
  sess=html_session(df[j,]$WS)
  
  xyz=follow_link(sess,xpath = paste('//*[@id="schedule"]/tbody/tr[',df[j,]$pos,']/td[6]/a',sep = ""))
  gamepage=read_html(xyz$url)
  
  #below is a stack overflow solution. It takes code in comments in HTML and uses it as source code
  alt_tables <- xml2::xml_find_all(gamepage,"//comment()") %>% {
    #Find only commented nodes that contain the regex for html table markup
    raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
    # Remove the comment begin and end tags
    strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                  vectorize_all = FALSE)
    # Loop through the pieces that have tables within markup and 
    # apply the same functions
    lapply(grep("<table", strip_html, value = TRUE), function(i){
      rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
        .[[1]]
    })
  }
  all_tables <- c(
     alt_tables
  )
  qou=all_tables[[2]]
  q1=sum(qou[2:3,2])
  q2=sum(qou[2:3,3])
  q3=sum(qou[2:3,4])
  q4=sum(qou[2:3,5])
  
  df[j,]$q1=q1
  df[j,]$q2=q2
  df[j,]$q3=q3
  df[j,]$q4=q4
  back(xyz)
}
