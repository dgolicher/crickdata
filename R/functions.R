latest_bowling<-function(yr=2017,npages=1)
{
  require(XML)
  require(lubridate)
  require(dplyr)
  i=1
  url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=bowling;view=innings",sep="")
  tables <-readHTMLTable(url, stringsAsFactors = F)
  t <- tables$"Innings by innings list"

  for (i in 2:npages+1)
  {

    url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=bowling;view=innings",sep="")

    try(tables <-readHTMLTable(url, stringsAsFactors = F))
    try(tt <- tables$"Innings by innings list")
    try(t<-rbind(t,tt))
  }
  d<-t
  d$Player<-as.character(d$Player) ## Can leave the team name in
  d$Country<- unlist(sub("\\).*", "", sub(".*\\(", "", d$Player)) ) ## Extract the country from the player's name
  d$Date<-as.Date(d$"Start Date",format="%d %b %Y") ## Set up the date format
  d$Year<-year(d$Date)
  d$Day<-day(d$Date)
  d$Month<-month(d$Date)
  d$Yday<-yday(d$Date)
  d$Overs<-as.numeric(as.character(d$Overs))
  d$BPO<-6 ## A potential problem here, as all modern games have 6, so column missing
  d$Mdns<-as.numeric(as.character(d$"Mdns"))
  d$Runs<-as.numeric(as.character(d$"Runs"))
  d$type<-d$Opposition
  d$type<-sub(" v.*", "", d$type)
  d$Opposition<- sub(".*v", "v", d$Opposition)
  #d<-d[,-grep("X",names(d))]
  d$Wkts<-as.numeric(as.character(d$Wkts))
  d$Econ<-as.numeric(as.character(d$Econ))
  d$Date<-as.Date(d$Date)
  return(d)
}

latest_batting<-function(yr=2017,npages=1)
{
  require(XML)
  require(lubridate)
  require(dplyr)
  i=1
  url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=batting;view=innings",sep="")
  tables <-readHTMLTable(url, stringsAsFactors = F)
  t <- tables$"Innings by innings list"
  for (i in 2:npages+1)
  {url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=batting;view=innings",sep="")

    try(tables <-readHTMLTable(url, stringsAsFactors = F))
    try(tt <- tables$"Innings by innings list")
    try(t<-rbind(t,tt))
  }
  d<-t
  ## Go through all the same steps

  d$Player<-as.character(d$Player) ## Can leave the team name in
  d$Country<- unlist(sub("\\).*", "", sub(".*\\(", "", d$Player)) ) ## Extract the country from the player's name
  d$Notout<-gsub('[0-9]+', '', d$Runs) ## Set up a column for not out innings
  d$Notout<-gsub("\\*","TRUE",d$Notout) ## Convert asterixs
  d$Runs<-as.numeric(gsub("\\D+", "", d$Runs)) ## Now turn the runs as a numeric column
  d$Date<-as.Date(d$"Start Date",format="%d %b %Y") ## Set up the date format
  d$Year<-year(d$Date)
  d$Day<-day(d$Date)
  d$Month<-month(d$Date)
  d$Yday<-yday(d$Date)
  d$BF<-as.numeric(as.character(d$BF))
  d$SR<-as.numeric(as.character(d$SR))
  d$Fours<-as.numeric(as.character(d$"4s"))
  d$Sixs<-as.numeric(as.character(d$"6s"))
  d$type<-d$Opposition
  d$type<-sub(" v.*", "", d$type)
  d$Opposition<- sub(".*v", "v", d$Opposition)

  return(d)
}

latest_innings<-function(yr=2017,npages=1)
{
yr=2017
    require(XML)
    require(lubridate)
    require(dplyr)
    i=1
    url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=team;view=innings",sep="")
    tables <-readHTMLTable(url, stringsAsFactors = F)
    t <- tables$"Innings by innings list"

    for (i in 2:npages+1)
    {
     url<-paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=",i,";spanmax2=31+Dec+",yr,";spanmin2=5+Aug+2017;spanval2=span;template=results;type=team;view=innings",sep="")

    try(tables <-readHTMLTable(url, stringsAsFactors = F))
    try(tt <- tables$"Innings by innings list")
    try(t<-rbind(t,tt))
}
    d<-t
    d$Total<-as.numeric(unlist(sub("\\/.*", "", d$Score)) )
    d$Date<-as.Date(d$"Start Date",format="%d %b %Y") ## Set up the date format
    d$Year<-year(d$Date)
    d$Day<-day(d$Date)
    d$Month<-month(d$Date)
    d$Yday<-yday(d$Date)
    d$RPO<-as.numeric(as.character(d$RPO))
    d$type<-d$Opposition
    d$type<-sub(" v.*", "", d$type)
    d$Opposition<- sub(".*v", "v", d$Opposition)
    d$Overs<-as.numeric(as.character(d$Overs))
    d$declared<-FALSE
    d$declared[grep("d",d$Score)]<-TRUE
   return(d)
}












