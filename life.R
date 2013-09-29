# Sys.setlocale(,"CHS")
require(XML)
require(plyr)
require(ggplot2)
require(ggthemes)

readLifeTables<-function(){
  category<-c("food","fashion","bmm","cool","home","city","travel")
  tribe.var<-c("man.low","man.high","woman.low","woman.high")
  tribe.name<-c("男白","男高","女白","女高")
  url.base<-"file:///C:/Users/xiangyuc/Desktop/output/down/2/"
  #generate htm files urls
  temp<-formatC(0:27,width=3,format="d",flag=0)
  url.list<-sapply(temp,function(x) paste(url.base,"rank_",x,".htm",sep=""))
  
  
  all<-data.frame()
  read<-function(url_id){
    url<-url.list[url_id]
    table<-readHTMLTable(url,which=1,header=F,skip.rows=1)
    table<-table[,c(2:8,10:12)]
    names(table)<-c("id","rank","category","action","source","date_added","date_exceed","interest","quality","title")
    table<-transform(table,
                     id=as.integer(as.vector(id)),
                     rank=as.numeric(as.vector(rank)),
                     action=as.logical(action),
                     date_added=as.POSIXct(date_added),
                     date_exceed=as.POSIXct(date_exceed),
#                      interest=as.integer(as.vector(interest)),
#                      quality=as.integer(as.vector(quality)),
                     title=as.character(title))  
    return(table)
  }  
  
  for(i in 0:3){
    df.tribe<-data.frame()
#     assign(tribe.var[i+1],data.frame(),envir=.GlobalEnv)
    for(j in 1:7){
      table<-read(i*7+j)
      df.tribe<-rbind(df.tribe,table)
#       assign(tribe.var[i+1],rbind(get(tribe.var[i+1]),table),envir=.GlobalEnv)
    }
    df.tribe<-cbind(data.frame(rep(tribe.name[i+1],nrow(df.tribe))),df.tribe)
    all<-rbind(all,df.tribe)
  }
  names(all)[1]<-"tribe"
  return(all)
}

get.tribe<-function(name){
  return(subset(all,subset=(tribe==name)))
}

all<-readLifeTables()
# summarize.judgement<-function(){
#   tribe.temp<-list(rep(tribe.name,each=7))
#   names(tribe.temp)<-c("tribe")
#   output<-data.frame()
#   for(t in tribe.var){
#     output<-rbind(output,ddply(get(t),.(category),summarize,count=length(category),interest=mean(interest),quality=mean(quality)))
#   }
#   output<-cbind(tribe.temp,output)
# }

summarize.judgement<-function(){
  tribe.temp<-list(rep(tribe.name,each=7))
  names(tribe.temp)<-c("tribe")
  output<-data.frame()
  for(t in tribe.var){
    output<-rbind(output,ddply(get(t),.(category),summarize,count=length(category),interest=mean(interest),quality=mean(quality)))
  }
  output<-cbind(tribe.temp,output)
}

summarize.interest<-function(){
  output<-data.frame()
  for(t in tribe.var){
    df.temp<-ddply(get(t),.(category),summarize,interest=mean(interest))
    rownames(df.temp)<-df.temp$category
#     df.temp<-df.temp[,2]
    output<-cbind(output,df.temp)
  }
  names(output)<-tribe.name
  output
}



