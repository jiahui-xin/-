#fileName1 <- "唐诗三百首.txt" 
fileName1 <- "全唐诗.txt"
#fileName1 <- "宋词三百首.txt"
#setwd("C:/Users/Sai Li/Downloads")
TS <- readChar(fileName1, file.info(fileName1)$size, useBytes = T)
nchar(TS)
#head(TS)
#fileName2 <- "宋词三百首.txt" 
#SC <- readChar(fileName2, file.info(fileName2)$size)
#nchar(SC)
#SC.corpus<-strsplit(SC, split='\r\n')[[1]]
TS.corpus<-strsplit(TS, split='\r\n')[[1]]
len.ts <- length(TS.corpus) #2536
headlines<-grep("[0-9]", TS.corpus)
TS.corpus1<-list()
j=1
for(i in 1:(len.ts-3)){
  #全唐诗 nchar(TS.corpus[i])>=16; 唐诗三百首>=12
  if(!(i %in%headlines) & nchar(TS.corpus[i])>=16 & nchar(TS.corpus[i])%%2==0){#not headline or empty line
    TS.corpus1[[j]]<-TS.corpus[i]
    j=j+1
  }
}
length(TS.corpus1) #1465
head(TS.corpus1)
filter_func<-function(x){
  strsplit(x,"\\s+|，|。")[[1]]
}

str.dic<-lapply(TS.corpus1,filter_func)



str.dic<-unique(unlist(str.dic)) #2362 unique chars

str.dic<-str.dic[-1]

str.dic<-c("s",str.dic)


p<-length(str.dic)

char.dic<-paste0(str.dic,collapse="")
char.dic<-(strsplit(char.dic,"")[[1]])
char.dic<-unique(char.dic)
q<-length(char.dic)

bigram <- vector(mode = "list", length = q)

for(i in seq(2,p,2)){#every sentence contains two str.dic
  char.cur1=strsplit(str.dic[i],split="")[[1]]
  char.cur<-c("s",char.cur1)
  char.cur2=strsplit(str.dic[i+1],split="")[[1]]
  char.cur<-c(char.cur,"s",char.cur2)
  len.cur=length(char.cur)
  for(k in 1:(len.cur-1)){
    if(char.cur[k+1]=='s'){next} #the end of the sentence
    loc<-which(char.dic==char.cur[k])
    bigram[[loc]]<-c(bigram[[loc]],char.cur[k+1])
  }
}


















#set.seed(1234)
write.poem<-function(bigram, char.dic,n.char=5, n.rows=4){
  word.gen<-function(char.vec, rand=T){
    tab<-table(char.vec)
    df<-data.frame(name=names(tab), prob=as.matrix(tab)/length(char.vec))
    #add
    df$prob=df$prob+1e-6
    #end add
    df$prob<-df$prob/sum(df$prob)
    if(rand){
      sub1<-which(df$prob>=quantile(df$prob,0.9))
      prob1<-df$prob[sub1]/sum(df$prob[sub1])
      sel.char<-sub1[which.max(rmultinom(1,1,prob1))]
    }else{
      sel.char<-which.max(df$prob)
    }
    df$name[sel.char]
  }
  poet<-vector(mode = "list", length = n.rows)
  s.loc<-which(char.dic=='s') #start
  for(i in 1: n.rows){
    poet[[i]]<-word.gen(char.vec=bigram[[s.loc]])
    for(j in 2:n.char){
      pre.loc<-which(char.dic==poet[[i]][j-1])
      poet[[i]]<-c(poet[[i]],word.gen(bigram[[pre.loc]]))
    }
  }
  poet
}
write.poem(bigram, char.dic, n.char=5)


# Two bigram, bigram1 for the former sentence and bigram2 for the latter

bigram1 <- vector(mode = "list", length = q)
bigram2 <- vector(mode = "list", length = q)

for(i in seq(2,p,2)){#every sentence contains two str.dic
  char.cur1=strsplit(str.dic[i],split="")[[1]]
  char.cur1<-c("s",char.cur1)
  char.cur2=strsplit(str.dic[i+1],split="")[[1]]
  char.cur2<-c("s",char.cur2)
  len.cur1=length(char.cur1)
  len.cur2=length(char.cur2)
  for(k in 1:(len.cur1-1)){
    loc<-which(char.dic==char.cur1[k])
    bigram1[[loc]]<-c(bigram1[[loc]],char.cur1[k+1])
  }
  for(k in 1:(len.cur2-1)){
    loc<-which(char.dic==char.cur2[k])
    bigram2[[loc]]<-c(bigram2[[loc]],char.cur2[k+1])
  }
  if(i%%10000==0)print(i)#loop print
}


#set.seed(1234)
my.write.poem<-function(bigram1,bigram2, char.dic,n.char=5, n.rows=4){
  word.gen<-function(char.vec, rand=T){
    tab<-table(char.vec)
    df<-data.frame(name=names(tab), prob=as.matrix(tab)/length(char.vec))
    #add
    df$prob=df$prob+1e-6
    #end add
    df$prob<-df$prob/sum(df$prob)
    if(rand){
      sub1<-which(df$prob>=quantile(df$prob,0.9))
      prob1<-df$prob[sub1]/sum(df$prob[sub1])
      sel.char<-sub1[which.max(rmultinom(1,1,prob1))]
    }else{
      sel.char<-which.max(df$prob)
    }
    df$name[sel.char]
  }
  poet<-vector(mode = "list", length = n.rows)
  s.loc<-which(char.dic=='s') #start
  for(i in seq(1, n.rows,2)){
    poet[[i]]<-word.gen(char.vec=bigram1[[s.loc]])
    for(j in 2:n.char){
      pre.loc<-which(char.dic==poet[[i]][j-1])
      poet[[i]]<-c(poet[[i]],word.gen(bigram1[[pre.loc]]))
    }
  }
  s.loc<-which(char.dic=='s') #start
  for(i in seq(2, n.rows,2)){
    poet[[i]]<-word.gen(char.vec=bigram2[[s.loc]])
    for(j in 2:n.char){
      pre.loc<-which(char.dic==poet[[i]][j-1])
      poet[[i]]<-c(poet[[i]],word.gen(bigram2[[pre.loc]]))
    }
  }
  poet
}
my.write.poem(bigram1,bigram2, char.dic, n.char=5)

#select 李世民 poems but also need addtional comparison

which(str.dic[1:1e4]=="隔岫断猿吟")#964
lstm<-c()
for(i in seq(1,964,2)){
  lstm<-c(lstm,paste0(str.dic[i],"，",str.dic[i+1],"。"))
}
writeLines(lstm,"lstm.txt",useByte=T)

