df<-data.frame(group=c("A","B","C","D"),numb=c(12,24,36,48))
g<-ggplot(df,aes(group,numb))+geom_bar(stat="identity")
g+geom_path(x=c(1,1,2,2),y=c(25,26,26,25))+
  geom_path(x=c(2,2,3,3),y=c(37,38,38,37))+
  geom_path(x=c(3,3,4,4),y=c(49,50,50,49))+
  annotate("text",x=1.5,y=27,label="p=0.012")+
  annotate("text",x=2.5,y=39,label="p<0.0001")+
  annotate("text",x=3.5,y=51,label="p<0.0001")
library(ggplot2)
df<-data.frame(group=c("Grasses","Weeds","Acarea","Trees","Fungi", "Danders"),numb=c(52.5,14.2,52.8,30.8,26.7,10.8))
g<-ggplot(df,aes(group,numb))+geom_bar(stat="identity")

df = melt(data.frame(Male=c(2, 10), Female=c(3, 20), 
                     experiment=c("X", "X & Y")),
          variable_name="metric")
library(ggplot2)
df = melt(data.frame(A=c(2, 10), B=c(3, 20), 
          variable_name="metric"))

ggplot(df, aes(experiment, value, fill=metric)) + 
  geom_bar(position="dodge")

x <- replicate(2, sample(letters[1:2], 100, rep=T))
apply(x, 2, table)
barplot(matrix(c(5,3,8,9),nr=2), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=LETTERS[1:2])
legend("topleft", c("A","B"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n")

df<-data.frame(group=c("A","B","C","D"),numb=c(12,24,36,48))
g<-ggplot(df,aes(group,numb))+geom_bar(stat="identity")
g+geom_path(x=c(1,1,2,2),y=c(25,26,26,25))+
  geom_path(x=c(2,2,3,3),y=c(37,38,38,37))+
  geom_path(x=c(3,3,4,4),y=c(49,50,50,49))+
  annotate("text",x=1.5,y=27,label="p=0.012")+
  annotate("text",x=2.5,y=39,label="p<0.0001")+
  annotate("text",x=3.5,y=51,label="p<0.0001")


means<-aggregate(df,by=list(df$gender),mean)
Group.1      tea     coke     beer    water gender
1       1 87.70171 27.24834 24.27099 37.24007      1
2       2 24.73330 25.27344 25.64657 24.34669      2
means<-means[,2:length(means)]
