df<-data.frame(group=c("A","B","C","D"),numb=c(12,24,36,48))
g<-ggplot(df,aes(group,numb))+geom_bar(stat="identity")
g+geom_path(x=c(1,1,2,2),y=c(25,26,26,25))+
  geom_path(x=c(2,2,3,3),y=c(37,38,38,37))+
  geom_path(x=c(3,3,4,4),y=c(49,50,50,49))+
  annotate("text",x=1.5,y=27,label="p=0.012")+
  annotate("text",x=2.5,y=39,label="p<0.0001")+
  annotate("text",x=3.5,y=51,label="p<0.0001")

#STARTED HERE

library(xlsx)
library(ggplot2)
library(gridExtra)

SPT <- read.xlsx("c:/users/christos/desktop/working data folder/M-F.xlsx", sheetIndex=1)
View(SPT)

str(SPT)
dim(SPT)
nrow(SPT)
ncol(SPT)
with(SPT, Male + Female)
SPT$Male + SPT$Female
ggplot(SPT, aes(x = Allergen, fill=Male)) + geom_bar(position = "dodge")

SPT_sex2 <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_sex2.xlsx", sheetIndex=1)
View(SPT_sex2)

#this is your basic plot
a<-ggplot(SPT_sex2, aes(x=Allergen, y=SPT, fill=Sex)) + geom_bar(position = "dodge",  stat="identity")


#try this
a + theme_bw()
#and this
a + theme_minimal()
#getting rid of the label
a + xlab("")
#combinations
a + theme_minimal()+ xlab("") + scale_fill_discrete(name = "") + scale_y_continuous(limits=c(0, 60)) 




b<- ggplot(SPT_sex2, aes(x=Allergen, y=SPT, fill=Sex)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(position = "dodge",  stat="identity")
b

#combinations
b + theme_minimal()+ ylab("SPT %") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 60)) 

#so far so good!!! NOW TRYING TO ADD pvalues
SPT_sex2 <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_sex2.xlsx", sheetIndex=1)
View(SPT_sex2)

b<- ggplot(SPT_sex2, aes(x=Allergen, y=SPT, fill=Sex)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge",  stat="identity")
b

#combinations
b + theme_minimal()+ ylab("SPT %") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 60)) +
  annotate("text",x=1.0,y=60,label="p=0.012")+
  annotate("text",x=2.0,y=24,label="p=0.012")+
  annotate("text",x=3.0,y=36,label="p=0.012")+
  annotate("text",x=4.0,y=59,label="p=0.012")+
  annotate("text",x=5.0,y=46,label="p=0.012")+
  annotate("text",x=6.0,y=21,label="p=0.012")+
  annotate(x=c(0.75,0.75,1.25,1.25),y=c(55,57,57,55),"path")+
  annotate(x=c(1.75,1.75,2.25,2.25),y=c(19,21,21,19),"path")+
  annotate(x=c(2.75,2.75,3.25,3.25),y=c(31,33,33,31),"path")+
  annotate(x=c(3.75,3.75,4.25,4.25),y=c(54,56,56,54),"path")+
  annotate(x=c(4.75,4.75,5.25,5.25),y=c(41,43,43,41),"path")+
  annotate(x=c(5.75,5.75,6.25,6.25),y=c(16,18,18,16),"path")
  

  
#run the above with correct values

b + theme_minimal()+ ylab("SPT %") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 60)) +
  annotate("text",x=2.0,y=24,label="p=0.017")+
  annotate("text",x=4.0,y=59,label="p=0.008")+
  annotate("text",x=5.0,y=46,label="p=0.022")+
  annotate(x=c(1.75,1.75,2.25,2.25),y=c(19,21,21,19),"path")+
  annotate(x=c(3.75,3.75,4.25,4.25),y=c(54,56,56,54),"path")+
  annotate(x=c(4.75,4.75,5.25,5.25),y=c(41,43,43,41),"path")

  
#doing the same for adults  
  
SPT_age <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_age.xlsx", sheetIndex=1)
View(SPT_age)

c<- ggplot(SPT_age, aes(x=Allergen, y=SPT, fill=Age)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge",  stat="identity")
c

c + theme_minimal()+ ylab("SPT %") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 60)) +
  annotate("text",x=3.0,y=50,label="p<0.001")+
  annotate(x=c(2.75,2.75,3.25,3.25),y=c(45,47,47,45),"path")

#now for total group

SPT_total <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_total.xlsx", sheetIndex=1)
View(SPT_age)

d<- ggplot(SPT_total, aes(x=Allergen, y=SPT)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge", width=0.65, stat="identity", fill="grey37")
d

d + theme_minimal()+ ylab("SPT %") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_y_continuous(limits=c(0, 60))

#checking effects: intermittent vs. persistent 

SPT_int <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_int.xlsx", sheetIndex=1)
View(SPT_int)

g<- ggplot(SPT_int, aes(x=Allergen, y=SPT, fill=Effect)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge",  stat="identity")
g

g + theme_minimal()+ ylab("%") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 100))+
  annotate("text",x=1.0,y=80,label="p<0.001")+
  annotate("text",x=2.0,y=95,label="p<0.001")+
  annotate("text",x=3.0,y=86,label="p<0.001")+
  annotate("text",x=4.0,y=86,label="p<0.001")+
  annotate("text",x=5.0,y=85,label="p<0.001")+
  annotate("text",x=6.0,y=80,label="p<0.001")+
  annotate(x=c(0.75,0.75,1.25,1.25),y=c(73,75,75,73),"path")+
  annotate(x=c(1.75,1.75,2.25,2.25),y=c(89,91,91,89),"path")+
  annotate(x=c(2.75,2.75,3.25,3.25),y=c(79,81,81,79),"path")+
  annotate(x=c(3.75,3.75,4.25,4.25),y=c(79,81,81,79),"path")+
  annotate(x=c(4.75,4.75,5.25,5.25),y=c(77,79,79,77),"path")+
  annotate(x=c(5.75,5.75,6.25,6.25),y=c(73,75,75,73),"path")


#checking effects: mild vs. severe 

SPT_sever <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_sever.xlsx", sheetIndex=1)
View(SPT_sever)

f<- ggplot(SPT_sever, aes(x=Allergen, y=SPT, fill=Effect)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge",  stat="identity")
f

f + theme_minimal()+ ylab("%") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 100))+
  annotate("text",x=1.0,y=80,label="p<0.001")+
  annotate("text",x=2.0,y=99,label="p<0.001")+
  annotate("text",x=3.0,y=94,label="p<0.001")+
  annotate("text",x=4.0,y=90,label="p<0.001")+
  annotate("text",x=5.0,y=84,label="p<0.001")+
  annotate("text",x=6.0,y=84,label="p<0.001")+
  annotate(x=c(0.75,0.75,1.25,1.25),y=c(73,75,75,73),"path")+
  annotate(x=c(1.75,1.75,2.25,2.25),y=c(92,94,94,92),"path")+
  annotate(x=c(2.75,2.75,3.25,3.25),y=c(88,90,90,88),"path")+
  annotate(x=c(3.75,3.75,4.25,4.25),y=c(82,84,84,82),"path")+
  annotate(x=c(4.75,4.75,5.25,5.25),y=c(76,78,78,76),"path")+
  annotate(x=c(5.75,5.75,6.25,6.25),y=c(76,78,78,76),"path")

#checking effects: seasonal vs. perennial 

SPT_season <- read.xlsx("c:/users/christos/desktop/working data folder/SPT_season.xlsx", sheetIndex=1)
View(SPT_season)

h<- ggplot(SPT_season, aes(x=Allergen, y=SPT, fill=Effect)) + 
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.text.x = element_text(face="plain", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  geom_bar(colour="black", position = "dodge",  stat="identity")
h

h + theme_minimal()+ ylab("%") +
  theme(text = element_text(size=16), axis.title.x = element_blank(), 
        axis.title.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", colour="black", size=16), 
        axis.text.y = element_text(face="plain",colour="black", size=16)) +
  scale_fill_discrete(name = "") +
  theme(legend.text = element_text(size=16, face="plain"))+
  scale_y_continuous(limits=c(0, 100))+
  annotate("text",x=1.0,y=76,label="p<0.001")+
  annotate("text",x=2.0,y=80,label="p=0.003")+
  annotate("text",x=3.0,y=73,label="p=0.009")+
  annotate(x=c(0.75,0.75,1.25,1.25),y=c(70,72,72,70),"path")+
  annotate(x=c(1.75,1.75,2.25,2.25),y=c(74,76,76,74),"path")+
  annotate(x=c(2.75,2.75,3.25,3.25),y=c(67,69,69,67),"path")

