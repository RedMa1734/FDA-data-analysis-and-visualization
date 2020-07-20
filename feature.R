rm(list = ls())
getwd()

data=read.csv(file = "[all_data]product&device.csv")
data=data[,-1]
summary(data)
set.seed(1234)



# Feature Engineering

# device+factor
count_device.decision_code=as.data.frame(table(data$device.decision_code))
count_device.decision_code[,2]=log(count_device.decision_code[,2])
count_device.clearance_type=as.data.frame(table(data$device.clearance_type))
count_device.clearance_type[,2]=log(count_device.clearance_type[,2])
count_device.advisory_committee_name=as.data.frame(table(data$device.advisory_committee_name))
count_device.advisory_committee_name[,2]=log(count_device.advisory_committee_name[,2])
count_device.device.regulation_number=as.data.frame(table(data$device.regulation_number))
count_device.device.regulation_number[,2]=log(count_device.device.regulation_number[,2])
count_product.device_class=as.data.frame(table(data$product.device_class))
count_product.device_class[,2]=log(count_product.device_class[,2])

# device+int
count_device.third_party_flag=as.data.frame(table(data$device.third_party_flag))
count_device.expedited_review_flag=as.data.frame(table(data$device.expedited_review_flag))
count_device.company_is_manufacturer=as.data.frame(table(data$device.company_is_manufacturer))
count_device.correspondent_is_manufacturer=as.data.frame(table(data$device.correspondent_is_manufacturer))
count_device.show_indications=as.data.frame(table(data$device.show_indications))
count_device.show_indications=count_device.show_indications[1:2,]
# all zero
#count_device.applicant_contact_is_verified=as.data.frame(table(data$device.applicant_contact_is_verified))
#count_device.correspondent_contact_is_verified=as.data.frame(table(data$device.correspondent_contact_is_verified))

# product+factor
count_product.submission_type=as.data.frame(table(data$product.submission_type))
count_product.submission_type[,2]=log(count_product.submission_type[,2])
count_product.premarket_preview_name=as.data.frame(table(data$product.premarket_preview_name))
count_product.premarket_preview_name[,2]=log(count_product.premarket_preview_name[,2])
count_product.advisory_committee=as.data.frame(table(data$product.advisory_committee))
count_product.advisory_committee[,2]=log(count_product.advisory_committee[,2])
count_product.regulation_number.y=as.data.frame(table(data$product.regulation_number.y))
count_product.regulation_number.y[,2]=log(count_product.regulation_number.y[,2])
# product+int
count_product.gmp_exempt=as.data.frame(table(data$product.gmp_exempt_flag))
count_product.third_party_review_eligible=as.data.frame(table(data$product.third_party_review_eligible))
count_product.implant_flag=as.data.frame(table(data$product.implant_flag))
count_product.life_sustain_support_flag=as.data.frame(table(data$product.life_sustain_support_flag))
count_product.summary_malfunction_reporting=as.data.frame(table(data$product.summary_malfunction_reporting))


# num
#count_product.regulation_number.x=as.data.frame(table(data$product.regulation_number.x))
# logit
#count_product.third_party_review_code_flag_flag=as.data.frame(table(data$product.third_party_review_code))

data_summary_int=data.frame(matrix(nrow = 10,ncol = 3))
colnames(data_summary_int)=c("varialbe","count_0","count_1")
data_summary_int[1,1]="count_device.third_party_flag"
data_summary_int[1,2]=count_device.third_party_flag[1,2]
data_summary_int[1,3]=count_device.third_party_flag[2,2]

data_summary_int[2,1]="count_device.expedited_review_flag"
data_summary_int[2,2]=count_device.expedited_review_flag[1,2]
data_summary_int[2,3]=count_device.expedited_review_flag[2,2]

data_summary_int[3,1]="count_device.company_is_manufacturer"
data_summary_int[3,2]=count_device.company_is_manufacturer[1,2]
data_summary_int[3,3]=count_device.company_is_manufacturer[2,2]

data_summary_int[4,1]="count_device.correspondent_is_manufacturer"
data_summary_int[4,2]=count_device.correspondent_is_manufacturer[1,2]
data_summary_int[4,3]=count_device.correspondent_is_manufacturer[2,2]

data_summary_int[5,1]="count_product.gmp_exempt"
data_summary_int[5,2]=count_product.gmp_exempt[1,2]
data_summary_int[5,3]=count_product.gmp_exempt[2,2]

data_summary_int[6,1]="count_product.third_party_review_eligible"
data_summary_int[6,2]=count_product.third_party_review_eligible[1,2]
data_summary_int[6,3]=count_product.third_party_review_eligible[2,2]

data_summary_int[7,1]="count_product.implant_flag"
data_summary_int[7,2]=count_product.implant_flag[1,2]
data_summary_int[7,3]=count_product.implant_flag[2,2]

data_summary_int[8,1]="count_product.life_sustain_support_flag"
data_summary_int[8,2]=count_product.life_sustain_support_flag[1,2]
data_summary_int[8,3]=count_product.life_sustain_support_flag[2,2]

data_summary_int[9,1]="count_product.summary_malfunction_reporting"
data_summary_int[9,2]=count_product.summary_malfunction_reporting[1,2]
data_summary_int[9,3]=count_product.summary_malfunction_reporting[2,2]

data_summary_int[10,1]="count_device.show_indications"
data_summary_int[10,2]=count_device.show_indications[1,2]
data_summary_int[10,3]=count_device.show_indications[2,2]


data_log_summary_int=data_summary_int
data_log_summary_int[,2:3]=log(data_log_summary_int[,2:3])

rm(count_product.gmp_exempt)
rm(count_product.third_party_review_eligible)
rm(count_product.implant_flag)
rm(count_product.life_sustain_support_flag)
rm(count_product.summary_malfunction_reporting)
rm(count_device.third_party_flag)
rm(count_device.expedited_review_flag)
rm(count_device.company_is_manufacturer)
rm(count_device.correspondent_is_manufacturer)
rm(count_device.show_indications)

# int data/

# facotr cor data & plot
library(corrplot)
library(polycor)
cor_data=data.frame(data$device.advisory_committee_name,
                    data$device.clearance_type,
                    data$device.decision_code,
                    data$device.regulation_number,
                    data$product.advisory_committee,
                    data$product.device_class,
                    data$product.premarket_preview_name,
                    data$product.regulation_number.y,
                    data$product.submission_type)
colnames(cor_data)=c("device.advisory_committee_name",
                     "device.clearance_type",
                     "device.decision_code",
                     "device.regulation_number",
                     "product.advisory_committee",
                     "product.device_class",
                     "product.premarket_preview_name",
                     "product.regulation_number.y",
                     "product.submission_type")

# cor=cor(cor_data,method = "kendall")
cor=hetcor(data = cor_data)
correlations=cor$correlations
correlations[1,5]=0
correlations[5,1]=0
correlations[4,8]=0
correlations[8,4]=0

corrplot(correlations)
rm(cor_data)
rm(cor)
rm(correlations)
# factor cor data & plot/

# date data
library(ggplot2)
library(ggthemes)
date_data=data.frame(data$device.date_receive,data$device.decision_date)
colnames(date_data)=c("receive_date","decision_date")
dd=as.character.Date(date_data)

dd=date_data

dd$receive_date=as.character(dd$receive_date)
dd$decision_date=as.character(dd$decision_date)
dd1=dd[,1]
dd2=dd[,2]

ee1=as.Date(dd1)
ee2=as.Date(dd2)

date_data_remake=data.frame(ee1,ee2)
colnames(date_data_remake)=c("receive_date","decision_date")
rm(dd,dd1,dd2,ee1,ee2)

date_data_remake$time_spent=date_data_remake$decision_date-date_data_remake$receive_date
summary(as.numeric(date_data_remake$time_spent))

# date data/

# boxplot : spent time based on receive_month 
library(lubridate)
date_data_remake$receive_month=month(date_data_remake$receive_date)
date_data_remake$receive_month=as.factor(date_data_remake$receive_month)

boxdata=date_data_remake[sample(nrow(date_data_remake),nrow(date_data_remake)/10), ]
boxdata$time_spent=log(as.numeric(boxdata$time_spent))

ggplot(data = boxdata,aes(x = receive_month, y = as.numeric(time_spent))) +
  geom_boxplot(aes(fill = receive_month))+theme_pander()+ #+theme_wsj()
  ggtitle("Spent time based on Receive month")+xlab("Receive month")+ylab("Logarithm of Count")
rm(boxdata)
# boxplot : spent time based on receive_month /

# boxplot : spent time based on decision_month 

date_data_remake$decision_month=month(date_data_remake$decision_date)
date_data_remake$decision_month=as.factor(date_data_remake$decision_month)
rm(boxdata)
boxdata=date_data_remake[sample(nrow(date_data_remake),nrow(date_data_remake)/10), ]
boxdata$time_spent=log(as.numeric(boxdata$time_spent))

ggplot(data = boxdata,aes(x = decision_month, y = as.numeric(time_spent))) +
  geom_boxplot(aes(fill = decision_month))+theme_pander() + #+theme_wsj()
ggtitle("Spent time based on decision month")+xlab("Decision month")+ylab("Logarithm of Count")
rm(boxdata)
# boxplot : spent time based on decision_month /

# geom hex : spent time based on receive date

hexdata=date_data_remake
hexdata$time_spent=log(as.numeric(hexdata$time_spent))
ggplot(hexdata,aes(x=receive_date,y=as.numeric(time_spent)))+geom_hex(bins = 20)+
  scale_fill_gradient(low="white", high="red")+theme_pander() + #+theme_wsj()
  ggtitle("Spent time based on receive date")+xlab("Receive date")+ylab("Logarithm of Count")


rm(hexdata)
# geom hex : spent time based on receive date/

# geom_density : based on receive_date

# full
densitydata=date_data_remake

ggplot(densitydata,aes(x=receive_date))+
  geom_density(color = 'red', fill = alpha("#FF4500", 0.5))+theme_pander() + #+theme_wsj()
  ggtitle("Density of spent time based on receive date")+xlab("Receive date")+ylab("Density")

# line
ggplot(densitydata,aes(x=receive_date))+geom_density(aes(color = receive_month)) +theme_pander()+ #+theme_wsj()
  ggtitle("Month density of spent time based on receive date")+xlab("Receive date")+ylab("Density")

rm(densitydata)
# geom_density : based on receive_date/

# bar plot 

#bar 1
bardata=count_device.advisory_committee_name
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Device advisory committee name")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()
 

# #bar 2
# bardata=count_device.clearance_type
# ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
#   ggtitle("Device clearance type")+
#   theme(axis.ticks.length=unit(0.1,'cm')) +
#   geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
#   theme_pander()

#bar 3
bardata=count_device.decision_code
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Device decision code")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()

#bar 4-lollipop plot
bardata=count_device.device.regulation_number
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Device regulation number")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  # geom_col(fill='#87CEFA') +
  geom_point(color='red',size=5) +
  geom_segment(aes(x=Var1, xend=Var1, y=8,yend=Freq)) +
  coord_flip() +
  xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()

#bar 5
bardata=count_product.advisory_committee
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Product advisory committee")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()

# #bar 6
# bardata=count_product.device_class
# ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
#   ggtitle("Product device class")+
#   theme(axis.ticks.length=unit(0.1,'cm')) +
#   geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
#   theme_pander()

#bar 7
bardata=count_product.premarket_preview_name
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Product premarket preview name")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()

#bar 8-lollipop plot
bardata=count_product.regulation_number.y
ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
  ggtitle("Product regulation number")+
  theme(axis.ticks.length=unit(0.1,'cm')) +
  # geom_col(fill='#87CEFA') +
  geom_point(color='red',size=5) +
  geom_segment(aes(x=Var1, xend=Var1, y=8,yend=Freq)) +
  coord_flip() +
  xlab(" ")+ylab("Logarithm of Count")+
  theme_pander()

# #bar 9
# bardata=count_product.submission_type
# bardata[1,1]=NA
# ggplot(data = bardata, mapping = aes(x = Var1, y = Freq)) + 
#   ggtitle("Product submission type")+
#   theme(axis.ticks.length=unit(0.1,'cm')) +
#   geom_col(fill='#87CEFA') +coord_flip() +xlab(" ")+ylab("Logarithm of Count")+
#   theme_pander()

rm(bardata)
# bar plot/

# radar plot

# radar 1
radardata=count_product.device_class
radarcolname=radardata[,1]
radarcolname=t(radarcolname)

radardata=radardata[,2]
radardata=t(radardata)
maxmin <- data.frame(
  Attractive = c(13, 0),
  Sincere = c(13, 0),
  Intelligent = c(13, 0),
  Fun = c(13, 0),
  Ambitious = c(13, 0),
  Interest = c(13, 0))
colnames(maxmin)=radarcolname
colnames(radardata)=radarcolname

df=rbind(maxmin,radardata)

radarchart(df,
           pty = 32,
           axistype = 0,
           pcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           pfcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 1,
           palcex = 0.75)
legend("topleft", 
       c("Product device class"),
       fill = c(adjustcolor("cadetblue2", 0.5)),cex =1.2 )
rm(radardata)
rm(df)
rm(radarcolname)
rm(maxmin)

# radar 2 count_device.clearance_type

radardata=count_device.clearance_type
radarcolname=radardata[,1]
radarcolname=t(radarcolname)

radardata=radardata[,2]
radardata=t(radardata)
maxmin <- data.frame(
  Attractive = c(13, 0),
  Sincere = c(13, 0),
  Intelligent = c(13, 0),
  Fun = c(13, 0),
  Ambitious = c(13, 0),
  Interest = c(13, 0))
colnames(maxmin)=radarcolname
colnames(radardata)=radarcolname

df=rbind(maxmin,radardata)

radarchart(df,
           pty = 32,
           axistype = 0,
           pcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           pfcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 1,
           palcex = 0.75)
legend("topleft", 
       c("Device clearance type"),
       fill = c(adjustcolor("cadetblue2", 0.5)),cex =1.2)
rm(radardata)
rm(df)
rm(radarcolname)
rm(maxmin)

# radar 3 count_product.submission_type
radardata=count_product.submission_type
radardata[1,1]=NA
radarcolname=radardata[,1]
radarcolname=t(radarcolname)

radardata=radardata[,2]
radardata=t(radardata)
maxmin <- data.frame(
  Attractive = c(13, 0),
  Sincere = c(13, 0),
  Intelligent = c(13, 0),
  Fun = c(13, 0),
  Ambitious = c(13, 0),
  Interest = c(13, 0),
  main=c(13,0),
  mxad=c(13,0))
colnames(maxmin)=radarcolname
colnames(radardata)=radarcolname

df=rbind(maxmin,radardata)

radarchart(df,
           pty = 32,
           axistype = 0,
           pcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           pfcol = c(adjustcolor("cadetblue2", 0.5), adjustcolor("cadetblue2", 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 1,
           palcex = 0.75)
legend("topleft", 
       c("Product submission type"),
       fill = c(adjustcolor("cadetblue2", 0.5)),cex =1.2)
rm(radardata)
rm(df)
rm(radarcolname)
rm(maxmin)
# radar plot/

# words cloud


library(jiebaR,jiebaRD)
library(wordcloud2)
wordcloud2(demoFreq, size = 1,shape = 'star')
words_cloud_data=as.character(data$device.device_name)

mixseg<-worker("mix")
words_cloud_apart_data<-segment(words_cloud_data,mixseg)

stopwords <- read.table("stopwords.txt")
class(stopwords)
stopwords <- as.character(stopwords[,1])

removewords <- function(target_words,stop_words){
  target_words <- target_words[target_words%in%stop_words==FALSE]
  return(target_words)
}

words_cloud_apart_data <- removewords(words_cloud_apart_data,stopwords)

# word frequency count
words_cloud_freq<-as.data.frame(table(words_cloud_apart_data))

#Drawing word clouds
wordcloud2(words_cloud_freq,shape='diamond') 

# words cloud/