# KPIF descriptive ###########
library(readxl)
dat<-read_excel("~/Desktop/ReservationsExport_20211119_0719 With Districts.xlsx",sheet = "ALL_CLINICS")

dim(dat)

library(dplyr)

dat%>%group_by(District,`KP Labels`)%>%
  summarise(count_num=n())

# Number of KP in Each District
library(stringr)
unique(dat$`KP Labels`)
# create new lable column
dat$kp_label=ifelse(str_detect(dat$`KP Labels`, "MS."), "MSM", 
      ifelse(str_detect(dat$`KP Labels`, "FS."),"FSW",
             ifelse(str_detect(dat$`KP Labels`,"OTHER"),"OTHER",
                    ifelse(str_detect(dat$`KP Labels`,"TG"),"TG",
                           ifelse(str_detect(dat$`KP Labels`,"^PWI"),"PWID",
                                  ifelse(str_detect(dat$`KP Labels`,"^SWCL"),"SWCLIENT",   NA)) ))))

#transfer from long to wide 
d1=data.frame(dat%>%group_by(District,kp_label)%>%
  summarise(count_num=n()))

data.frame(dat%>%group_by(District)%>%
             summarise(count_num=n()))
data.frame(dat%>%group_by(kp_label)%>%
             summarise(count_num=n()))
library(tidyr)
spread(d1,kp_label,count_num)

# age IQR 
my_quantile <- function(x, probs) {
  tibble(x = quantile(x, probs), probs = probs)
}
dat$age=as.numeric(dat$`Age at Booking`)
dat%>%group_by(kp_label)%>%
  summarise(min=min(age),
            Q1=quantile(age,probs = 0.25),
            median=median(age), 
            Q3=quantile(age, probs = 0.75),
            max=max(age))


data.frame(dat%>%group_by(District,kp_label)%>%
  summarise(
            Q1=quantile(age,probs = 0.25),
            median=median(age),
            Q3=quantile(age, probs = 0.75)))


data.frame(dat%>%group_by(District)%>%
             summarise(
               Q1=quantile(age,probs = 0.25),
               median=median(age),
               Q3=quantile(age, probs = 0.75)))

data.frame(dat%>%group_by(kp_label)%>%
             summarise(
               Q1=quantile(age,probs = 0.25),
               median=median(age),
               Q3=quantile(age, probs = 0.75)))
          

dat$District<-chartr("balaka", "Balaka", dat$District)
summary(dat$age)
# HIV STATUS 
# living with HIV
HIV_pos=dat%>%
  filter(str_detect(`CLIN HTS`, "^po.")|str_detect(`RA HIV Status`,"^po."))
dim(HIV_pos
    )
HIV_neg=dat %>%
  filter(!`Reservation ID` %in% HIV_pos$`Reservation ID`)
dim(HIV_neg)

HIV_pos%>%
  group_by(kp_label)%>%
  summarise(count=n())

# loop over all the districts to gather the HIV results 
HIV_status<-list()
districts<-unique(dat$District)
for(i in 1:length(districts)){
  HIV_status[[i]]=dat%>%
    filter(District==districts[i])%>%group_by(`RA HIV Status`,`CLIN HTS`)%>%
    summarise(count=n()) 
}
names(HIV_status)<-districts
HIV_status


HIV_neg%>%
  group_by(kp_label)%>%
  summarise(count=n())




### ART status 


# all district 
HIV_pos%>%
  group_by(`SH ART`)%>%
  summarise(count=n())%>%
  mutate(total=sum(count))%>%
  mutate(proportion=count/total)
  
unique(HIV_pos$`SH ART`)
unique(dat$`SH ART`)



#ART among Those Living with HIV by District, FSW
HIV_pos%>%
  filter(kp_label=="MSM")%>%
  group_by(District,`SH ART`)%>%
  summarise(count=n())%>%
  #group_by(District)%>%
  mutate(total=sum(count))%>%
  mutate(proportion=count/total)

HIV_pos%>%
  filter(kp_label=="MSM")%>%
  group_by(`SH ART`)%>%
  summarise(count=n())%>%
  mutate(total=sum(count))%>%
  mutate(proportion=count/total)
unique(dat$kp_label)
           
ART_total= list()

for (i in 1:4){
  ART_total[[i]]=as.data.frame(HIV_pos%>%
                              filter(kp_label==kp_people[i])%>%
                                 group_by(`SH ART`)%>%
                                 summarise(count=n())%>%
                                 mutate(total=sum(count))%>%
                                 mutate(proportion=count/total))

  
}
           
names(ART_total)<-kp_people
ART_total        
           
HIV_pos%>%
  group_by(`SH ART`,kp_label)%>%
  summarise(count=n())%>%
  #group_by(District)%>%
  mutate(total=sum(count))%>%
  mutate(proportion=count/total)         
           
           