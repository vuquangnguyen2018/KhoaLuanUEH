library(readxl)
library(dplyr)
library(janitor) # GROUP SUMMARY
library(readr) # MA TRAN PEARSON
library(strucchange) # KIEM DINH CHOW
library(plm) # KIEM DINH HAUSMAN
library(lmtest) # KIEM DINH Breusch-Pagan Test.
library(openxlsx)
library(ggplot2)
library(hrbrthemes)

#==================IMPORT===========================================================================
rm(list = ls()) # CLEAR

Output<- read_excel("C:/Users/Vu Quang Nguyen/Working/KhoaLuanUEH/Dataset/Report.xlsx", 
                    sheet = "Output")

Output<-select(Output,-1)
Output <- Output %>% 
  mutate(ID=row_number())

Output <- Output %>% 
  mutate(NhomNganh=`Nhom nganh`)

Output<-Output %>%  # HOSE = 1; HNX =2 ; UPCOM =3
  mutate(ID_San = ifelse(San == "HOSE", 1, 
                     ifelse(San == "HNX", 2, 3)))

Output %>%
  filter_at(vars(GRWTH,SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS), all_vars(!is.na(.))) %>%
  select(GRWTH,SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID) %>%
  summary()


#--------- SETTING REGRESSION

# POLS------------------------
pooling <- plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,
          data = Output, 
          index = c("INDS","ID_Year"),
          model = "pooling")
summary(pooling)


# FE------------------------
fe <- plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,
          data = Output, 
          index = c("INDS","ID_Year"),
          model = "within")
summary(fe)

#RANDOM EFFECT MODEL-------------------------------


# RE------------------------
re <- plm(STLEV ~ GRWTH +SIZE + LIQD +TANG + UNIQ, 
          data = Output, 
          index = c("INDS","ID_Year"),
          model = "random")
summary(re)



#_---------- TESTING
# LM TEST for Random vs OLS (Lagrange Multiplier Test - (Honda)), H0: OLS phù hợp, H1: REM Phù hợp
plmtest(pooling)

#LM TEST for Fixed vs OLS (F-test): H0 OLS phù hợp; H1: FEM phù hợp
pFtest(fe,pooling)

#Hausman Test for Fixed VS RANDOM: H0: REM Phù hợp; H1: FEM Phù hợp
phtest(re,fe)


#=============================================================================================


# PEARSON TABULATE CORRELATION
Corr <- cor(Output %>% 
              na.omit %>% 
              select(GRWTH,SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID)) # lenh na.omit de loai bo dong na (missing value)

upper<-round(Corr,4)
upper[upper.tri(Corr)]<-""
upper<-as.data.frame(upper)
upper
View(upper)

#-------------- PLOT=======================================================================



p<-ggplot(data = Output,
          aes(x=`Nhom nganh`,y = SIZE)) +
  geom_boxplot()
p

#------------- SIZE
pSize<-ggplot(data = Output,
          aes(x=San,y = SIZE)) +
  geom_boxplot()
pSize


#------------- Growth
pGrowth<-ggplot(data = Output,
          aes(x=`Nhom nganh`,y = GRWTH)) +
  geom_boxplot()
pGrowth



#------------- Prof
pProf<-ggplot(data = Output,
                aes(x=`Nhom nganh`,y = PROF)) +
  geom_boxplot()
pProf



#------------- Liqd
pLiqd<-ggplot(data = Output,
                aes(x=`Nhom nganh`,y = LIQD)) +
  geom_boxplot()
pLiqd


#------------- Tang
pTang<-ggplot(data = Output,
              aes(x=`Nhom nganh`,y = TANG)) +
  geom_boxplot()
pTang

#------------- STLEV
pSTLEV<-ggplot(data = Output,
              aes(x=`Nhom nganh`,y = STLEV)) +
  geom_boxplot()
pSTLEV


#------------- LTLEV
pLTLEV<-ggplot(data = Output,
               aes(x=`Nhom nganh`,y = LTLEV)) +
  geom_boxplot()
pLTLEV
#------------- BLEV
pBLEV<-ggplot(data = Output,
               aes(x=`Nhom nganh`,y = BLEV)) +
  geom_boxplot()
pBLEV



#-----
pBar <-ggplot(data = Output,
              aes(x=`Nhom nganh`, y=SIZE)) + geom_bar()
pBar

# Grouped
p<-ggplot(data = Output, aes(fill=`Nhom nganh`, y=SIZE, x=San)) + 
  geom_bar(position="dodge", stat="identity")
p
#----------------

# Barplot

setwd("C:/Users/Vu Quang Nguyen/Working/KhoaLuanUEH/Dataset/Plots")

pGRWTH<-Output %>%
  ggplot(aes(x = GRWTH, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")
ggsave("pGRWTH.png", plot = pGRWTH)

pSIZE<-Output %>%
  ggplot(aes(x = SIZE, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("pSIZE.png", plot = pSIZE)


p_PROF<-Output %>%
  ggplot(aes(x = PROF, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_PROF.png", plot = p_PROF)

p_LIQD<-Output %>%
  ggplot(aes(x = LIQD, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_LIQD.png", plot = p_LIQD)


p_PROF<-Output %>%
  ggplot(aes(x = PROF, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_PROF.png", plot = p_PROF)

p_UNIQ<-Output %>%
  ggplot(aes(x = UNIQ, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_UNIQ.png", plot = p_UNIQ)

p_TANG<-Output %>%
  ggplot(aes(x = TANG, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_TANG.png", plot = p_TANG)


p_STLEV<-Output %>%
  ggplot(aes(x = STLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_STLEV.png", plot = p_STLEV)


p_LTLEV<-Output %>%
  ggplot(aes(x = LTLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_LTLEV.png", plot = p_LTLEV)



p_BLEV<-Output %>%
  ggplot(aes(x = BLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean")

ggsave("p_BLEV.png", plot = p_BLEV)


#--------------

pGRWTH<-Output %>%
  ggplot( aes(x=GRWTH, y=`Nhom nganh`)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
  ggsave("pGRWTH.png", plot = pGRWTH)
  
  
Output %>%
  ggplot(aes(x = SIZE, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")



Output %>%
  ggplot(aes(x = PROF, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")



Output %>%
  ggplot(aes(x = LIQD, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")



Output %>%
  ggplot(aes(x = UNIQ, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")


Output %>%
  ggplot(aes(x = TANG, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")


Output %>%
  ggplot(aes(x = STLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")

Output %>%
  ggplot(aes(x = LTLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")

Output %>%
  ggplot(aes(x = BLEV, y = `Nhom nganh`, fill=San)) +
  geom_bar(stat = "identity")

 

# Plot
Output %>%
  tail(10) %>%
  ggplot( aes(x=ID_Year, y=SIZE)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("SIZE")


# Plot
Output %>%
  ggplot( aes(x=ID_Year, y=SIZE)) +
  geom_line()

#--------

Data <- read_excel("C:/Users/Vu Quang Nguyen/Working/KhoaLuanUEH/Dataset/Report.xlsx", 
                     sheet = "Output")
Data<-select(Data,-1)

#-- FILTERING
Top_Ticket<-Data %>% 
  na.omit() %>% # Drop NA 
  filter(case_when(`Nam`==2021 ~  `Tong Tai san`>= 100*10^3 & `Von hoa thi truong`>=1000 )) %>%# Tong Tai san >= 1000B, Von Hoa >=1000B 
  select(`Ma CK`)

Top_Ticket <- Top_Ticket %>% 
  mutate(ID_CK=row_number())

# APPLIED FILTER -> DATASET_1
Data <- right_join(Data,Top_Ticket, by = "Ma CK")
Data<-Data %>%
  na.omit()


View(Data)

# EXPORT CSV/XLSX
#write.csv(Top_Ticket, file="TOP_TICKET.csv",  row.names=FALSE)
#write_xlsx(Data, "OUTPUT_R.xlsx") 




#_-----------
Report<-Data %>%
  group_by(`Ma CK`) %>%
  select(STLEV,LTLEV, BLEV) %>%
  summarise_at(vars(STLEV,LTLEV, BLEV),              # Specify column
               list(mean)) 

View(Report)

#--- Report Nhom nganh
Data %>%
  filter_at(vars(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS), all_vars(!is.na(.))) %>% # Loai bo MISSING VALUES
  group_by(`Nhom nganh`) %>%
  select(STLEV,LTLEV, BLEV) %>%
  summarise_at(vars(STLEV,LTLEV, BLEV),              # Specify column
               list(mean)) 
#--- Report Nhom nganh
Data %>%
  filter_at(vars(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS), all_vars(!is.na(.))) %>%
  group_by(`Nhom nganh`) %>%
  select(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID) %>%
  summarise_at(vars(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID),              # Specify column
               list(mean)) 



# THONG KE MO TA --------------------------
Data %>%
  filter_at(vars(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS), all_vars(!is.na(.))) %>%
  select(`Nhom nganh`,SIZE,PROF,LIQD,UNIQ,TANG) %>%
  split(.$`Nhom nganh`) %>%
  map(summary)


Data %>%
  filter_at(vars(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS), all_vars(!is.na(.))) %>%
  select(SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID) %>%
  summary()




#--OLS
OLS_Model<-lm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,data = Output)
summary(OLS_Model)


#-------------------
plmtest(fe,c("time"),type = ("bp"))
pcdtest(fe,test = ("lm"))


# POOLED OLS------------------------
OLS<-plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID ,data = Data, model = "pooling", index = c("INDS","ID_Year"))
summary(re)
