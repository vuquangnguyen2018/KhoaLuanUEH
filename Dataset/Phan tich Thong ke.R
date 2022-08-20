library(readxl)
library(dplyr)
library(janitor) # GROUP SUMMARY
library(readr) # MA TRAN PEARSON
library(strucchange) # KIEM DINH CHOW
library(plm) # KIEM DINH HAUSMAN
library(lmtest) # KIEM DINH Breusch-Pagan Test.
library(openxlsx)



Output<- read_excel("C:/Users/Vu Quang Nguyen/Working/KhoaLuanUEH/Dataset/Report.xlsx", 
                    sheet = "Output")

Output<-select(Output,-1)
Output <- Output %>% 
  mutate(ID=row_number())







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

# PEARSON TABULATE CORRELATION
Corr <- cor(Data %>% 
              na.omit %>% 
              select(GRWTH,SIZE,PROF,LIQD,UNIQ,TANG,GDP,COVID,INDS)) # lenh na.omit de loai bo dong na (missing value)

upper<-round(Corr,4)
upper[upper.tri(Corr)]<-""
upper<-as.data.frame(upper)

View(upper)

#--------------


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




#--------- SETTING REGRESSION
# Set data as panel data
pdata <- plm.data(Output, index=c("INDS","ID_Year"))

# FE------------------------
fe<-plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,
        data = Output, 
        model = "within",
        index = c("INDS","ID_Year"))
summary(fe)



# RE------------------------
re<-plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,
        data = Output, model = "random",
        random.method = "swar", 
        random.dfcor = 3,
        index = c("INDS","ID_Year"))

# re<-plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,data = Output, model = "between", index = c("INDS","ID_Year"))

summary(re)

#--OLS
OLS_Model<-lm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID,data = Output)
summary(OLS_Model)


#-------------------
plmtest(fe,c("time"),type = ("bp"))
pcdtest(fe,test = ("lm"))


# POOLED OLS------------------------
OLS<-plm(STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID ,data = Data, model = "pooling", index = c("INDS","ID_Year"))
summary(re)
