library(readxl)
library(dplyr)
library(janitor) # GROUP SUMMARY
library(readr) # MA TRAN PEARSON
library(strucchange) # KIEM DINH CHOW
library(plm) # KIEM DINH HAUSMAN
library(lmtest) # KIEM DINH Breusch-Pagan Test.
library(openxlsx)
library(xlsx)

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
write.csv(Top_Ticket, file="TOP_TICKET.csv",  row.names=FALSE)
write_xlsx(Data, "OUTPUT_R.xlsx") 

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



#MODEL TEST: Method: OLS, RE, FE

# TESTING MODEL: 
# Chow: H0: OLS Gop phu hop , H1: FE phu hop -> p_value <5% -> Chon FE
# Hausman: H0: RE phu hop, H1: FE phu hop -> p_value <5% -> chon FE
# Breusch-Pagan Test H0: OLS Gop; H1: RE Phu hop -> p_value < 5% -> Chon RE

#--MODEL: Short Term STLEV
reg_STLEV <- STLEV ~ GRWTH + SIZE + PROF + LIQD + UNIQ + TANG +GDP +COVID 
#--MODEL: Short Term LTLEV
reg_LTLEV <- LTLEV ~ GRWTH  + PROF + LIQD + UNIQ + TANG +GDP +COVID 
#--MODEL: Short Term BLEV
reg_BLEV <- BLEV ~ GRWTH + PROF + LIQD + UNIQ + TANG +GDP +COVID 



# KIEM DINH CHOW TEST -> SHORT TERM
sctest(reg_STLEV, data = Data, type = "Chow", point = 10)
# KIEM DINH CHOW TEST -> LONG TERM
sctest(reg_LTLEV, data = Data, type = "Chow", point = 10)
# KIEM DINH CHOW TEST -> BOOK VALUE OF DEBT
sctest(reg_BLEV, data = Data, type = "Chow", point = 10)


# KIEM DINH HAUSMAN TEST -> SHORT TERM
phtest(reg_STLEV, data = Data)
# KIEM DINH HAUSMAN TEST -> LONG TERM
phtest(reg_LTLEV, data = Data)
# KIEM DINH HAUSMAN TEST -> BOOK VALUE OF DEBT
phtest(reg_BLEV, data = Data)



# KIEM DINH  Breusch-Pagan Test -> SHORT TERM
bptest(reg_STLEV, data = Data)
# KIEM DINH Breusch-Pagan Test -> LONG TERM
bptest(reg_LTLEV, data = Data)
# KIEM DINH Breusch-Pagan Test -> BOOK VALUE OF DEBT
bptest(reg_BLEV, data = Data)



