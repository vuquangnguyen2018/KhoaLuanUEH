clear
//import excel "C:\Users\Vu Quang Nguyen\Working\KhoaLuanUEH\Dataset\Report.xlsx", sheet("Output") firstrow
import excel "C:\Users\Vu Quang Nguyen\Working\KhoaLuanUEH\Dataset\Report.xlsx", sheet("Output") firstrow
drop A
generate ID = _n

// THONG KE MO TA =======================================================
label define NhomNganh 1 "Công nghiệp" 2 "Hàng Tiêu dùng" 3 "Nguyên vật liệu" 4 "Công nghệ Thông tin" 5 "Dịch vụ Tiêu dùng" 6 "Dầu khí" 7 "Dược phẩm và Y tế"
label values INDS NhomNganh

summarize GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID STLEV LTLEV BLEV
mean GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID STLEV LTLEV BLEV, over(INDS)
tabstat  GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID STLEV LTLEV BLEV, by(INDS)
tabstat STLEV LTLEV BLEV, by(INDS)

// GRAPH
graph hbar (mean) PROF UNIQ TANG , over(INDS)
graph hbar (mean) GRWTH SIZE LIQD  , over(INDS)
graph hbar (mean) STLEV LTLEV BLEV  , over(INDS)

/// CORRELATION
pwcorr GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID, sig star(.01)
// HOI QUY REGRESSION =======================================================
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID

// # TESTING MODEL: 
// # Chow: H0: OLS Gop phu hop , H1: FE phu hop -> p_value <5% -> Chon FE
// # Hausman: H0: RE phu hop, H1: FE phu hop -> p_value <5% -> chon FE
// # Breusch-Pagan Test H0: OLS Gop; H1: RE Phu hop -> p_value < 5% -> Chon RE

// --------------------------------------------------------------------------

// Mo hinh Phan tich cho STLEV
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG   GDP COVID
// --> KIEM DINH F TEST voi H0: POOLED OLS va H1: FEM 
test GRWTH SIZE PROF LIQD UNIQ TANG   GDP COVID


// --------------------------------------------------------------------------
// MO HINH FE and RE
xtset ID Nam
// Hoi quy FEM ----------------------
xtreg STLEV GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID,fe
estimates store fe
// Hoi quy REM ----------------------
xtreg STLEV GRWTH SIZE PROF LIQD UNIQ TANG GDP COVID,re
estimates store re

// KIEM DINH HAUSMAN cho RE va FE
// Hoi quy FEM ----------------------
hausman fe re
// --------------------------------------------------------------------------


// Mo hinh Phan tich cho STLEV
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID
// --> KIEM DINH Da cong Tuyen VIF
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID
vif
