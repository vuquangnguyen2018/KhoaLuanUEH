clear
import excel "C:\Users\Vu Quang Nguyen\Working\KhoaLuanUEH\Dataset\Report.xlsx", sheet("Output") firstrow
drop A

summarize GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID STLEV LTLEV BLEV
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID

// # TESTING MODEL: 
// # Chow: H0: OLS Gop phu hop , H1: FE phu hop -> p_value <5% -> Chon FE
// # Hausman: H0: RE phu hop, H1: FE phu hop -> p_value <5% -> chon FE
// # Breusch-Pagan Test H0: OLS Gop; H1: RE Phu hop -> p_value < 5% -> Chon RE

// --------------------------------------------------------------------------

// Mo hinh Phan tich cho STLEV
reg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID
// --> KIEM DINH F TEST voi H0: POOLED OLS va H1: FEM 
test GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID


// --------------------------------------------------------------------------
// MO HINH FE and RE
xtset ID_TICKET
// Hoi quy FEM ----------------------
xtreg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID,fe
estimates store fe
// Hoi quy REM ----------------------
xtreg STLEV GRWTH SIZE PROF LIQD UNIQ TANG INDS  GDP COVID,re
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
