* Constructing the LNI from experiment VOD 2 (November 2008)

clear all
version 14.1
set more off
capture log close

use vod2, clear

drop sibs sex age till town live fshare budget feduc fudeg meduc mudeg

gen x = -1
replace x = member if dec & vlr

bys gid period: egen tmp = max(x)
replace x = tmp
drop tmp

gen xs = -1
replace xs = member if dec & vlr & strong

bys gid period: egen tmp = max(xs)
replace xs = tmp
drop tmp

sort member gid period

* h1 sequences
gen sw1 = 0
bys member gid (period): replace sw1 = 1 if x[_n-2] == x & x[_n-1] == x & x != -1 & period > 2
replace sw1 = 3 if sw1 == 1 & sw1[_n-1] == 0

gen lbda1 = 0
bys member gid (period): replace lbda1 = sw1 + lbda1[_n-1] if period > 1

drop sw1

* h1 sequences (only strong/focal actor)
gen sw1 = 0
bys member gid (period): replace sw1 = 1 if xs[_n-2] == xs & xs[_n-1] == xs & xs != -1 & period > 2
replace sw1 = 3 if sw1 == 1 & sw1[_n-1] == 0

gen lbda1s = 0
bys member gid (period): replace lbda1s = sw1 + lbda1s[_n-1] if period > 1

replace lbda1s = lbda1 if treat==1

drop sw1

* h2 sequences
gen sw2 = 0
bys member gid (period): replace sw2 = 1 if x[_n-2] == x & x[_n-1] != x & x != -1 & x[_n-1] != -1 & period > 2
replace sw2 = 3 if sw2 == 1 & sw2[_n-1] == 0

gen lbda2 = 0
bys member gid (period): replace lbda2 = sw2 + lbda2[_n-1] if period > 1

drop sw2

* h3 sequences
gen sw3 = 0
bys member gid (period): replace sw3 = 1 if x[_n-2] != x & x[_n-1] != x & x[_n-2] != x[_n-1] & x != -1 & x[_n-1] != -1 & x[_n-2] != -1 & period > 2
replace sw3 = 3 if sw3 == 1 & sw3[_n-1] == 0

gen lbda3 = 0
bys member gid (period): replace lbda3 = sw3 + lbda3[_n-1] if period > 1

drop sw3

bys gid: egen m = max(period)
bys gid: egen L1 = max(lbda1)
bys gid: egen L1s = max(lbda1s)
bys gid: egen L2 = max(lbda2)
bys gid: egen L3 = max(lbda3)

gen lni1 = 100 * L1 / m
gen lni1s = 100 * L1s / m
gen lni2 = 100 * L2 / m
gen lni3 = 100 * L3 / m

tabstat lni1s lni2 lni3 if member==1 & period==1, by(treat) s(mean n sem) not
tabstat lni1s lni2 lni3 if sgame==1 & member==1 & period==1, by(treat) s(mean n sem) not
tabstat lni1s lni2 lni3 if sgame==2 & member==1 & period==1, by(treat) s(mean n sem) not
tabstat lni1s lni2 lni3 if sgame==3 & member==1 & period==1, by(treat) s(mean n sem) not

drop m
lab var x "Outcome of interaction (1, 2, 3 or -1)"
lab var xs "Outcome of interaction (strong/focal player only)"
lab var lbda1 "Lambda_1,3 at t"
lab var lbda1s "Lambda_1,3 at t (strong/focal player only)"
lab var lbda2 "Lambda_2,3 at t"
lab var lbda3 "Lambda_3,3 at t"
lab var L1 "Lambda_1,3 entire sequence"
lab var L1s "Lambda_1,3 entire sequence (strong/focal player only)"
lab var L2 "Lambda_2,3 entire sequence"
lab var L3 "Lambda_3,3 entire sequence"
lab var lni1 "LNI_1,3"
lab var lni1s "LNI_1,3 (strong/focal player only)"
lab var lni2 "LNI_2,3"
lab var lni3 "LNI_3,3"

compress
save vod2_lni, replace
clear
