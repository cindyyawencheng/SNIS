clear all
set more off

cd "/Users/tranganhdo/Dropbox/SNIS/Data"
import delimited "./covData.csv", clear

****************************** t-tests *****************************

**** Attendance ****
* China
mean attendance if country=="China" & year <= 1995 /* .1985646 */
mean attendance if country=="China" & year > 1995 /* .1985646 */

gen treated_C=.
replace treated_C=0 if year <=1995 & country=="China"
replace treated_C=1 if year >1995 & country=="China"

ttest attendance if country=="China", by (treated)

egen att_C = total(attendance) if country=="China", by (year)

graph bar att_C, over(year, label(labsize(vsmall) angle(45))) title ("China's Attendance") ytitle("Number of Meetings")

* Vietnam
mean attendance if country=="Vietnam" & year <= 1995 /* .0287081 */
mean attendance if country=="Vietnam" & year > 1995 /* .3091922  */

gen treated_V=.
replace treated_V=0 if year <=1995 & country=="Vietnam"
replace treated_V=1 if year >1995 & country=="Vietnam"

ttest attendance if country=="Vietnam", by (treated_V)

egen att_V = total(attendance) if country=="Vietnam", by (year)

graph bar att_V, over(year, label(labsize(vsmall) angle(45))) title ("Vietnam's Attendance") ytitle("Number of Meetings")

* combine two graphs -attendance
graph bar att_C att_V, over(year, label(labsize(vsmall) angle(45))) legend(label(1 "China") label(2 "Vietnam")) ytitle("Number of Meetings") title("Attendance of China and Vietnam") note("Source: Codex Alimentarius Webpage")

****** Comments *****
* China
mean comments if country=="China" & year <= 1995 /* .2870813 */
mean comments if country=="China" & year > 1995 /* .902507 */

gen com_C=.
replace com_C=0 if year <=1995 & country=="China"
replace com_C=1 if year >1995 & country=="China"

ttest comments if country=="China", by (com_C)

egen comm_C = total(comments) if country=="China", by (year)

graph bar comm_C, over(year, label(labsize(vsmall) angle(45))) title ("China's Comments") ytitle("Number of Comments")

* Vietnam
mean comments if country=="Vietnam" & year <= 1995 /* 0 */
mean comments if country=="Vietnam" & year > 1995 /* .0752089*/

gen com_V=.
replace com_V=0 if year <=1995 & country=="Vietnam"
replace com_V=1 if year >1995 & country=="Vietnam"

ttest comments if country=="Vietnam", by (com_V)

egen comm_V = total(comments) if country=="Vietnam", by (year)

graph bar comm_V, over(year, label(labsize(vsmall) angle(45))) title ("Vietnam's Comments") ytitle("Number of Comments")

* combine two graphs - comments
graph bar comm_C comm_V, over(year, label(labsize(vsmall) angle(45))) legend(label(1 "China") label(2 "Vietnam")) ytitle("Number of Comments") title("Comments of China and Vietnam") note("Source: Codex Alimentarius")

****** If attendend, comments *****
* China
mean comments if country=="China" & year <=1995 & attendance>0 /* 1.337349 */
mean comments if country=="China" & year >1995 & attendance>0 /* 1.408889 */
egen com_att_C = total(comments) if country=="China" & attendance>0, by (year)

* Vietnam
mean comments if country=="Vietnam" & year <=1995 & attendance>0 /* 0 */
mean comments if country=="Vietnam" & year >1995 & attendance>0 /* .2072072 */
egen com_att_V = total(comments) if country=="Vietnam" & attendance>0, by (year)

graph bar com_att_C com_att_V, over(year, label(labsize(vsmall) angle(45))) legend(label(1 "China") label(2 "Vietnam")) ytitle("Number of Comments") title("Comments of China and Vietnam if attended meeting") note("Source: Codex Alimentarius")

label(1 "China") label(2 "Vietnam")) ytitle("Number of Comments") title("Comments of China and Vietnam") note("Source: Codex Alimentarius")
