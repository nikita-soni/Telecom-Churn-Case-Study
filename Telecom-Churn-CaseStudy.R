
                      ##### CAPSTONE PROJECT - TELECOM CHURN CASE STUDY #####

# Importing telecom churn dataset
telecom<-read.csv("telecomfinal.csv",stringsAsFactors = FALSE)


#### Understanding Data : Creating Data Quality Report ####

library(dataQualityR)

# Running data quality checks on each variable in dataset
checkDataQuality(telecom,out.file.num = "numeric.csv",out.file.cat = "character.csv")

# Reading data quality report for numeric variables
numeric<-read.csv("numeric.csv")
View(numeric)

# Reading data quality report for categorical variables
character<-read.csv("character.csv")
View(character)


#### Variable Profiling : Continuous Variables ####

library(dplyr)

#(1.) mou_Mean - decreasing trend

telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))

#(2.) totmrc_Mean - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))

#(3.) rev_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))

#(4.) mou_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))

#(5.) change_mou - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))

#(6.) drop_blk_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat6$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat6$varname<-rep("drop_blk_Mean",nrow(dat6))

#(7.)  drop_vce_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat7$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat7$varname<-rep("drop_vce_Range",nrow(dat7))

#(8.) owylis_vce_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat8$LessThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))

#(9.) mou_opkv_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat9$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat9$varname<-rep("mou_opkv_Range",nrow(dat9))

#(10.) months - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat10$LessThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))

#(11.) totcalls - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat11$LessThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))

#(12.) income - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(income,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$N<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(min(income)))[[2]]
dat12$LessThan<-unclass(telecom%>%mutate(dec=ntile(income,n=6))%>%group_by(dec)%>%summarise(max(income)))[[2]]
dat12$varname<-rep("income",nrow(dat12))

#(13.) eqpdays -  profiling could not be done (none of the values from n=2 to 10 worked)

#telecom%>%mutate(dec=ntile(eqpdays,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat13
#dat13$N<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=2))%>%count(dec)%>%unname())[[2]]
#dat13$churn_perc<-dat13$n/dat13$N
#dat13$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=2))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
#dat13$LessThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=2))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
#dat13$varname<-rep("eqpdays",nrow(dat13))

#(14.) custcare_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat14$LessThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=3))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat14$varname<-rep("custcare_Mean",nrow(dat14))

#(15.) callwait_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean ,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean )))[[2]]
dat15$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean ,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean )))[[2]]
dat15$varname<-rep("callwait_Mean ",nrow(dat15))

#(16.) iwylis_vce_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat16$LessThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat16$varname<-rep("iwylis_vce_Mean",nrow(dat16))

#(17.) callwait_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat17$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat17$varname<-rep("callwait_Range",nrow(dat17))

#(18.) ccrndmou_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(dec)%>%unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat18$LessThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat18$varname<-rep("ccrndmou_Range",nrow(dat18))

#(19.) adjqty - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat19$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat19$varname<-rep("adjqty",nrow(dat19))

#(20.) ovrrev_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat20$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat20$varname<-rep("ovrrev_Mean",nrow(dat20))

#(21.) rev_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat21$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat21$varname<-rep("rev_Mean",nrow(dat21))

#(22.) ovrmou_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat22$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat22$varname<-rep("ovrmou_Mean",nrow(dat22))

#(23.) comp_vce_Mean - decreasing trend

telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat23$LessThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat23$varname<-rep("comp_vce_Mean",nrow(dat23))

#(24.) plcd_vce_Mean - decreasing trend

telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat24$LessThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat24$varname<-rep("plcd_vce_Mean",nrow(dat24))

#(25.) avg3mou - decreasing trend

telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat25$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat25$varname<-rep("avg3mou",nrow(dat25))

#(26.) avgmou - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat26$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat26$varname<-rep("avgmou",nrow(dat26))

#(27.) avg3qty - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat27$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat27$varname<-rep("avg3qty",nrow(dat27))

#(28.) avgqty - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat28$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat28$varname<-rep("avgqty",nrow(dat28))

#(29.) avg6mou - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat29$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat29$varname<-rep("avg6mou",nrow(dat29))

#(30.) avg6qty - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$N<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat30$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat30$varname<-rep("avg6qty",nrow(dat30))

#(31.) age1 - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$N<-unclass(telecom%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat31$LessThan<-unclass(telecom%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat31$varname<-rep("age1",nrow(dat31))

#(32.) age2 - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(age2,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(telecom%>%mutate(dec=ntile(age2,n=3))%>%count(dec)%>%unname())[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(age2,n=3))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat32$LessThan<-unclass(telecom%>%mutate(dec=ntile(age2,n=3))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
dat32$varname<-rep("age2",nrow(dat32))

#(33.) models - profiling could not be done (none of the values from n=2 to 10 worked)

#telecom%>%mutate(dec=ntile(models,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat33
#dat33$N<-unclass(telecom%>%mutate(dec=ntile(models,n=2))%>%count(dec)%>%unname())[[2]]
#dat33$churn_perc<-dat33$n/dat33$N
#dat33$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(models,n=2))%>%group_by(dec)%>%summarise(min(models)))[[2]]
#dat33$LessThan<-unclass(telecom%>%mutate(dec=ntile(models,n=2))%>%group_by(dec)%>%summarise(max(models)))[[2]]
#dat33$varname<-rep("models",nrow(dat33))

#(34.) hnd_price - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$N<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat34$LessThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat34$varname<-rep("hnd_price",nrow(dat34))

#(35.) actvsubs - decreasing trend

telecom%>%mutate(dec=ntile(actvsubs,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$N<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=2))%>%count(dec)%>%unname())[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=2))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]
dat35$LessThan<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=2))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]
dat35$varname<-rep("actvsubs",nrow(dat35))

#(36.) uniqsubs - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(uniqsubs,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$N<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=3))%>%count(dec)%>%unname())[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=3))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
dat36$LessThan<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=3))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
dat36$varname<-rep("uniqsubs",nrow(dat36))

#(37.) forgntvl - decreasing trend

telecom%>%mutate(dec=ntile(forgntvl,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$N<-unclass(telecom%>%mutate(dec=ntile(forgntvl,n=2))%>%count(dec)%>%unname())[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(min(forgntvl)))[[2]]
dat37$LessThan<-unclass(telecom%>%mutate(dec=ntile(forgntvl,n=2))%>%group_by(dec)%>%summarise(max(forgntvl)))[[2]]
dat37$varname<-rep("forgntvl",nrow(dat37))

#(38.) opk_dat_Mean - decreasing trend

telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
dat38$LessThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
dat38$varname<-rep("opk_dat_Mean",nrow(dat38))

#(39.) mtrcycle - decreasing trend

telecom%>%mutate(dec=ntile(mtrcycle,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$N<-unclass(telecom%>%mutate(dec=ntile(mtrcycle,n=2))%>%count(dec)%>%unname())[[2]]
dat39$churn_perc<-dat39$n/dat39$N
dat39$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mtrcycle,n=2))%>%group_by(dec)%>%summarise(min(mtrcycle)))[[2]]
dat39$LessThan<-unclass(telecom%>%mutate(dec=ntile(mtrcycle,n=2))%>%group_by(dec)%>%summarise(max(mtrcycle)))[[2]]
dat39$varname<-rep("mtrcycle",nrow(dat39))

#(40.) numbcars - decreasing trend

telecom%>%mutate(dec=ntile(numbcars,n=3))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$N<-unclass(telecom%>%mutate(dec=ntile(numbcars,n=3))%>%count(dec)%>%unname())[[2]]
dat40$churn_perc<-dat40$n/dat40$N
dat40$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(numbcars,n=3))%>%group_by(dec)%>%summarise(min(numbcars)))[[2]]
dat40$LessThan<-unclass(telecom%>%mutate(dec=ntile(numbcars,n=3))%>%group_by(dec)%>%summarise(max(numbcars)))[[2]]
dat40$varname<-rep("numbcars",nrow(dat40))

#(41.) retdays - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(retdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$N<-unclass(telecom%>%mutate(dec=ntile(retdays,n=10))%>%count(dec)%>%unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(retdays,n=10))%>%group_by(dec)%>%summarise(min(retdays)))[[2]]
dat41$LessThan<-unclass(telecom%>%mutate(dec=ntile(retdays,n=10))%>%group_by(dec)%>%summarise(max(retdays)))[[2]]
dat41$varname<-rep("retdays",nrow(dat41))

#(42.) truck - decreasing trend

telecom%>%mutate(dec=ntile(truck,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(telecom%>%mutate(dec=ntile(truck,n=2))%>%count(dec)%>%unname())[[2]]
dat42$churn_perc<-dat42$n/dat42$N
dat42$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(truck,n=2))%>%group_by(dec)%>%summarise(min(truck)))[[2]]
dat42$LessThan<-unclass(telecom%>%mutate(dec=ntile(truck,n=2))%>%group_by(dec)%>%summarise(max(truck)))[[2]]
dat42$varname<-rep("truck",nrow(dat42))

#(43.) roam_Mean - decreasing trend

telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat43$churn_perc<-dat43$n/dat43$N
dat43$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat43$LessThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
dat43$varname<-rep("roam_Mean",nrow(dat43))

#(44.) recv_sms_Mean - decreasing trend

telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat44$churn_perc<-dat44$n/dat44$N
dat44$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
dat44$LessThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
dat44$varname<-rep("recv_sms_Mean",nrow(dat44))

#(45.) blck_dat_Mean - decreasing trend

telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat45$LessThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat45$varname<-rep("blck_dat_Mean",nrow(dat45))

#(46.) mou_pead_Mean - decreasing trend

telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat46$churn_perc<-dat46$n/dat46$N
dat46$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
dat46$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
dat46$varname<-rep("mou_pead_Mean",nrow(dat46))

#(47.) churn - profiling could not be done (none of the values of n worked)

#telecom%>%mutate(dec=ntile(churn,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat47
#dat47$N<-unclass(telecom%>%mutate(dec=ntile(churn,n=2))%>%count(dec)%>%unname())[[2]]
#dat47$churn_perc<-dat47$n/dat47$N
#dat47$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(churn,n=2))%>%group_by(dec)%>%summarise(min(churn)))[[2]]
#dat47$LessThan<-unclass(telecom%>%mutate(dec=ntile(churn,n=2))%>%group_by(dec)%>%summarise(max(churn)))[[2]]
#dat47$varname<-rep("churn",nrow(dat47))

#(48.) da_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat48$churn_perc<-dat48$n/dat48$N
dat48$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat48$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat48$varname<-rep("da_Mean",nrow(dat48))

#(49.) da_Range - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat49$churn_perc<-dat49$n/dat49$N
dat49$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat49$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat49$varname<-rep("da_Range",nrow(dat49))

#(50.) datovr_Mean - decreasing trend

telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat50$churn_perc<-dat50$n/dat50$N
dat50$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat50$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat50$varname<-rep("datovr_Mean",nrow(dat50))

#(51.) datovr_Range - decreasing trend

telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]
dat51$churn_perc<-dat51$n/dat51$N
dat51$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat51$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat51$varname<-rep("datovr_Range",nrow(dat51))

#(52.) drop_dat_Mean - decreasing trend

telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$N<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat52$churn_perc<-dat52$n/dat52$N
dat52$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat52$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat52$varname<-rep("drop_dat_Mean",nrow(dat52))

#(53.) drop_vce_Mean - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat53$churn_perc<-dat53$n/dat53$N
dat53$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat53$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat53$varname<-rep("drop_vce_Mean",nrow(dat53))

#(54.) adjmou - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat54
dat54$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat54$churn_perc<-dat54$n/dat54$N
dat54$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat54$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat54$varname<-rep("adjmou",nrow(dat54))

#(55.) totrev - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat55$churn_perc<-dat55$n/dat55$N
dat55$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat55$LessThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat55$varname<-rep("totrev",nrow(dat55))

#(56.) adjrev - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat56
dat56$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat56$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat56$varname<-rep("adjrev",nrow(dat56))

#(57.) avgrev - both decreasing and increasing trend

telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat57
dat57$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat57$churn_perc<-dat57$n/dat57$N
dat57$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat57$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat57$varname<-rep("avgrev",nrow(dat57))

#(58.) Customer_ID - both increasing and decreasing trend

telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat58
dat58$N<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(dec)%>%unname())[[2]]
dat58$churn_perc<-dat58$n/dat58$N
dat58$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(min(Customer_ID)))[[2]]
dat58$LessThan<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(max(Customer_ID)))[[2]]
dat58$varname<-rep("Customer_ID",nrow(dat58))

#(59.) comp_dat_Mean - decreasing trend

telecom%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat59
dat59$N<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat59$churn_perc<-dat59$n/dat59$N
dat59$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]
dat59$LessThan<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
dat59$varname<-rep("comp_dat_Mean",nrow(dat59))

#(60.) plcd_dat_Mean - decreasing trend

telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat60
dat60$N<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat60$churn_perc<-dat60$n/dat60$N
dat60$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]]
dat60$LessThan<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]]
dat60$varname<-rep("plcd_dat_Mean",nrow(dat60))

# Storing profiles of continuous variables 
cont_profiles<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,
                     dat11,dat12,dat14,dat15,dat16,dat17,dat18,dat19,dat20,
                     dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,
                     dat31,dat32,dat34,dat35,dat36,dat37,dat38,dat39,dat40,
                     dat41,dat42,dat43,dat44,dat45,dat46,dat48,dat49,dat50,
                     dat51,dat52,dat53,dat54,dat55,dat56,dat57,dat58,dat59,dat60)


# Exporting profiles of continuous variables 
write.csv(cont_profiles,"Profiles_Of_Continuous Variables.csv",row.names = F)


#### Variable Profiling : Categorical Variables ####

#(1.) crclscod - 50 levels

telecom%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(telecom%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("crclscod",nrow(datC1))

#(2.) asl_flag - 2 levels

telecom%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC2
datC2$N<-unclass(telecom%>%filter(asl_flag%in%datC2$levels)%>%count(asl_flag))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("asl_flag",nrow(datC2))

#(3.) prizm_social_one - 5 levels 

telecom%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC3
datC3$N<-unclass(telecom%>%filter(prizm_social_one%in%datC3$levels)%>%count(prizm_social_one))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("prizm_social_one",nrow(datC3))

#(4.) area - 19 levels 

telecom%>%count(churn,levels=area)%>%filter(churn==1)->datC4
datC4$N<-unclass(telecom%>%filter(area%in%datC4$levels)%>%count(area))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("area",nrow(datC4))

#(5.) refurb_new - 2 levels

telecom%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC5
datC5$N<-unclass(telecom%>%filter(refurb_new%in%datC5$levels)%>%count(refurb_new))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("refurb_new",nrow(datC5))

#(6.) hnd_webcap - 3 levels

telecom%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC6
datC6$N<-unclass(telecom%>%filter(hnd_webcap%in%datC6$levels)%>%count(hnd_webcap))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("hnd_webcap",nrow(datC6))

#(7.) marital - 5 levels 

telecom%>%count(churn,levels=marital)%>%filter(churn==1)->datC7
datC7$N<-unclass(telecom%>%filter(marital%in%datC7$levels)%>%count(marital))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("marital",nrow(datC7))

#(8.) ethnic - 17 levels 

telecom%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC8
datC8$N<-unclass(telecom%>%filter(ethnic%in%datC8$levels)%>%count(ethnic))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("ethnic",nrow(datC8))

#(9.) dwlltype - 2 levels 

telecom%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC9
datC9$N<-unclass(telecom%>%filter(dwlltype%in%datC9$levels)%>%count(dwlltype))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("dwlltype",nrow(datC9))

#(10.) dwllsize -  15 levels 

telecom%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC10
datC10$N<-unclass(telecom%>%filter(dwllsize%in%datC10$levels)%>%count(dwllsize))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("dwllsize",nrow(datC10))

#(11.) mailordr - 1 level

telecom%>%count(churn,levels=mailordr)%>%filter(churn==1)->datC11
datC11$N<-unclass(telecom%>%filter(mailordr%in%datC11$levels)%>%count(mailordr))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("mailordr",nrow(datC11))

#(12.) occu1 - 20 levels 

telecom%>%count(churn,levels=occu1)%>%filter(churn==1)->datC12
datC12$N<-unclass(telecom%>%filter(occu1%in%datC12$levels)%>%count(occu1))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("occu1",nrow(datC12))

#(13.) wrkwoman - 1 level 

telecom%>%count(churn,levels=wrkwoman)%>%filter(churn==1)->datC13
datC13$N<-unclass(telecom%>%filter(wrkwoman%in%datC13$levels)%>%count(wrkwoman))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("wrkwoman",nrow(datC13))

#(14.) solflag - 2 levels 

telecom%>%count(churn,levels=solflag)%>%filter(churn==1)->datC14
datC14$N<-unclass(telecom%>%filter(solflag%in%datC14$levels)%>%count(solflag))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("solflag",nrow(datC14))

#(15.) proptype - 6 levels 

telecom%>%count(churn,levels=proptype)%>%filter(churn==1)->datC15
datC15$N<-unclass(telecom%>%filter(proptype%in%datC15$levels)%>%count(proptype))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("proptype",nrow(datC15))

#(16.) mailresp - 1 level 

telecom%>%count(churn,levels=mailresp)%>%filter(churn==1)->datC16
datC16$N<-unclass(telecom%>%filter(mailresp%in%datC16$levels)%>%count(mailresp))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("mailresp",nrow(datC16))

#(17.) cartype - 7 leVels 

telecom%>%count(churn,levels=cartype)%>%filter(churn==1)->datC17
datC17$N<-unclass(telecom%>%filter(cartype%in%datC17$levels)%>%count(cartype))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("cartype",nrow(datC17))

#(18.) car_buy -  2 levels 

telecom%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC18
datC18$N<-unclass(telecom%>%filter(car_buy%in%datC18$levels)%>%count(car_buy))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("car_buy",nrow(datC18))

#(19.) children - 2 levels 

telecom%>%count(churn,levels=children)%>%filter(churn==1)->datC19
datC19$N<-unclass(telecom%>%filter(children%in%datC19$levels)%>%count(children))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("children",nrow(datC19))

#(20.) csa - 621 levels 

telecom%>%count(churn,levels=csa)%>%filter(churn==1)->datC20
datC20$N<-unclass(telecom%>%filter(csa%in%datC20$levels)%>%count(csa))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("csa",nrow(datC20))

#(21.) div_type - 3 levels 

telecom%>%count(churn,levels=div_type)%>%filter(churn==1)->datC21
datC21$N<-unclass(telecom%>%filter(div_type%in%datC21$levels)%>%count(div_type))[[2]]
datC21$ChurnPerc<-datC21$n/datC21$N
datC21$Var.Name<-rep("div_type",nrow(datC21))

# Storing profiles of categorical variables
category_profiles<-rbind(datC1,datC2,datC3,datC4,datC5,datC6,datC7,datC8,datC9,datC10,
                         datC11,datC12,datC13,datC14,datC15,datC16,datC17,datC18,datC19,datC20,datC21)

# Exporting profiles of categorical variables
write.csv(category_profiles,"Profiles_Of_Categorical_Variables.csv",row.names = F)


#### Data Preparation : Continuous Variables ####

#(1.) mou_Mean - 6 groups 
mou_MeanC_<-ifelse((dat1$dec==3 | dat1$dec==4),"Group3",ifelse((dat1$dec==5 | dat1$dec==6 | dat1$dec==7),"Group4",ifelse((dat1$dec==8 | dat1$dec==9),"Group5",dat1$dec)))

#(2.) rev_Range - 5 groups 
rev_RangeC_<-ifelse((dat3$dec==1 | dat3$dec==10),"Group1",ifelse((dat3$dec==2 | dat3$dec==4 | dat3$dec==5),"Group2",ifelse((dat3$dec==3 | dat3$dec==6),"Group3",ifelse((dat3$dec==8 | dat3$dec==9),"Group4",dat3$dec))))

#(3.) mou_Range - 5 groups 
mou_RangeC_<-ifelse((dat4$dec==2 | dat4$dec==3 | dat4$dec==4 | dat4$dec==8),"Group2",ifelse((dat4$dec==5 | dat4$dec==6),"Group3",ifelse((dat4$dec==7 | dat4$dec==9),"Group4",dat4$dec)))

#(4.) change_mou - 7 groups 
change_mouC_<-ifelse((dat5$dec==2 | dat5$dec==6),"Group2",ifelse((dat5$dec==3 | dat5$dec==5),"Group3",ifelse((dat5$dec==4 | dat5$dec==10),"Group4",dat5$dec)))

#(5.) drop_blk_Mean - 5 groups
drop_blk_MeanC_<-ifelse((dat6$dec==3 | dat6$dec==4 | dat6$dec==5 | dat6$dec==8),"Group3",ifelse((dat6$dec==7 | dat6$dec==9 | dat6$dec==10),"Group4",dat6$dec))

#(6.) drop_vce_Range - 7 groups
drop_vce_RangeC_<-ifelse((dat7$dec==2 | dat7$dec==7),"Group2",ifelse((dat7$dec==5 | dat7$dec==8 | dat7$dec==10),"Group5",dat7$dec))

#(7.) owylis_vce_Range - 7 groups
owylis_vce_RangeC_<-ifelse((dat8$dec==2 |dat8$dec==3 | dat8$dec==7),"Group2",ifelse((dat8$dec==8 | dat8$dec==10),"Group3",dat8$dec))

#(8.) mou_opkv_Range - 6 groups
mou_opkv_RangeC_<-ifelse((dat9$dec==2 | dat9$dec==3),"Group2",ifelse((dat9$dec==4 | dat9$dec==5),"Group3",ifelse((dat9$dec==6 | dat9$dec==7 | dat9$dec==10),"Group4",dat9$dec)))

#(9.) totcalls - 5 groups
totcallsC_<-ifelse((dat11$dec==1 | dat11$dec==10),"Group1",ifelse((dat11$dec==4 | dat11$dec==5 | dat11$dec==6 | dat11$dec==7| dat11$dec==9),"Group4",dat11$dec))

#(10.) iwylis_vce_Mean - 5 groups
iwylis_vce_MeanC_<-ifelse((dat16$dec==3 | dat16$dec==4),"Group3",dat16$dec)

#(11.) adjqty - 6 groups
adjqtyC_<-ifelse((dat19$dec==1 | dat19$dec==10),"Group1",ifelse((dat19$dec==5 | dat19$dec==6 | dat19$dec==7 | dat19$dec==9),"Group5",dat19$dec))

#(12.) rev_Mean - 5 groups 
rev_MeanC_<-ifelse((dat21$dec==3 | dat21$dec==6 | dat21$dec==8),"Group3",ifelse((dat21$dec==5 | dat21$dec==7 | dat21$dec==9 | dat21$dec==10),"Group5",dat21$dec))

#(13.) comp_vce_Mean - 6 groups
comp_vce_MeanC_<-ifelse((dat23$dec==3 | dat23$dec==4 | dat23$dec==5),"Group3",ifelse((dat23$dec==6 | dat23$dec==7),"Group4",ifelse((dat23$dec==8 | dat23$dec==9),"Group5",dat23$dec)))

#(14.) plcd_vce_Mean - 6 groups
plcd_vce_MeanC_<-ifelse((dat24$dec==3 | dat24$dec==4 | dat24$dec==5 | dat24$dec==6),"Group3",ifelse((dat24$dec==7 | dat24$dec==8),"Group4",dat24$dec))

#(15.) avg3mou - 6 groups
avg3mouC_<-ifelse((dat25$dec==2 | dat25$dec==3),"Group2",ifelse((dat25$dec==5 | dat25$dec==6 | dat25$dec==7),"Group3",ifelse((dat25$dec==8 | dat25$dec==9),"Group5",dat25$dec)))

#(16.) avgmou - 4 groups
avgmouC_<-ifelse((dat26$dec==1 | dat26$dec==3 | dat26$dec==5),"Group1",ifelse((dat26$dec==2 | dat26$dec==4 | dat26$dec==6 | dat26$dec==7),"Group2",ifelse((dat26$dec==8 | dat26$dec==9),"Group3",dat26$dec)))

#(17.) avg3qty - 6 groups
avg3qtyC_<-ifelse((dat27$dec==2 | dat27$dec==5 | dat27$dec==6),"Group2",ifelse((dat27$dec==4 | dat27$dec==7 | dat27$dec==8),"Group4",dat27$dec))

#(18.) avgqty - 4 groups
avgqtyC_<-ifelse((dat28$dec==1 | dat28$dec==8),"Group1",ifelse((dat28$dec==2 | dat28$dec==4 | dat28$dec==5 | dat28$dec==6),"Group2",ifelse((dat28$dec==3 | dat28$dec==7 | dat28$dec==9),"Group3",dat28$dec)))

#(19.) avg6mou - 6 groups
avg6mouC_<-ifelse((dat29$dec==4 | dat29$dec==5 | dat29$dec==7),"Group4",ifelse((dat29$dec==6 | dat29$dec==8 | dat29$dec==9),"Group5",dat29$dec))

#(20.) avg6qty - 5 groups 
avg6qtyC_<-ifelse((dat30$dec==2 | dat30$dec==3 | dat30$dec==5),"Group2",ifelse((dat30$dec==4 | dat30$dec==6),"Group3",ifelse((dat30$dec==7 | dat30$dec==8 | dat30$dec==9),"Group4",dat30$dec)))

#(21.) age1 - 5 groups 
age1C_<-ifelse((dat31$dec==4 | dat31$dec==5),"Group4",dat31$dec)

#(31.) hnd_price - 9 groups
hnd_priceC_<-ifelse((dat34$dec==3 | dat34$dec==4),"Group3",dat34$dec)

#(32.) retdays - 7 groups 
retdaysC_<-ifelse((dat41$dec==4 | dat41$dec==5),"Group4",ifelse((dat41$dec==6 | dat41$dec==8),"Group5",ifelse((dat41$dec==9 | dat41$dec==10),"Group6",dat41$dec)))

#(33.) drop_vce_Mean - 6 groups
drop_vce_MeanC_<-ifelse((dat53$dec==4 | dat53$dec==7 | dat53$dec==8),"Group4",ifelse((dat53$dec==5 | dat53$dec==6 | dat53$dec==9),"Group5",dat53$dec))

#(34.) adjmou - 4 groups
adjmouC_<-ifelse((dat54$dec==1 | dat54$dec==2 | dat54$dec==10),"Group1",ifelse((dat54$dec==3 | dat54$dec==4 | dat54$dec==9),"Group2",ifelse((dat54$dec==5 | dat54$dec==7 | dat54$dec==8),"Group3",dat54$dec)))

#(35.) totrev - 7 groups
totrevC_<-ifelse((dat55$dec==4 | dat55$dec==9),"Group4",ifelse((dat55$dec==5 | dat55$dec==7),"Group5",ifelse((dat55$dec==6 | dat55$dec==8),"Group6",dat55$dec)))

#(36.) adjrev - 7 groups
adjrevC_<-ifelse((dat56$dec==4 | dat56$dec==5 | dat56$dec==7),"Group4",ifelse((dat56$dec==6 | dat56$dec==8),"Group5",dat56$dec))

#(37.) avgrev - 3 groups
avgrevC_<-ifelse((dat57$dec==1 | dat57$dec==5 | dat57$dec==7),"Group1",ifelse((dat57$dec==2 | dat57$dec==3 | dat57$dec==9 | dat57$dec==10),"Group2",ifelse((dat57$dec==4 | dat57$dec==6 | dat57$dec==8),"Group3",dat57$dec)))

#(38.) Customer_ID - 8 groups
Customer_IDC_<-ifelse((dat58$dec==2 | dat58$dec==5),"Group2",ifelse((dat58$dec==3 | dat58$dec==8),"Group3",dat58$dec))


#### Data Preparation : Categorical Variables ####

#(1.) crclscod - 50 levels reduced to 25 levels
crclscodC_<-ifelse((datC1$levels=='A' | datC1$levels=='AA' | datC1$levels=='BA' | datC1$levels=='D2' | datC1$levels=='JF' | datC1$levels=='Z2'),"Group1",ifelse((datC1$levels=='B' | datC1$levels=='GA' | datC1$levels=='K'),"Group2",ifelse((datC1$levels=='B2' | datC1$levels=='C' | datC1$levels=='O'),"Group3",ifelse((datC1$levels=='C2' | datC1$levels=='EA' | datC1$levels=='Z4'),"Group4",ifelse((datC1$levels=='C5' | datC1$levels=='Y'),"Group5",ifelse((datC1$levels=='CA' | datC1$levels=='D' | datC1$levels=='I'),"Group6",ifelse((datC1$levels=='CC' | datC1$levels=='ZA' | datC1$levels=='ZY'),"Group7",ifelse((datC1$levels=='D4' | datC1$levels=='W'),"Group8",ifelse((datC1$levels=='DA' | datC1$levels=='U'),"Group9",ifelse((datC1$levels=='E' | datC1$levels=='J' | datC1$levels=='L' | datC1$levels=='U1'),"Group10",ifelse((datC1$levels=='EF' | datC1$levels=='P1'),"Group11",ifelse((datC1$levels=='G' | datC1$levels=='M' | datC1$levels=='Z'),"Group12",ifelse((datC1$levels=='GY' | datC1$levels=='Z1'),"Group13",datC1$levels)))))))))))))

#(2.) prizm_social_one - 5 levels reduced to 3 levels
prizm_social_oneC_<-ifelse((datC3$levels=='C' | datC3$levels=='S' | datC3$levels=='U'),"Group1",datC3$levels)

#(3.) area - 19 levels reduced to 8 levels 
areaC_<-ifelse((datC4$levels=='ATLANTIC SOUTH AREA' | datC4$levels=='DC/MARYLAND/VIRGINIA AREA' | datC4$levels=='GREAT LAKES AREA'),"Group1",ifelse((datC4$levels=='CALIFORNIA NORTH AREA' | datC4$levels=='NEW ENGLAND AREA'),"Group2",ifelse((datC4$levels=='CENTRAL/SOUTH TEXAS AREA' | datC4$levels=='HOUSTON AREA' | datC4$levels=='OHIO AREA'),"Group3",ifelse((datC4$levels=='CHICAGO AREA' | datC4$levels=='DALLAS AREA' | datC4$levels=='LOS ANGELES AREA' | datC4$levels=='NEW YORK CITY AREA'),"Group4",ifelse((datC4$levels=='MIDWEST AREA' | datC4$levels=='TENNESSEE AREA'),"Group5",ifelse((datC4$levels=='NORTH FLORIDA AREA' | datC4$levels=='PHILADELPHIA AREA' | datC4$levels=='SOUTHWEST AREA'),"Group6",datC4$levels))))))

#(4.) marital - 5 levels reduced to 4 levels 
maritalC_<-ifelse((datC7$levels=='A' | datC7$levels=='B'),"Group1",datC7$levels)

#(5.) ethnic - 17 levels  reduced to 11 levels 
ethnicC_<-ifelse((datC8$levels=='F' | datC8$levels=='G' | datC8$levels=='H' | datC8$levels=='I' | datC8$levels=='R'),"Group1",ifelse((datC8$levels=='P' | datC8$levels=='X'),"Group2",ifelse((datC8$levels=='S' | datC8$levels=='U'),"Group3",datC8$levels)))

#(6.) dwllsize -  15 levels reduced to 8 levels 
dwllsizeC_<-ifelse((datC10$levels=='A' | datC10$levels=='K'),"Group1",ifelse((datC10$levels=='C' | datC10$levels=='N'),"Group2",ifelse((datC10$levels=='E' | datC10$levels=='L'),"Group3",ifelse((datC10$levels=='G' | datC10$levels=='J' | datC10$levels=='M' | datC10$levels=='O'),"Group4",ifelse((datC10$levels=='H' | datC10$levels=='I'),"Group5",datC10$levels)))))

#(7.) occu1 - 20 levels reduced to 12 levels 
occu1C_<-ifelse((datC12$levels=='1' | datC12$levels=='4' | datC12$levels=='6'),"Group1",ifelse((datC12$levels=='2' | datC12$levels=='3' | datC12$levels=='5' | datC12$levels=='8' | datC12$levels=='C'),"Group2",ifelse((datC12$levels=='A' | datC12$levels=='E'),"Group3",ifelse((datC12$levels=='D' | datC12$levels=='F'),"Group4",datC12$levels))))

#(8.) cartype - 7 leVels reduced to 3 levels 
cartypeC_<-ifelse((datC17$levels=='A' | datC17$levels=='D' | datC17$levels=='G'),"Group1",ifelse((datC17$levels=='B' | datC17$levels=='C'),"Group2",ifelse((datC17$levels=='E' | datC17$levels=='F'),"Group3",datC17$levels)))

#(9.) csa - 621 levels reduced to 54 levels
csaC_<-ifelse((datC20$levels=='DETTRO248' | datC20$levels=='LOUCOR812'),"Group1",ifelse((datC20$levels=='OHIWAR330' | datC20$levels=='STLJOS816'),
              "Group2",ifelse((datC20$levels=='AIRBEA843' | datC20$levels=='DALSHR903' | datC20$levels=='NMXAMA806' | datC20$levels=='NORDUL218' | 
               datC20$levels=='OKCMUS918' | datC20$levels=='PITNEW412' | datC20$levels=='SEWWAL509'),"Group3",ifelse((datC20$levels=='DALSTV254' | 
               datC20$levels=='NCRCON704'),"Group4",ifelse((datC20$levels=='AWISHE920' | datC20$levels=='FLNEUS352' | datC20$levels=='LAXIND760' | 
               datC20$levels=='NCRPTR804' | datC20$levels=='NMXPRE520'),"Group5",ifelse((datC20$levels=='APCSAL443' | datC20$levels=='KCYNEW316' | 
               datC20$levels=='MINBLO952' | datC20$levels=='OKCBTN501'),"Group6",ifelse((datC20$levels=='AWIOSH920' | datC20$levels=='FLNAVO863' | 
               datC20$levels=='NYCPAS973' | datC20$levels=='OHIMED330' | datC20$levels=='OHISAN419' | datC20$levels=='PHIGEO302' | datC20$levels=='SFRROC916'),
               "Group7",ifelse((datC20$levels=='AIRROA252' | datC20$levels=='CHIPEO309' | datC20$levels=='CHISPR217' | datC20$levels=='DALCRS903' |
                datC20$levels=='FLNWIL352' | datC20$levels=='INHCRI419' | datC20$levels=='LAXSIM805' | datC20$levels=='NYCWOO732'| datC20$levels=='OHHMOR304' |
                datC20$levels=='OHIMAR740' | datC20$levels=='SDABRK605' | datC20$levels=='SFRCON925' | datC20$levels=='SHEMYE301'),"Group8",
                ifelse((datC20$levels=='HARSPR413' | datC20$levels=='OHINEW740' | datC20$levels=='STLJEF573'),"Group9",ifelse((datC20$levels=='AIRWIL910' |
                datC20$levels=='NEVESC760' | datC20$levels=='SEAABN253'),"Group10",ifelse((datC20$levels=='AIRROC252' | datC20$levels=='AIRWIN252' |
                datC20$levels=='APCEAS443' | datC20$levels=='ATLLAG706' | datC20$levels=='DALSLS903' | datC20$levels=='DETDET313' | datC20$levels=='FLNSEB863' |
                datC20$levels=='FLNZEP813' | datC20$levels=='INDMUN765' | datC20$levels==' MILLAK262' | datC20$levels=='NCRHAR704' | datC20$levels=='NCRKAN704' |
                datC20$levels=='NMXLAR956' | datC20$levels=='OHHCHI740' | datC20$levels=='OHIAKR330' | datC20$levels=='OKCMCA918' | datC20$levels=='PHIJEN215' |
                datC20$levels=='PITIND724' | datC20$levels=='SHEEDI540'),"Group11",ifelse((datC20$levels=='ATLCOL706' | datC20$levels=='ATLKNO423' | datC20$levels=='CHICHA217' |
                datC20$levels=='DETTOL419' | datC20$levels=='KCYKCM816' | datC20$levels=='LAXPER909' | datC20$levels=='LAXRIV909' | datC20$levels=='NCRASH336' |
                datC20$levels=='NMXLCR505' | datC20$levels=='NSHNSH615' | datC20$levels=='NYCETT732' | datC20$levels=='NYCTMR732' | datC20$levels=='OHICAN330' |
                datC20$levels=='SFRDSR925' | datC20$levels=='SFRWOO530' | datC20$levels=='STLSTL314'),"Group12",ifelse((datC20$levels=='ATLATL678' |
                datC20$levels=='CHIGRY219' | datC20$levels=='FLNKIS407' | datC20$levels=='GCWLAF337' | datC20$levels=='KCYKCK913' | datC20$levels=='LAXDOW562' |
                datC20$levels=='LAXMON323' | datC20$levels=='LOUNAL812' | datC20$levels=='MILKEN414' | datC20$levels=='NCRSPN910' | datC20$levels=='NCRWIN336' |
                datC20$levels=='NMXEAG830' | datC20$levels=='NNYROC716' | datC20$levels=='OHHZAN740' | datC20$levels=='PHIPLS609' | datC20$levels=='PITGIB412' |
                datC20$levels=='SANCRP512' | datC20$levels=='SLCOGD801' | datC20$levels=='STLSPR417'),"Group13",ifelse((datC20$levels=='AIRMOR828' |
                datC20$levels=='AIRSAV912' | datC20$levels=='APCBEL443' | datC20$levels=='APCWES443' | datC20$levels=='ATLDOT334' | datC20$levels=='ATLNOR678' |
                datC20$levels=='ATLOVB601' | datC20$levels=='AWIAPP920' | datC20$levels=='DETMON734' | datC20$levels=='DETSOU248' | datC20$levels=='FLNCOC407' |
                datC20$levels=='FLNDAY904' | datC20$levels=='FLNLKC904' | datC20$levels=='GCWGUL228' | datC20$levels=='HOUBMT409' | datC20$levels=='INDIND317' |
                datC20$levels=='KCYTOP913' | datC20$levels=='LAXSJC949' | datC20$levels=='LAXSMN310' | datC20$levels=='LOULEX606' | datC20$levels=='MIAOKE863' |
                datC20$levels=='MINCOR763' | datC20$levels=='NCRALB704' | datC20$levels=='NCRGST704' | datC20$levels=='NCRNWN757' | datC20$levels=='NEVCHU619' |
                datC20$levels=='NEVPOW619' | datC20$levels=='NMCPUE719' | datC20$levels=='NMXELP915' | datC20$levels=='NOLKEN504' | datC20$levels=='NYCFHD732' |
                datC20$levels=='OHHATH740' | datC20$levels=='OHITRT937' | datC20$levels=='OHIYNG330' | datC20$levels=='OKCFTS501' | datC20$levels=='OKCTUL918' |
                datC20$levels=='PHICHC215' | datC20$levels=='PHIMID302' | datC20$levels=='PHIRDN484' | datC20$levels=='PHIVIN609' | datC20$levels=='SANMCA210' |
                datC20$levels=='SANWOO361' | datC20$levels=='SEACDA208' | datC20$levels=='SFRSFS650' | datC20$levels=='STLCPG573'),"Group14",
                ifelse((datC20$levels=='AIRCHA843' | datC20$levels=='APCBAL410' | datC20$levels=='CHIDEC217' | datC20$levels=='FLNLAK941' | datC20$levels=='FLNLEE352' |
                datC20$levels=='FLNSAN407' | datC20$levels=='FLNSAR941' | datC20$levels=='HARLON860' | datC20$levels=='HOUHOU281' | datC20$levels=='INDLAF765' |
                datC20$levels=='KCYWIC316' | datC20$levels=='LAXLAG949' | datC20$levels=='LAXLAX323' | datC20$levels=='LOUETN502' | datC20$levels=='MILWAU262' |
                datC20$levels=='NCRDUR919' | datC20$levels=='NCRWAK919' | datC20$levels=='NYCKPT732' | datC20$levels=='OHIAUR330' | datC20$levels=='OHICLE216' |
                datC20$levels=='OKCLAW580' | datC20$levels=='PITMON412' | datC20$levels=='SANSAN210' | datC20$levels=='SFRCRU831' | datC20$levels=='STLCOL618'),"Group15",
                ifelse((datC20$levels=='AIRORA803' | datC20$levels=='APCFCH703' | datC20$levels=='APCWAS202' | datC20$levels=='ATLANE678' | datC20$levels=='ATLJCK901' |
                datC20$levels=='BOSBOS978' | datC20$levels=='DETANN734' | datC20$levels=='DETFLI810' | datC20$levels=='GCWBTR225' | datC20$levels=='HARNEW203' |
                datC20$levels=='HOUVIC361' | datC20$levels=='LAXANA714' | datC20$levels=='LAXIRV949' | datC20$levels=='LAXPAS626' | datC20$levels=='MIAHWD954' |
                datC20$levels=='MILMIL414' | datC20$levels=='MINMIN612' | datC20$levels=='NCRGRB757' | datC20$levels=='NCRRIC804' | datC20$levels=='NCRWLM757' |
                datC20$levels=='NMXLUB806' | datC20$levels=='NMXTER915' | datC20$levels=='NYCPLS609' | datC20$levels=='OHICOL614' | datC20$levels=='OHIDEL740' |
                datC20$levels=='OHIMRY937' | datC20$levels=='OHIPIQ937' | datC20$levels=='OKCOKC405' | datC20$levels=='OKCSAL785' | datC20$levels=='OMAAMS515' |
                datC20$levels=='PHIMER609' | datC20$levels=='PHISAL856' | datC20$levels=='SANAUS512' | datC20$levels=='SFROAK510' | datC20$levels=='SHEHAR540'),"Group16",
                ifelse((datC20$levels=='AIRGRE864' | datC20$levels=='ATLATH706' | datC20$levels=='ATLMAC912' | datC20$levels=='ATLMEM901' | datC20$levels=='BOSHYA508' |
                datC20$levels=='FLNCLR813' | datC20$levels=='FLNINV352' | datC20$levels=='FLNOCA352' | datC20$levels=='HARHAR860' | datC20$levels=='LAXCDG310' |
                datC20$levels=='LAXONT909' | datC20$levels=='LAXPSG760' | datC20$levels=='LOULOU502' | datC20$levels=='MIAFTM941' | datC20$levels=='MILMAD608' |
                datC20$levels=='MINSTP612' | datC20$levels=='NCRCRY919' | datC20$levels=='NCRGRE336' | datC20$levels=='NCRMID704' | datC20$levels=='NEVNAT619' |
                datC20$levels=='NEVSDG619' | datC20$levels=='NNYBUF716' | datC20$levels=='NYCBRO917' | datC20$levels=='NYCPLA908' | datC20$levels=='OKCFAY501' |
                datC20$levels=='PHIWIL302' | datC20$levels=='SEWYAK509' | datC20$levels=='SFRCBL408' | datC20$levels=='VAHROA540'),"Group17",ifelse((datC20$levels=='AIRHIC828' |
                datC20$levels=='APCSIL301' | datC20$levels=='ATLCHA423' | datC20$levels=='BOSBOS508' | datC20$levels=='CHIBLO309' | datC20$levels=='DALFTW817' |
                datC20$levels=='DETJAC517' | datC20$levels=='DETPON248' | datC20$levels=='DETROS810' | datC20$levels=='FLNBRD941' | datC20$levels=='FLNORL407' |
                datC20$levels=='FLNWNP407' | datC20$levels=='MIAFTL954' | datC20$levels=='MIANDA305' | datC20$levels=='NYCJER201' | datC20$levels=='NYCMAN917' |
                datC20$levels=='NYCMTK914' | datC20$levels=='NYCNEW908' | datC20$levels=='NYCQUE917' | datC20$levels=='NYCSUF516' | datC20$levels=='OHIBER440' |
                datC20$levels=='OHICIN513' | datC20$levels=='OHILAN740' | datC20$levels=='OHILRN440' | datC20$levels=='OHIWOO330' | datC20$levels=='OKCARD580' |
                datC20$levels=='PHIDOV302' | datC20$levels=='PITHOM412' | datC20$levels=='SANKIL254' | datC20$levels=='SEAEVE425' | datC20$levels=='SFRPAL650' |
                datC20$levels=='SFRSFR415' | datC20$levels=='SHEHAG301'),"Group18",ifelse((datC20$levels=='AIRCOL803' | datC20$levels=='AIRKIN252' | datC20$levels=='AIRSPA864'|
                datC20$levels=='APCLEE703' | datC20$levels=='ATLBRU912' | datC20$levels=='ATLDAL334' | datC20$levels=='AWIGRE920' | datC20$levels=='BIRBIR205' |
                datC20$levels=='BOSPRO401' | datC20$levels=='CHICHI773' | datC20$levels=='CHINBK847' | datC20$levels=='CHIRCK815' | datC20$levels=='DALDAL214' |
                datC20$levels=='DALMNW940' | datC20$levels=='DENGLD303' | datC20$levels=='DETADR517' | datC20$levels=='DETWAS419' | datC20$levels=='FLNLKW863' |
                datC20$levels=='KCYELD316' | datC20$levels=='LAUTUP662' | datC20$levels=='LAXBEV310' | datC20$levels=='LAXING310' | datC20$levels=='LAXVNY818' |
                datC20$levels=='LAXWES310' | datC20$levels=='NCRCHA704' | datC20$levels=='NCRHGP336' | datC20$levels=='NCRRAL919' | datC20$levels=='NEVELC619' |
                datC20$levels=='NMXABI915' | datC20$levels=='NNYBUR914' | datC20$levels=='NNYSYR315' | datC20$levels=='OHHCHA304' | datC20$levels=='OHICOV606' |
                datC20$levels=='OHIDAY937' | datC20$levels=='OHIMID513' | datC20$levels=='OMADES515' | datC20$levels=='PHIPHI215' | datC20$levels=='PHIWLW609' |
                datC20$levels=='PHXSCO480' | datC20$levels=='SANFRE830' | datC20$levels=='SANLAM512' | datC20$levels=='SEALVW360' | datC20$levels=='SEAOKH360' |
                datC20$levels=='SEASEA206' | datC20$levels=='SEATAC253' | datC20$levels=='SFRSRO707' | datC20$levels=='SHECHA717' | datC20$levels=='SHEWIN540' |
                datC20$levels=='STLOZA573'),"Group19",ifelse((datC20$levels=='AIRASH828' | datC20$levels=='APCFRE540' | datC20$levels=='BOSBOS617' | datC20$levels=='BOSBOS781' |
                datC20$levels=='BOSFRA508' | datC20$levels=='BOSWOR508' | datC20$levels=='CHILAG708' | datC20$levels=='FLNTAL850' | datC20$levels=='IPMGDR616' |
                datC20$levels=='LAXBUR818' | datC20$levels=='LAXLAX213' | datC20$levels=='LAXSAN714' | datC20$levels=='MIAWPB561' | datC20$levels=='NCRVIR757' |
                datC20$levels=='NEVLVS702' | datC20$levels=='NNYALB518' | datC20$levels=='NYCNAS516' | datC20$levels=='OKCLRK501' | datC20$levels=='PHXGLE623' |
                datC20$levels=='PITCOR412' | datC20$levels=='SANGEO512' | datC20$levels=='SEAOLY360' | datC20$levels=='SFRDAN925' | datC20$levels=='SFROAK925' |
                datC20$levels=='SFRSAC916' | datC20$levels=='STLCMB573' | datC20$levels=='VAHCHL804'),"Group20",ifelse((datC20$levels=='AIRMYR843' | datC20$levels=='APCANN443' |
                datC20$levels=='APCFRD301' | datC20$levels=='APCSVP443' | datC20$levels=='ATHKIN423' | datC20$levels=='CHILAG630' | datC20$levels=='DALDTN940' |
                datC20$levels=='FLNWNH941' | datC20$levels=='HOUBRN409' | datC20$levels=='INDAND765' | datC20$levels=='KCYLAW913' | datC20$levels=='LAUJAC601' |
                datC20$levels=='LAXALB626' | datC20$levels=='LAXSNP310' | datC20$levels=='MIADEL561' | datC20$levels=='NCRPOR757' | datC20$levels=='NEVLMS619' |
                datC20$levels=='NVUREN775' | datC20$levels=='NYCNEW201' | datC20$levels=='NYCNEW732' | datC20$levels=='OKCCON501' |datC20$levels=='OMAOMA402' | 
                datC20$levels=='PHITRT609' | datC20$levels=='PHXPHX602' | datC20$levels=='PITBUT412' | datC20$levels=='PITGRE412' |datC20$levels=='SEABEA503' |
                datC20$levels=='SFRSMO650' | datC20$levels=='STLJOP417'),"Group21",ifelse((datC20$levels=='CHICHI312' | datC20$levels=='DETFER248' | datC20$levels=='DETKAL616' |
                datC20$levels=='FLNJAC904' | datC20$levels=='FLNOGC904' | datC20$levels=='FLNTAM813' | datC20$levels=='HOUGLV409' | datC20$levels=='LAXALA562' |
                datC20$levels=='LAXCUL310' | datC20$levels=='LAXSBN909' | datC20$levels=='LOUFRK502' | datC20$levels=='NEVOCN760' | datC20$levels=='NMXALB505' |
                datC20$levels=='NYCNEW973' | datC20$levels=='OHIPSV440' | datC20$levels=='OMAIWC319' | datC20$levels=='PHXTUC520' | datC20$levels=='SEABLV425' |
                datC20$levels=='SEASPO509'),"Group22",ifelse((datC20$levels=='AIRAUG706' | datC20$levels=='AIRGOL919' | datC20$levels=='AIRGRN252' | datC20$levels=='AIRGWD864' |
                datC20$levels=='AIRJAC910' | datC20$levels=='AIRNWB252' | datC20$levels=='ATLALB912' | datC20$levels=='BOSMAN603' | datC20$levels=='CHIDAV319' |
                datC20$levels=='DENCOL719' | datC20$levels=='DETBNH616' | datC20$levels=='HOUCON409' | datC20$levels=='IPMSAG517' | datC20$levels=='LAXOAK805' |
                datC20$levels=='MIAMIA305' | datC20$levels=='MIAPOR941' | datC20$levels=='NCRFAY910' | datC20$levels=='NEVENC760' | datC20$levels=='NOROWT507' |
                datC20$levels=='NYCWHI914' | datC20$levels=='OHISGF937' | datC20$levels=='OKCCHC405' | datC20$levels=='OMALNC402' | datC20$levels=='PHIMUL609' |
                datC20$levels=='SDAWTR605' | datC20$levels=='SEAMTV360' | datC20$levels=='SFRSCL408' | datC20$levels=='SFRWLC925' | datC20$levels=='STLCHE636' |
                datC20$levels=='VAHRAD540'),"Group23",ifelse((datC20$levels=='AIRAND864' | datC20$levels=='DALDEN903' | datC20$levels=='DENDEN303' | datC20$levels=='DETBAT616' |
                datC20$levels=='HARBRI203' | datC20$levels=='HARNOR203' | datC20$levels=='HWIHON808' | datC20$levels=='LAXCOV626' | datC20$levels=='NCRCHE757' |
                datC20$levels=='NEVCOR619' | datC20$levels=='OHIHAR330' | datC20$levels=='PHIARD610' | datC20$levels=='SEACOR541' | datC20$levels=='SEASIL360' |
                datC20$levels=='SEWGTP541'),"Group24",ifelse((datC20$levels=='ATLROS678' | datC20$levels=='DETLAN517' | datC20$levels=='FLNSAG904' | datC20$levels=='HOUSPR832' |
                datC20$levels=='INHSBN219' | datC20$levels=='LAXLAN661' | datC20$levels=='MIADFD954' | datC20$levels=='MIANAP941' | datC20$levels=='MIASUG305' |
                datC20$levels=='NMXSAN915' | datC20$levels=='NSHCOL615' | datC20$levels=='OHHPOR740' | datC20$levels=='OKCSTW405' | datC20$levels=='OKCWIC940' |
                datC20$levels=='SFRHAY510'),"Group25",ifelse((datC20$levels=='DETWYN734' | datC20$levels=='KCYHUT316' | datC20$levels=='NMXDEL830' | datC20$levels=='NMXSAN505' |
                datC20$levels=='NORROC507' | datC20$levels=='NORSTC320' | datC20$levels=='OHILAW812' | datC20$levels=='SEAPOR503'),"Group26",ifelse((datC20$levels=='AIRMAR828' |
                datC20$levels=='AIRSUM803' | datC20$levels=='ATLSWT423' | datC20$levels=='CHIJOL815' | datC20$levels=='DALCOM903' | datC20$levels=='DALGVL940' |
                datC20$levels=='DENVAL970' | datC20$levels=='FLNLKP863' | datC20$levels=='IPMMID517' | datC20$levels=='LAUGNW662' | datC20$levels=='LAXCOR909' |
                datC20$levels=='MIABON941' | datC20$levels=='MIAMAR305' | datC20$levels=='MIAVER561' | datC20$levels=='NCRPIT919' | datC20$levels=='NCRYOR803' |
                datC20$levels=='NEVLAU702' | datC20$levels=='NMXLSA505' | datC20$levels=='NORFRM218' | datC20$levels=='NORRDW651' | datC20$levels=='OHHFAI304' |
                datC20$levels=='OHIKEN330' | datC20$levels=='OHIMAN419' | datC20$levels=='OHIOBE440' | datC20$levels=='OMANEW515' | datC20$levels=='PHIAVD610' |
                datC20$levels=='PHIELK443' | datC20$levels=='PHIMIL302' | datC20$levels=='PHXCGR520' | datC20$levels=='PITCAR412' | datC20$levels=='SEACHE360' |
                datC20$levels=='SFUSAC530' | datC20$levels=='SLCPRO801' | datC20$levels=='VAHDAN804' | datC20$levels=='VAHMTN540'),"Group27",ifelse((datC20$levels=='BOSPTL207' |
                datC20$levels=='CHIROC309' | datC20$levels=='MIAPSL561' | datC20$levels=='SLCSLC801'),"Group28",ifelse((datC20$levels=='FLNNPR813' | datC20$levels=='HARWAT203' |
                datC20$levels=='LAXSFN818' | datC20$levels=='PHICTR610' | datC20$levels=='SEWMED541'),"Group29",ifelse((datC20$levels=='BOSNSH603' | datC20$levels=='DENBOU303' |
                datC20$levels=='DENGRE970' | datC20$levels=='FLNGAN352' | datC20$levels=='NMXYUM520' | datC20$levels=='OHIXEN937' | datC20$levels=='SEAEUG541'),"Group30",
                ifelse((datC20$levels=='INHFTW219' | datC20$levels=='OMACDR319' | datC20$levels=='SANSMC512'),"Group31",ifelse((datC20$levels=='DALATH903' | 
                datC20$levels=='MILRAC414' | datC20$levels=='NORMAN507' | datC20$levels=='NYCCIT914' | datC20$levels=='OHIELY440'),"Group32",ifelse((datC20$levels=='HWIMAU808' |
                datC20$levels=='SANTEM254'),"Group33",ifelse((datC20$levels=='AIRELI252' | datC20$levels=='CHICPT219' | datC20$levels=='DALKAU469' | datC20$levels=='DETFRE419' |
                datC20$levels=='LAXCAN661' | datC20$levels=='LAXOXN805' | datC20$levels=='NNYUTI315' | datC20$levels=='OHINCA937' | datC20$levels=='OKCEND580' |
                datC20$levels=='SEWKEN509'),"Group34",ifelse((datC20$levels=='DENFTC970' | datC20$levels=='HOUFRE409' | datC20$levels=='KCYLEA913' | datC20$levels=='MIAJUP561' |
                datC20$levels=='MILJAN608' | datC20$levels=='NMCGDJ970' | datC20$levels=='PITMNG412' | datC20$levels=='SDASFL605' | datC20$levels=='SEASAL503' |
                datC20$levels=='SLCKAY801'),"Group35",ifelse((datC20$levels=='AIRFLO843' | datC20$levels=='PITWAS412'),"Group36",ifelse((datC20$levels=='NNYPOU914' |
                datC20$levels=='OKCMAN785' | datC20$levels=='PITROC412'),"Group37",ifelse((datC20$levels=='AIRGAF864' | datC20$levels=='AIRHHI843' | datC20$levels=='APCWAL240' |
                datC20$levels=='ATLATN423' | datC20$levels=='ATLVAL229' | datC20$levels=='CHIWAT319' | datC20$levels=='FLNBSH352' | datC20$levels=='FLNCRY352' |
                datC20$levels=='FLNSMY904' | datC20$levels=='FLNSTK904' | datC20$levels=='HOUHUN936' | datC20$levels=='KCYCLI660' | datC20$levels=='NMXFLA520' |
                datC20$levels=='NORFAR701' | datC20$levels=='OHHASH606' | datC20$levels=='OHIASH419' | datC20$levels=='OHICIR740' | datC20$levels=='OHILEB513' |
                datC20$levels=='OKCCAB501' | datC20$levels=='OKCEMP316' | datC20$levels=='SANREF361' | datC20$levels=='SEABLG360' | datC20$levels=='SEWKHF541' |
                datC20$levels=='SEWPAS509' | datC20$levels=='SEWPRO509' | datC20$levels=='SLCPRK435' | datC20$levels=='STLROL573'),"Group38",ifelse((datC20$levels=='DETBWG419' |
                datC20$levels=='SANCOC254'),"Group39",ifelse((datC20$levels=='ATLOPE334' | datC20$levels=='HOULJK409' | datC20$levels=='LAXVIC760'),"Group40",
                ifelse((datC20$levels=='AIRAIK803' | datC20$levels=='MIAKEY305' | datC20$levels=='OKCJUN785'),"Group41",ifelse((datC20$levels=='LAUCLM662' | 
                datC20$levels=='OHHHUN304' | datC20$levels=='OHIOXF513' | datC20$levels=='VAHLYN804'),"Group42",ifelse((datC20$levels=='ATLTUN601' | datC20$levels=='DENDIL970' |
                datC20$levels=='LAXCAS661' | datC20$levels=='PHILAN717' | datC20$levels=='PITFOR412' | datC20$levels=='PITUNT412' | datC20$levels=='VAHWAY540'),"Group43",
                ifelse((datC20$levels=='AIRGEO843' | datC20$levels=='APCSOL443' | datC20$levels=='ATLBLY870' | datC20$levels=='CHIMCH815' | datC20$levels=='FLNKEH352' |
                datC20$levels=='INDCLO765' | datC20$levels=='INDMAR765' | datC20$levels=='INHBLU419' | datC20$levels=='LAXVEN805' | datC20$levels=='NCRSIC919' |
                datC20$levels=='NCRTHO336' | datC20$levels=='OHIERI814' | datC20$levels=='OHINOR419' | datC20$levels=='OHISAL330' | datC20$levels=='SEWCLE509' |
                datC20$levels=='SEWHER541' | datC20$levels=='SFRPLA530' | datC20$levels=='STLMIA918' | datC20$levels=='STLMIA918' | datC20$levels=='VAHLEX540'),"Group44",datC20$levels))))))))))))))))))))))))))))))))))))))))))))

# Above grouping done by rounding off churn percentages to 2 decimal places & then created groups for similar churn percentages.

#### Data Preparation : Missing Value Imputation ####

# Set Cut-off percentage : 15% i.e. excluded the variables having more than 15% missing
# percentage.But important variables will not be excluded from analysis even if they have
# more than cutoff percentage.

# Checking variables containing NA's
summary(telecom)

# Missing Values Imputation of Continuous Variables

# Below 3 numeric variables have more than 15% missing percentage.

#variables     missing percentage  
#1.retdays     96.75               
#2.numbcars    48.99              
#3.income      24.93               

# Can't exclude income,retdays variables from analysis because they are an important variable.

#(1.) avg6mou 

telecom$avg6mou[is.na(telecom$avg6mou)]<-median(telecom$avg6mou,na.rm = TRUE)

#(2.) avg6qty

telecom$avg6qty[is.na(telecom$avg6qty)]<-median(telecom$avg6qty,na.rm = TRUE)

#(3.) age1

telecom$age1[is.na(telecom$age1)]<-median(telecom$age1,na.rm = TRUE)

#(4.) age2

telecom$age2[is.na(telecom$age2)]<-median(telecom$age2,na.rm = TRUE)

#(5.) forgntvl - Categorical Variable 

telecom$forgntvl<-ifelse((is.na(telecom$forgntvl)),"Missing",telecom$forgntvl)

#(6.) mtrcycle - Categorical Variable

telecom$mtrcycle<-ifelse((is.na(telecom$mtrcycle)),"Missing",telecom$mtrcycle)

#(7.) truck - Categorical Variable

telecom$truck<-ifelse((is.na(telecom$truck)),"Missing",telecom$truck)

#(8.) hnd_price

telecom$hnd_price[is.na(telecom$hnd_price)]<-mean(telecom$hnd_price,na.rm = TRUE)

#(9.) change_mou

telecom$change_mou[is.na(telecom$change_mou)]<-mean(telecom$change_mou,na.rm = TRUE)

#(10.) mou_Mean

telecom$mou_Mean[is.na(telecom$mou_Mean)]<-mean(telecom$mou_Mean,na.rm = TRUE)

#(11.) totmrc_Mean

telecom$totmrc_Mean[is.na(telecom$totmrc_Mean)]<-mean(telecom$totmrc_Mean,na.rm = TRUE)

#(12.) rev_Range

telecom$rev_Range[is.na(telecom$rev_Range)]<-mean(telecom$rev_Range,na.rm = TRUE)

#(13.) mou_Range

telecom$mou_Range[is.na(telecom$mou_Range)]<-median(telecom$mou_Range,na.rm = TRUE)

#(14.) ovrrev_Mean

telecom$ovrrev_Mean[is.na(telecom$ovrrev_Mean)]<-mean(telecom$ovrrev_Mean,na.rm = TRUE)

#(15.) rev_Mean

telecom$rev_Mean[is.na(telecom$rev_Mean)]<-mean(telecom$rev_Mean,na.rm = TRUE)

#(16.) ovrmou_Mean

telecom$ovrmou_Mean[is.na(telecom$ovrmou_Mean)]<-mean(telecom$ovrmou_Mean,na.rm = TRUE)

#(17.) roam_Mean

telecom$roam_Mean[is.na(telecom$roam_Mean)]<-mean(telecom$roam_Mean,na.rm = TRUE)

#(18.) da_Mean

telecom$da_Mean[is.na(telecom$da_Mean)]<-mean(telecom$da_Mean,na.rm = TRUE)

#(19.) da_Range

telecom$da_Range[is.na(telecom$da_Range)]<-mean(telecom$da_Range,na.rm = TRUE)

#(20.) datovr_Mean

telecom$datovr_Mean[is.na(telecom$datovr_Mean)]<-mean(telecom$datovr_Mean,na.rm = TRUE)

#(21.) datovr_Range

telecom$datovr_Range[is.na(telecom$datovr_Range)]<-mean(telecom$datovr_Range,na.rm = TRUE)

#(22.) eqpdays

# Only 1 missing value,so removing it.
index1<-which(is.na(telecom$eqpdays))
telecom<-telecom[-index1,]

#(23.) models
# On removing missing value in eqpdays,missing value in models variable got automatically removed.

#(24.) income

telecom$income[is.na(telecom$income)]<-median(telecom$income,na.rm = TRUE)

#(25.) retdays

telecom$retdays[is.na(telecom$retdays)]<-median(telecom$retdays,na.rm = TRUE)


# Missing Value Treatment for Categorical Variables #

# Below 11 variables have more than 15% missing percentage.

#variables   missing percentage  
#1.solflag   98.02               
#2.wrkwoman  87.41              
#3.div_type  81.06               
#4.occu1     73.14               
#5.proptype  71.45               
#6.cartype   67.88              
#7.children  65.79               
#8.mailordr  63.72               
#9.mailresp  62.23               
#10.dwllsize 37.70               
#11.dwlltype 31.41              

# Can't exclude these variables - wrkwoman,div_type,occu1,children,dwllsize,dwlltype,
# mailordr,mailresp variables because these variables can be important for analysis.

# Creating Missing category for missing values in categorical variables

#(1.) wrkwomen

telecom$wrkwoman<-ifelse((is.na(telecom$wrkwoman)),"Missing",telecom$wrkwoman)

#(2.) div_type

telecom$div_type<-ifelse((is.na(telecom$div_type)),"Missing",telecom$div_type)

#(3.) occu1

telecom$occu1<-ifelse((is.na(telecom$occu1)),"Missing",telecom$occu1)

#(4.) children

telecom$children<-ifelse((is.na(telecom$children)),"Missing",telecom$children)

#(5.) dwllsize

telecom$dwllsize<-ifelse((is.na(telecom$dwllsize)),"Missing",telecom$dwllsize)

#(6.) dwlltype

telecom$dwlltype<-ifelse((is.na(telecom$dwlltype)),"Missing",telecom$dwlltype)

#(7.) hnd_webcap

telecom$hnd_webcap<-ifelse((is.na(telecom$hnd_webcap)),"Missing",telecom$hnd_webcap)

#(8.) prizm_social_one

telecom$prizm_social_one<-ifelse((is.na(telecom$prizm_social_one)),"Missing",telecom$prizm_social_one)

#(9.) marital

telecom$marital<-ifelse((is.na(telecom$marital)),"Missing",telecom$marital)

#(10.) ethnic

telecom$ethnic<-ifelse((is.na(telecom$ethnic)),"Missing",telecom$ethnic)

#(11.) car_buy

telecom$car_buy<-ifelse((is.na(telecom$car_buy)),"Missing",telecom$car_buy)

#(12.) area

telecom$area<-ifelse((is.na(telecom$area)),"Missing",telecom$area)

#(13.) csa

telecom$csa<-ifelse((is.na(telecom$csa)),"Missing",telecom$csa)

#(14.) refurb_new

# On removing missing value in eqpdays,missing value in refurb_new got automatically removed.

#(15.) mailordr

telecom$mailordr<-ifelse((is.na(telecom$mailordr)),"Missing",telecom$mailordr)

#(16.) mailresp 

telecom$mailresp<-ifelse((is.na(telecom$mailresp)),"Missing",telecom$mailresp)

# Excluding variables having high missing percentage and not important to consider for analysis.
telecom1<- select(telecom,-numbcars,-solflag,-cartype,-proptype)

#### Data Preparation : Derived Variables ####

# Total Completed Calls = Sum of completed data calls and completed voice calls
total_completed_calls = telecom1$comp_dat_Mean+telecom1$comp_vce_Mean

# (1.) Creating new column "totalCompletedCalls"
telecom1<-mutate(telecom1,totalCompletedCalls=total_completed_calls)

# Total Dropped Calls = Sum of dropped data calls and voice calls
total_dropped_calls=telecom1$drop_dat_Mean+telecom1$drop_vce_Mean

# (2.) Creating new column "totalDroppedCalls"
telecom1<-mutate(telecom1,totalDroppedCalls=total_dropped_calls)

## Splitting the data into test and training sets 
set.seed(1234)

#Random sampling without replacement
indexes<-sample(nrow(telecom1),0.70*nrow(telecom1)) 
train<-telecom1[indexes,]
test<-telecom1[-indexes,]

## Create dummies

library(dummies)

train=dummy.data.frame(train)
test=dummy.data.frame(test)

#### Model Building ####

#mod<-glm(churn~.,data = train,family = "binomial")

#step(mod,direction = "both")

## Stepwise regression was taking more than 1/2 hour for each iteration.So,avoided doing it.
## Also,causing memory allocation error

mod1<-glm(churn~mou_Mean+avg3mou+actvsubs+forgntvl0+forgntvl1+opk_dat_Mean+mtrcycle0+truck0+
          roam_Mean+recv_sms_Mean+blck_dat_Mean+mou_pead_Mean+datovr_Mean+datovr_Range+
          drop_dat_Mean+asl_flagN+refurb_newN+hnd_webcapUNKW+hnd_webcapWC+hnd_webcapWCMB+
          dwlltypeM+dwlltypeS+mailordrB+wrkwomanY+mailrespR+car_buyNew+childrenN+childrenY+
          div_typeBTH+div_typeLDD+div_typeLTD+totmrc_Mean+rev_Range+mou_Range+change_mou+
          drop_vce_Range+months+totcalls+income+eqpdays+custcare_Mean+ccrndmou_Range+ovrrev_Mean+
          rev_Mean+ovrmou_Mean+avgmou+avg6mou+age1+age2+uniqsubs+retdays+drop_vce_Mean+adjmou+
          totrev+adjrev+avgrev+prizm_social_oneC+prizm_social_oneR+prizm_social_oneS+prizm_social_oneT+
          prizm_social_oneU+dwllsizeA+dwllsizeB+dwllsizeC+dwllsizeD+dwllsizeE+dwllsizeF+dwllsizeG+
          dwllsizeH+dwllsizeI+dwllsizeJ+dwllsizeK+dwllsizeL+dwllsizeM+dwllsizeN+dwllsizeO,data=train,family="binomial")
  
summary(mod1)

mod2<-glm(churn~mou_Mean+actvsubs+mou_pead_Mean+asl_flagN+refurb_newN+hnd_webcapWCMB+
          dwlltypeM+mailrespR+childrenN+childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+
          mou_Range+months+totcalls+income+eqpdays+ovrrev_Mean+rev_Mean+avgmou+age1+
          uniqsubs+retdays+drop_vce_Mean+adjmou+totrev+prizm_social_oneS+dwllsizeE+
          dwllsizeH+dwllsizeI,data=train,family="binomial")

summary(mod2)

mod3<-glm(churn~mou_Mean+actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+
          dwlltypeM+mailrespR+childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+
          mou_Range+months+eqpdays+ovrrev_Mean+rev_Mean+avgmou+age1+
          uniqsubs+drop_vce_Mean+adjmou+totrev+prizm_social_oneS+dwllsizeE+
          dwllsizeH,data=train,family="binomial")

summary(mod3)

mod4<-glm(churn~mou_Mean+actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+
          dwlltypeM+mailrespR+childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+
          mou_Range+months+eqpdays+ovrrev_Mean+rev_Mean+avgmou+age1+
          uniqsubs+drop_vce_Mean+adjmou+totrev+prizm_social_oneS+dwllsizeE,data=train,family="binomial")

summary(mod4)

# Applying transformation to variables that are important to keep in model.

# (1.) income
telecom1<-mutate(telecom1,income1=sqrt(income))

# (2.) retdays
telecom1<-mutate(telecom1,retdays1=sqrt(retdays))

# Excluding original variable - income,retdays
telecom1<- select(telecom1,-income,-retdays)

# Again,splitting the data into test and training sets 
set.seed(1234)

# Random sampling without replacement
indexes<-sample(nrow(telecom1),0.70*nrow(telecom1)) 
train<-telecom1[indexes,]
test<-telecom1[-indexes,]

# Again,creating dummies

train=dummy.data.frame(train)
test=dummy.data.frame(test)

mod5<-glm(churn~mou_Mean+actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+
          dwlltypeM+mailrespR+childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+
          mou_Range+months+eqpdays+ovrrev_Mean+rev_Mean+avgmou+age1+
          uniqsubs+drop_vce_Mean+adjmou+totrev+prizm_social_oneS+dwllsizeE+
          income1+retdays1,data=train,family="binomial")

summary(mod5)

## Tried to apply different transformations on income variable to keep it in model but it didn't come out to
## be significant.So,excluded it.

mod6<-glm(churn~mou_Mean+actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+dwlltypeM+mailrespR+retdays1+
          childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+mou_Range+months+eqpdays+ovrrev_Mean+rev_Mean+
          avgmou+age1+uniqsubs+adjmou+totrev+prizm_social_oneS+dwllsizeE+totalCompletedCalls+totalDroppedCalls+
          occu1A+occu1B+occu1C+occu1D+occu1E+occu1F+occu1G+occu1H+occu1I+occu1J+occu1K+occu1Z,data=train,family="binomial")

summary(mod6)

mod7<-glm(churn~mou_Mean+actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+dwlltypeM+mailrespR+retdays1+
          childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+mou_Range+months+eqpdays+ovrrev_Mean+rev_Mean+
          avgmou+age1+uniqsubs+adjmou+totrev+prizm_social_oneS+dwllsizeE+totalCompletedCalls+totalDroppedCalls+
          occu1E,data=train,family="binomial")

summary(mod7)

#All variables in mod7 model turns out to be significant.

### Multicollinearity check
library(car)
vif(mod7)

##Ideally,vif should be less than 5.
##Variables having more than 5 vif shows multicollinearity & should be removed from the model.

#Removing variables having vif>5
mod8<-glm(churn~actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+dwlltypeM+mailrespR+retdays1+
           childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+mou_Range+months+eqpdays+age1+
           uniqsubs+totrev+prizm_social_oneS+dwllsizeE+totalCompletedCalls+totalDroppedCalls+
           occu1E,data=train,family="binomial")

summary(mod8)

# Removing insignificant variables
mod9<-glm(churn~actvsubs+asl_flagN+refurb_newN+hnd_webcapWCMB+dwlltypeM+mailrespR+retdays1+
            childrenY+div_typeLDD+div_typeLTD+totmrc_Mean+mou_Range+months+eqpdays+age1+
            uniqsubs+totrev+prizm_social_oneS+dwllsizeE+totalCompletedCalls+totalDroppedCalls,data=train,family="binomial")

summary(mod9)

# Again,doing Multicollinearity check
vif(mod9)

## All variables are significant & have vif < 5.

# Obtaining predictions
prob=predict(mod9,newdata=test,type="response")
head(prob)

# Plotting ROC curve
library(ROCR)
pred<-prediction(prob,test$churn)
roc=performance(pred,"tpr","fpr")
par(mar=c(1,1,1,1))
plot(roc)
abline(0,1)

# Checking Area Under the Curve for good fit 
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#For this data,auc of 0.62-0.63 is attainable.
#mod11 model turns out auc of 0.6078407 which shows it is not a good fit


# Again,creating model to achieve good fit value
modFinal<-glm(churn~actvsubs+totcalls+hnd_price+retdays1+avg3mou+change_mou+rev_Range+
              totmrc_Mean+mou_Range+months+eqpdays+age1+uniqsubs+totrev+totalCompletedCalls+
              totalDroppedCalls+asl_flagN+refurb_newN+mailrespR+childrenY+div_typeLDD+
              ethnicB+ethnicD+ethnicF+ethnicG+ethnicH+ethnicI+ethnicJ+ethnicO+ethnicN+
              ethnicR+ethnicS+ethnicU+prizm_social_oneR+prizm_social_oneT+
              mtrcycle0+dwlltypeM+datovr_Range+mou_pead_Mean,data=train,family="binomial")

summary(modFinal)


# Again, doing Multicollinearity check
vif(modFinal)

## All variables are significant and having vif<5 in modFinal model.

# Obtaining predictions 
prob=predict(modFinal,newdata=test,type="response")
pred<-prediction(prob,test$churn)

# Plotting ROC curve
roc=performance(pred,"tpr","fpr")
par(mar=c(1,1,1,1))
plot(roc)
abline(0,1)

# Checking Area Under the Curve for good fit 
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc

##auc value comes out to be 0.6176613 ~ 0.62.
##auc value of 0.62 shows that modFinal model is a good fit.
##So,selecting it as final model

# Looking at churn rate in the total data
table(telecom1$churn)/nrow(telecom1)

## proportion of churning in total data is 0.239215.
## Set up a cutoff of 0.239215.
## All the probabilities that are greater than or equal to 0.239215 are classified as customers who are likely to churn.
## All the probabilities that are less than or equal to 0.239215 are classified as customers who are not likely to churn.

# Computing Confusion Matrix
pred<-ifelse(prob>=0.239215,1,0)
pred<-as.factor(pred)
test$churn<-as.factor(test$churn)

library(caret)

confusionMatrix(pred,test$churn,positive="1")

## model is predicting 8623 correct non-events & 6422 incorrect non-events
## model is predicting 2920 correct events & 1924 incorrect events

##### TOP LINE QUESTIONS OF INTEREST TO SENIOR MANAGEMENT #####

#Ques: What would be your recommendation on how to use this churn model for prioritisation of customers for a
#proactive retention campaigns in the future?

#Answer: 

#To answer this question we can build a Gains Chart to target customers for proactive retention campaigns
library(gains)

test$churn<-as.numeric(test$churn)

# Gains chart
gains(test$churn,predict(modFinal,newdata=test,type="response"),groups=10)

#Here,with budgets constraint of contact list of 20% of the subscriber pool,
#the model accurately provides 21.8% of total customers who could churn.

#So,the company selects 20% of the subscribers which covers 21.8% of the customers who are likely to churn.

# Targeting top 20% of the customer pool
test$prob<-predict(modFinal,newdata=test,type="response")
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
targeted<-test[test$prob>0.2979541 & test$prob<=0.7946890,"Customer_ID"]
targeted<-as.data.frame(targeted)

# Exporting targeted customers ids who are likely to churn
write.csv(targeted,"TargetedCustomers.csv",row.names = F)

#### Creating customer segments ####

#Ques : What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
#concern and therefore, Mobicom would like to save their high revenue customers besides managing
#churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
#prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
#is the primary objective and revenue saves is the secondary objective.

#Answer :

pred1<-predict(modFinal,type="response",newdata = test)
test$prob1<-predict(modFinal,type="response",newdata = test)

# Grouping probability of churn (scores)
quantile(test$prob1,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
score<-ifelse(pred1<=0.20,"Low_Score",ifelse((pred1>=0.30 & pred1<=0.50),"Medium_Score","High_Score"))

# Grouping revenue
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenue<-ifelse(test$totrev<=455.462,"Low_Revenue",ifelse((test$totrev>= 557.910 & test$totrev<=795.970),"Medium_Revenue","High_Revenue"))

# Cross tabulation of probability of churn(score) and revenue
table(score,revenue)

test$score<-ifelse(pred1<0.20,"Low_Score",ifelse((pred1>=0.20 & pred1<=0.30),"Medium_Score","High_Score"))
test$revenue<-ifelse(test$totrev<455.462,"Low_Revenue",ifelse((test$totrev>=455.462 & test$totrev<=557.910),"Medium_Revenue","High_Revenue"))

targeted1<-test[test$score=="High_Score" & test$revenue=="High_Revenue","Customer_ID"]
targeted2<-test[test$score=="High_Score" & test$revenue=="Medium_Revenue","Customer_ID"]
targeted3<-test[test$score=="Medium_Score" & test$revenue=="High_Revenue","Customer_ID"]

targeted1<-as.data.frame(targeted1)
targeted2<-as.data.frame(targeted2)
targeted3<-as.data.frame(targeted3)

# Exporting targeted customer segments ids 
write.csv(targeted1,"High_Revenue_HighScoreTargeted.csv",row.names = F)
write.csv(targeted2,"High_Score_MediumRevenueTargeted.csv",row.names = F)
write.csv(targeted3,"Medium_Score_HighRevenueTargeted.csv",row.names = F)

############################################# END OF CAPSTONE PROJECT ##########################################################