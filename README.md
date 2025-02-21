# GSS-2000-2022
Affirmative Action and Public Opinion-GSS
*RECODES 

set maxvar 20000
use "/Users/namratashokeen/Downloads/GSS_stata/gss7222_r4.dta", clear
drop if year<2000 // the sample between years 2000 to 2022
drop if age>65  //sample for working age 18 to 65 years


//recode missings to . excluding iap

recode affrmact sex degree educ marblk marwht helpblk workblks wlthblks intlblks wrkwayup racdif1 racdif2 racdif3 racdif4 marwht intlwhts workwhts wlthwhts race  polviews  class degree (.d .n .s = .) 

//This file includes recodes of all the variables used in the study to standardize the variable names and categories used in previous literature

//Part 1 recode Socio-economic/demographic variables

//class use the same varaible and categories

//Recode race to newrace based on census categories
//variables used to recode newrace- race, racecen1, racecen2, racecen3, hispanic

recode hispanic (1 = 0) (2/50 = 1), gen(hisp) //for hispanic and non-hispanic

gen aian = .
replace aian = 1 if racecen1 == 3 | racecen2 == 3 | racecen3 == 3  //recoding american indians and alaska native (aian)//

gen asian = .
replace asian = 1 if (racecen1 >= 4 & racecen1 <=10) | (racecen2 >= 4 & racecen2 <=10) | (racecen3 >= 4 & racecen3 <=10)  //asians 

gen black = .
replace black = 1 if racecen1 == 2 | racecen2 == 2 | racecen3 == 2  //blacks

gen nhpi = .
replace nhpi = 1 if (racecen1 >= 11 & racecen1 <=14) | (racecen2 >= 11 & racecen2 <=14) | (racecen3 >= 11 & racecen3 <=14) //Native Hawaiian or Pacific Islander (nhpi)//

gen white = .
replace white = 1 if racecen1 == 1 | racecen2 == 1 | racecen3 == 1 //whites

gen othrac = .
replace othrac = 1 if racecen1 == 15 | racecen2 == 15 | racecen3 == 15 //other races//

gen racecount = 0 if racecen1 < 16 & hisp == 0
replace racecount = (racecount + 1) if aian == 1
replace racecount = (racecount + 1) if asian == 1
replace racecount = (racecount + 1) if black == 1
replace racecount = (racecount + 1) if nhpi == 1
replace racecount = (racecount + 1) if white == 1
replace racecount = (racecount + 1) if othrac == 1

recode racecount (2/6 = 1) (1 = 0), gen(multrac)   //more than one race, multirace

gen newrace = .
replace newrace = 1 if hisp == 1											// Hispanic (any race)
replace newrace = 2 if aian == 1 & hisp == 0 & multrac == 0					// AI/AN (non-Hispanic)
replace newrace = 3 if asian == 1 & hisp == 0 & multrac == 0				// Asian (non-Hispanic)
replace newrace = 4 if black == 1 & hisp == 0 & multrac == 0				// Black (non-Hispanic)
replace newrace = 5 if nhpi == 1 & hisp == 0 & multrac == 0					// NHPI (non-Hispanic)
replace newrace = 6 if white == 1 & hisp == 0 & multrac == 0				// White (non-Hispanic)
replace newrace = 7 if hisp == 0 & multrac == 1								// 2+ races (non-Hispanic)
replace newrace = 8 if othrac == 1 & hisp == 0 & multrac == 0				// other (non-Hispanic)

label var newrace "race/ethnicity (OMB categories)"
label define NEWRACE 1 "Hispanic" 2 "AIAN" 3 "Asian" 4 "Black" 5 "NHPI" 6 "White" 7 "2+ races" 8 "other"
label values newrace NEWRACE

label var newrace "race/ethnicity (OMB categories)"
label values newrace NEWRACE
ta newrace, m


//recode religion
 gen religion=.
 
 replace religion = 0 if inlist(relig, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13) //RELIGIOUS AFFILIATION
 
 replace religion =1 if relig==4 //NO AFFILAITION, NONE
 
 label define RELIGION 0 "Religiously affiliated" 1" no religious affiliation"
 label values religion RELIGION
 ta religion, m
 
 
//recode degree variable

ta degree, m
gen degree_i = degree
label define DEGREE_I 0" less than high school" 1 "high school" 2 "associate/junior college" 3 "bachelors" 4 "graduate"
label values degree_i DEGREE_I
 ta degree_i, m
ta educ,m

//recode education

* Generate a new variable for recoded years of education
gen education = .

* Recode based on highest year of school completed (educ)
* For "less than high school" (educ <= 11)
replace education = 1 if educ <= 11

* For "high school graduate" (12th grade, educ == 12)
replace education = 2  if educ == 12

* For "associate/junior college" (1-2 years of college, educ == 13 or 14)
replace education = 3 if educ == 13 | educ == 14

* For "bachelor's degree" (1-4 years of college, educ == 15 to 18)
replace education = 4 if educ >= 15 & educ <= 18

* For "graduate" (post-bachelor's education, educ > 18)
replace education = 5 if educ > 18

//for missings
replace education=. if educ==.|educ==.i

label define edu_labels 1 "Less than High School" 2 "High School Graduate" 3 "Associate's Degree" 4 "Bachelor's Degree" 5 "Graduate or Higher"
label values education edu_labels

* Check the results by tabulating the new years_education variable
tab education,m



//recode polviews
 gen polviews_new=.
 
 replace polviews_new =1 if inlist(polviews,1,2,3) //LIBERAL
 
  replace polviews_new =2 if polviews==4. //MODERATE
  
  replace polviews_new=3 if inlist(polviews,5,6,7) //CONSERVATIVE
  
   label define POLVIEWS_NEW 1"liberal" 2 "moderate" 3"conservative"
   label values polviews_new  POLVIEWS_NEW
   ta polviews_new,m
   

//recode age

gen birth_year = year - age

gen generation = .

replace generation = 1 if birth_year >= 1928 & birth_year <= 1945 // Silent Generation
replace generation = 2 if birth_year >= 1946 & birth_year <= 1964 // Baby Boomers
replace generation = 3 if birth_year >= 1965 & birth_year <= 1980 // Generation X
replace generation = 4 if birth_year >= 1981 & birth_year <= 1996 // Millennials
replace generation = 5 if birth_year >= 1997 & birth_year <= 2012 // Generation Z

label define gen_lbl 1 "Silent" 2 "Boomers" 3 "Gen X" 4 "Millennials" 5 "Gen Z"
label values generation gen_lbl
tab generation


// RECODE INDEPENDENT VARIABLES 

//recode intlblks

gen intlblks_new = intlblks  //  new variable

//  Recode into categories
replace intlblks_new = 1 if inrange(intlblks, 1, 2)   // Recode low intelligence (1-unintelligent and 2)
replace intlblks_new = 2 if inrange(intlblks, 3, 4)   // Recode moderate intelligence (3 and 4)
replace intlblks_new = 3 if inrange(intlblks, 5, 7)   // Recode high intelligence (5, 6, and 7)

label define intlblks_new_lbl 1 "Low Intelligence" 2 "Moderate Intelligence" 3 "High Intelligence" //label
label values intlblks_new intlblks_new_lbl
// Check the distribution 
tab intlblks_new, m


//recode intlwhts, same as intlblks

gen intlwhts_new = intlwhts

replace intlwhts_new = 1 if inrange(intlwhts, 1, 2)   // Recode low intelligence (1-unintelligent and 2)
replace intlwhts_new = 2 if inrange(intlwhts, 3, 4)   // Recode moderate intelligence (3 and 4)
replace intlwhts_new = 3 if inrange(intlwhts, 5, 7)   // Recode high intelligence (5, 6, and 7)

label define intlwhts_new_lbl 1 "Low Intelligence" 2 "Moderate Intelligence" 3 "High Intelligence" //label
label values intlwhts_new intlwhts_new_lbl
ta intlwhts_new,m //check


//RECODE workblks 

gen workblks_new = workblks  // 'workblks' variable into a new categorized variable

// Recode into a new scale: 1 = hard-working, 2 = moderate, 3 = lazy
replace workblks_new = 1 if workblks == 1  // Hard-working
replace workblks_new = 2 if inlist(workblks, 2, 3, 4)  // Moderate
replace workblks_new = 3 if inlist(workblks, 5, 6, 7)      //lazy

label define workblks_lbl 1 "Hard-working" 2 "Moderate" 3 "Lazy" //Label  

label values workblks_new workblks_lbl
tab workblks_new, m  // Check the recoded variable
tab workblks, m  

 
//recode workwhts

 gen workwhts_new = workwhts 
 
 replace workwhts_new = 1 if workwhts == 1 // Hard-working
  replace workwhts_new = 2 if inlist(workwhts, 2, 3, 4) //moderate
   replace workwhts_new = 3 if inlist(workwhts, 5, 6, 7) //lazy
	
	label define workwhts_lbl 1 "Hard-working" 2 "Moderate" 3 "Lazy" //label
  label values workwhts_new workwhts_lbl
  tab workwhts_new, m 
  
  
//recode helpblk 

 label define helpblk_lbl 1 "Government should help" 2 "Somewhat agree" 3 "Agree with both" 4 "Disagree" 5 "No special treatment"
 label values helpblk helpblk_lbl
ta helpblk,m

—------------------------------------------------------------------------------------------------------------------------------

//SUBSET 1, SAMPLE ONLY FOR AFFRMACT, HANDLING MISSING VALUES

*RUN RECODES 

do "/Users/namratashokeen/Downloads/recodes_19feb2025.do"

//outcome variable: preferential hiring among blakcs

ta affrmact, m
 
//IMPUTING MISSING VALUES

 //imputing polviews_new
  ta partyid,m //have lesser missing than polviews
   drop if partyid==.d|partyid==.n //(83 observations deleted)//
   replace polviews_new=1 if missing(polviews_new)& inlist(partyid, 0,1,2)
   replace polviews_new=3 if missing(polviews_new)& inlist(partyid, 4,5,6)
   replace polviews_new = 2 if missing(polviews_new) & partyid == 3
 
 //impute degree
 ta degree_i,m
 ta degree_i education, m
 
 replace degree_i=0 if missing(degree_i) & education==1 //high school
  replace degree_i=1 if missing(degree_i) & education==2   //high school
 replace degree_i=2 if missing(degree_i) & education==3 //associate
 replace degree_i=3 if missing(degree_i) & education==4 //bachelors
 replace degree_i= 4 if missing(degree_i) & education==5 //gradaute
 
 

//impute religion
	 replace religion=0 if missing(religion) & inlist(relig16,1,2,3,5,6,7,8,9,10,11,12,13,14,15) //imputing similar values from relig16
//checked if other matches found for imputation
ta religion if missing(religion)& !missing(other)
ta religion if missing(religion)& !missing(jew)
 ta religion if missing(religion)& !missing(othjew)
 ta religion if missing(religion)& !missing(oth16)
  ta religion if missing(religion)& !missing(othjew16)
  ta religion if missing(religion)& !missing(denom)
    
///missing values in newrace
   ta newrace,m
    ta newrace racecen1,m //matching for imputing
   replace newrace = 6 if missing(newrace) & racecen1 == 1 & !missing(racecen1) //whites 
   replace newrace = 6 if missing(newrace) & racecen2 == 1 & !missing(racecen2) //whites
  replace newrace = 4 if missing(newrace) & racecen1 == 2 & !missing(racecen1)  //blacks
  replace newrace = 8 if missing(newrace) & racecen1 == 15 & !missing(racecen1)  //otherrace
   replace newrace = 2 if missing(newrace) & racecen1 == 3 & !missing(racecen1)  //AIAN
    replace newrace = 1 if missing(newrace) & racecen1 == 16 & !missing(racecen1)  //hisapnic
	 replace newrace=6 if missing(newrace) & race == 1 & !missing(race)  //white using race
	  replace newrace=4 if missing(newrace) & race == 2 & !missing(race)  //black using race
  
 
//excluding iap those who were not asked the questions, sample only for those who were asked

foreach var in  marblk marwht affrmact workblks_new workwhts_new intlblks_new intlwhts_new wrkwayup racdif1 racdif2 racdif3 racdif4 polviews_new class degree_i sex newrace religion {
    drop if `var' == .i
}

//generate dummy varaible for missing

gen missing_all1 = 0 // Create the variable and set all to 0 initially

// Replace missing_all to 1 if any of the variables have missing values
 replace missing_all1 = 1 if affrmact==.| sex==. | newrace==.| religion==.|polviews_new==.| class==.| racdif1==. | racdif2==. | racdif3 ==.|racdif4==. |workblks_new==. |workwhts_new==.|intlblks_new==.| intlwhts_new==. |marwht==. |marblk==.|degree_i==.


// Create value labels for missing_all
label define missing_label 0 "observed" 1 "missing"

// Apply the value labels to missing_all
label values missing_all1 missing_label

// Check the frequencies again with labels

tab missing_all1

//dropping missing values
 
 *drop if missing_all1 ==1
   
//OLS REGRESSION // FOR AFFRMACT SAMPLE ONLY

//for black stereotypes and prejudices alone

 regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted
 
 //weighted regression
 svyset [pweight=wtssps] //declare  survey design and probibality weights
 
 svy:regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.marblk i.workblks_new i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
 

//white stereotypes alone

regress affrmact i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted

svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress affrmact i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted

//both white and black stereotypes

 regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i   //unweighted
 
svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted


—-----------------------------------------------------------------------------------------------------------------------------
//SUBSET 2, SAMPLE ONLY FOR HELPBLK, HANDLING MISSING VALUES


*RECODES 
do "/Users/namratashokeen/Downloads/recodes_19feb2025.do"


ta helpblk, m

//imputing varaibles
//imputing polviews

  ta partyid,m //have lesser missing than polviews
   drop if partyid==.d|partyid==.n //(83 observations deleted)//
   replace polviews_new=1 if missing(polviews_new)& inlist(partyid, 0,1,2)
   replace polviews_new=3 if missing(polviews_new)& inlist(partyid, 4,5,6)
   replace polviews_new = 2 if missing(polviews_new) & partyid == 3
   
//imputing religion

replace religion=0 if missing(religion) & inlist(relig16,1,2,3,5,6,7,8,9,10,11,12,13,14,15) //imputing similar values from relig16
	 
//checked if other matches found for imputation
ta religion if missing(religion)& !missing(other)
ta religion if missing(religion)& !missing(jew)
 ta religion if missing(religion)& !missing(othjew)
 ta religion if missing(religion)& !missing(oth16)
  ta religion if missing(religion)& !missing(othjew16)
  ta religion if missing(religion)& !missing(denom)
 
   
///imputing missing values in newrace
   ta newrace,m
    ta newrace racecen1,m //matching for imputing
   replace newrace = 6 if missing(newrace) & racecen1 == 1 & !missing(racecen1) //whites 
   replace newrace = 6 if missing(newrace) & racecen2 == 1 & !missing(racecen2) //whites
  replace newrace = 4 if missing(newrace) & racecen1 == 2 & !missing(racecen1)  //blacks
  replace newrace = 8 if missing(newrace) & racecen1 == 15 & !missing(racecen1)  //otherrace
   replace newrace = 2 if missing(newrace) & racecen1 == 3 & !missing(racecen1)  //AIAN
   replace newrace = 1 if missing(newrace) & racecen1 == 16 & !missing(racecen1) //hispanic
    replace newrace=6 if missing(newrace) & race == 1 & !missing(race)  //white using race
	  replace newrace=4 if missing(newrace) & race == 2 & !missing(race)  //black using race
   
   
  //impute degree
 ta degree_i,m
 ta degree_i education, m
 
 replace degree_i=0 if missing(degree_i) & education==1 //high school
  replace degree_i=1 if missing(degree_i) & education==2   //high school
 replace degree_i=2 if missing(degree_i) & education==3 //associate
 replace degree_i=3 if missing(degree_i) & education==4 //bachelors
 replace degree_i= 4 if missing(degree_i) & education==5 //gradaute
 
   
//excluding iap those who were not asked the questions, sample only for those who were asked

foreach var in  marblk marwht helpblk workblks_new workwhts_new intlblks_new intlwhts_new wrkwayup racdif1 racdif2 racdif3 racdif4 polviews_new class degree_i sex newrace religion {
    drop if `var' == .i
}
 
//generate dummy varaible for missing

gen missing_all2 = 0 // Create the variable and set all to 0 initially

// Replace missing_all to 1 if any of the variables have missing values

 replace missing_all2 = 1 if helpblk==.| sex==. | newrace==.| religion==.|polviews_new==.| class==.| racdif1==. | racdif2==. | racdif3 ==.|racdif4==. |workblks_new==. |workwhts_new==.|intlblks_new==.| intlwhts_new==. |marwht==. |marblk==.|degree_i==.


// Create value labels for missing_all
label define missing_lbl 0 "observed" 1 "missing"

// Apply the value labels to missing_all
label values missing_all2 missing_lbl

// Check the frequencies again with labels

tab missing_all2

//dropping missing values
 
 *drop if missing_all2==1

//OLS REGRESSION//FOR HELPBLK SAMPLE ONLY 

//for black stereotypes and prejudices alone

 regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted
 
 //weighted regression
 svyset [pweight=wtssps] //declare  survey design and probibality weights
 
 svy:regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.marblk i.workblks_new i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
 

//white stereotypes alone

regress helpblk i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted

svyset [pweight=wtssps] //declare  survey design and probibality weights

svy: regress helpblk i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted

//both white and black stereotypes

 regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i   //unweighted
 
svyset [pweight=wtssps] //declare  survey design and probibality weights

svy: regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
 —-------------------------------------------------------------------------------------------------------------------------------
//SUBSET 3, SAMPLE FOR BOTH AFFRMACT AND HELPBLK, HANDLING MISSING VALUES


*RECODES 
do "/Users/namratashokeen/Downloads/recodes_19feb2025.do"


ta affrmact, m
ta helpblk,m

//impute missing values in polview_new using partyid

drop if partyid==.d|partyid==.n
replace polviews_new=1 if missing(polviews_new)& inlist(partyid, 0,1,2)
   replace polviews_new=3 if missing(polviews_new)& inlist(partyid, 4,5,6)
   replace polviews_new = 2 if missing(polviews_new) & partyid == 3
  
//impute religion 

replace religion=0 if missing(religion) & inlist(relig16,1,2,3,5,6,7,8,9,10,11,12,13,14,15)

//checked if other matches found for imputation
ta religion if missing(religion)& !missing(other)
ta religion if missing(religion)& !missing(jew)
 ta religion if missing(religion)& !missing(othjew)
 ta religion if missing(religion)& !missing(oth16)
  ta religion if missing(religion)& !missing(othjew16)
  ta religion if missing(religion)& !missing(denom)
 
  
//imputing missing in newrace
  ta newrace,m
  ta newrace racecen1,m
   replace newrace = 6 if missing(newrace) & racecen1 == 1 & !missing(racecen1) //whites 
   replace newrace = 6 if missing(newrace) & racecen2 == 1 & !missing(racecen2) //whites
  replace newrace = 4 if missing(newrace) & racecen1 == 2 & !missing(racecen1)  //blacks
  replace newrace = 8 if missing(newrace) & racecen1 == 15 & !missing(racecen1)  //otherrace
   replace newrace = 2 if missing(newrace) & racecen1 == 3 & !missing(racecen1)  //AIAN
    replace newrace = 1 if missing(newrace) & racecen1 == 16 & !missing(racecen1)  //hisapnic
	 replace newrace=6 if missing(newrace) & race == 1 & !missing(race)  //white using race
	  replace newrace=4 if missing(newrace) & race == 2 & !missing(race)  //black using race
	  
//imputing degree
 //impute degree
 ta degree_i,m
 ta degree_i education, m
 
 replace degree_i=0 if missing(degree_i) & education==1 //high school
  replace degree_i=1 if missing(degree_i) & education==2   //high school
 replace degree_i=2 if missing(degree_i) & education==3 //associate
 replace degree_i=3 if missing(degree_i) & education==4 //bachelors
 replace degree_i= 4 if missing(degree_i) & education==5 //gradaute
 

 
//excluding iap those who were not asked the questions, sample only for those who were asked

foreach var in  marblk marwht helpblk affrmact workblks_new workwhts_new intlblks_new intlwhts_new wrkwayup racdif1 racdif2 racdif3 racdif4 polviews_new class degree_i sex newrace religion {
    drop if `var' == .i
}

//generate dummy varaible for missing

gen missing_all3 = 0 // Create the variable and set all to 0 initially

// Replace missing_all to 1 if any of the variables have missing values
 replace missing_all3 = 1 if affrmact==.| helpblk==.|sex==. | newrace==.| religion==.|polviews_new==.| class==.| racdif1==. | racdif2==. | racdif3 ==.|racdif4==. |workblks_new==. |workwhts_new==.|intlblks_new==.| intlwhts_new==. |marwht==. |marblk==.|degree_i==.


// Create value labels for missing_all
label define MISSINGALL3 0 "observed" 1 "missing"

// Apply the value labels to missing_all
label values missing_all3 MISSINGALL3

// Check the frequencies again with labels

tab missing_all3

//dropping missing values
 
 *drop if missing_all3==1

//SAMPLE INCLUDING BOTH AFFRMACT AND HELPBLK
//OLS REGRESSION //FOR AFFRMACT 

//for black stereotypes and prejudices alone

 regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted
 
 //weighted regression
 svyset [pweight=wtssps] //declare  survey design and probibality weights
 
 svy:regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.marblk i.workblks_new i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
 

//white stereotypes alone

regress affrmact i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted

svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress affrmact i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted

//both white and black stereotypes

 regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i   //unweighted
 
svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress affrmact i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted

//OLS REGRESSION //FOR HELPBLK

//for black stereotypes and prejudices alone

 regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted
 
 //weighted regression
 svyset [pweight=wtssps] //declare  survey design and probibality weights
 
 svy:regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.marblk i.workblks_new i.intlblks_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
 

//white stereotypes alone

regress helpblk i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //unweighted

svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress helpblk i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted

//both white and black stereotypes

 regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i   //unweighted
 
svyset [pweight=wtssps] //declare  survey design and probability weights

svy: regress helpblk i.wrkwayup i.racdif1 i.racdif2 i.racdif3 i.racdif4 i.workblks_new i.marblk i.intlblks_new  i.workwhts_new i.marwht i.intlwhts_new i.newrace i.polviews_new i.sex i.religion i.class i.degree_i  //weighted
   
—------------------------------------------------------------------------------------------------------------------------------

