###############################################################################
#######                    INITIAL DATA CLEANING                        #######
###############################################################################



## Loading Packages -----------------------------------------------------------
library('tidyverse')
library('network')
library('sna')
library('igraph')
library('stringr')
library('GGally')
library('intergraph')
library('RecordLinkage')
library('visNetwork')
library('lubridate')
library('tools')
library('readxl')
library('stringdist')
library('reshape2')
library('lazyeval')
library('crimeVarCreation') # You will need to install this package locally if you haven't - it's located in the R project folder for the app.


## Importing data -------------------------------------------------------------
all_data <- read_csv('data_raw/Initiation.csv')

## Removing extraneous columns ------------------------------------------------
all_data <- all_data %>%
  select(-c(CHARGE_ID, CHARGE_VERSION_ID, CHAPTER, ACT, SECTION, CLASS, AOIC, EVENT_DATE, INCIDENT_END_DATE, ARREST_DATE, UNIT, RECEIVED_DATE, ARRAIGNMENT_DATE, EVENT))


## Renaming columns -----------------------------------------------------------
all_data <- all_data %>%
  rename("DATE" = INCIDENT_BEGIN_DATE) %>%
  rename("CITY" = INCIDENT_CITY) %>%
  rename("AGENCY" = LAW_ENFORCEMENT_AGENCY) %>%
  rename("UID" = CASE_PARTICIPANT_ID) %>%
  rename("DESCRIPTION" = OFFENSE_TITLE) %>%
  rename("INCIDENT_NO" = CASE_ID)


## Capitalizing all character columns -----------------------------------------
all_data$PRIMARY_CHARGE <- toupper(all_data$PRIMARY_CHARGE)
all_data$GENDER <- toupper(all_data$GENDER)
all_data$RACE <- toupper(all_data$RACE)
all_data$CITY <- toupper(all_data$CITY)


## Filtering out all non-primary charges --------------------------------------
all_data <- all_data %>%
  filter(PRIMARY_CHARGE == 'TRUE') %>%
  select(-PRIMARY_CHARGE)


## Turning numeric columns back into numerics ---------------------------------
all_data$AGE_AT_INCIDENT <- as.numeric(all_data$AGE_AT_INCIDENT)

## Cleaning date column -------------------------------------------------------
all_data <- all_data %>%
  separate(DATE, into = c('DATE', 'to_drop'), sep = ' ') %>%
  select(-c(to_drop))

## Fixing problems with UID/INCIDENT # ----------------------------------------
all_data <- all_data %>%
  mutate(UID_1 = as.character(UID)) %>%
  mutate(INC_1 = as.character(INCIDENT_NO)) %>%
  select(-c(UID, INCIDENT_NO)) %>%
  separate(UID_1, into = c('UID', 'drop_1')) %>%
  separate(INC_1, into = c('INCIDENT_NO', 'drop_2')) %>%
  select(-c(drop_1, drop_2))

## Simplifying dataset by removing duplicates ---------------------------------
all_data <- all_data %>%
  distinct(UID, INCIDENT_NO, .keep_all = TRUE)


###############################################################################
##########                SUSPECT DATA CLEANING                      ##########
###############################################################################
# This is the script for adding new suspect data. It is used to categorize
# crimes based on groups of interest. We will use these groups to a) identify
# which people have been accussed of certain crime groups, and b) aggregate
# those crimes so we can get a sense of suspects' criminal history

## Coding for Auto Burglary ---------------------------------------------------
burg_auto_codes <-  c('ATTEMPT VEHICULAR INVASION',
                      'ATTEMPT UNLAWFUL VEHICULAR INVASION',
                      'VEHICULAR INVASION',
                      'AID/ABET FLS REPORT VEH THEFT',
                      'CRIMINAL TRESPASS TO VEHICLES',
                      'AID/ABET POSS STOL TTL/CERTIF')

all_data <- gen_crime_type(burg_auto_codes, all_data, DESCRIPTION, CRIME_TYPE, 'AUTO BURGLARY')


## Coding for Other Burglary --------------------------------------------------
burg_other_codes <- c('ATTEMPT BURGLARY',
                    'BURGLARY',
                    'HOME INVASION',
                    'NO LIC/FALSE ENTRY/OMIT ENTRY',
                    'POSSESSION OF BURGLARY TOOLS',
                    'CRIM TRESPASS TO RESIDENCE',
                    'CRIMINAL TRESPASS TO A RESIDENCE',
                    'CRIMINAL TRESPASS BUILDING')

all_data <- gen_crime_type(burg_other_codes, all_data, DESCRIPTION, CRIME_TYPE, 'RESIDENTIAL BURGLARY')

## Coding for Robbery ---------------------------------------------------------
robbery_codes <- c('CONSPIRACY TO COMMIT ARMED ROBBERY',
                   'AGGRAVATED ROBBERY',
                   'ATTEMPT AGGRAVATED ROBBERY',
                   'ATTEMPT ARMED ROBBERY',
                   'ATTEMPT ARMED ROBBERY/DISCH FIREARM',
                   'ATTEMPT ROBBERY',
                   'CONSPIRACY \\(THEFT\\)',
                   'CONSPIRACY THEFT/\\$10K-100K/SCH/WRSHP/GOVT',
                   'CONSPIRACY AGGRAVATED ROBBERY',
                   'CONSPIRACY ARMED ROBBERY/NO FIREARM',
                   'CONSPIRACY TO COMMIT ARMED ROBBERY',
                   'CONSPIRACY TO COMMIT ROBBERY',
                   'ROBBERY')

all_data <- gen_crime_type(robbery_codes, all_data, DESCRIPTION, CRIME_TYPE, 'ROBBERY')


## Coding for Arson -----------------------------------------------------------
arson_codes <- c('AGG ARSON/KNOW PEOPLE PRESENT',
                 'AGGRAVATED ARSON',
                 'ARSON',
                 'ARSON/DEFR 0017212 AUD INS CO/>\\$150',
                 'ARSON/PLACE OF WORSHIP',
                 'ARSON/REAL/PERSONAL PROP>\\$150',
                 'ARSONIST FAIL TO REGISTER',
                 'ARSONIST FAIL TO REPORT CHANGE OF ADDRESS',
                 'ATTEMPT AGGRAVATED ARSON',
                 'ATTEMPT ARSON',
                 'ATTEMPT ARSON/REAL/PERSONAL PROP>\\$150',
                 'ATTEMPT RESIDENTIAL ARSON',
                 'CONSPIRACY TO COMMIT ARSON',
                 'RESIDENTIAL ARSON',
                 'SOLICITATION ARSON',
                 'SOLICITATION TO COMMITT ARSON',
                 'START FIRE/LAND/OTHER>\\$300-10K',
                 'START FIRE/LAND/SCHOOL 300>10K',
                 'CRIM DMG/GOVT PROP/EXPL/<\\$500',
                 'CRIM DMG/GOVT PROP/FIRE/<\\$500',
                 'CRIM DMG/START FIRE/\\$500-10K')

all_data <- gen_crime_type(arson_codes, all_data, DESCRIPTION, CRIME_TYPE, 'ARSON')


## Coding for Assault ---------------------------------------------------------
assault_codes <- c('AB/NEG/DISABL/CAREGIVER/DEATH',
                   'AB/NEG/PHY DISABL/CAREGIVER',
                   'ABUSE RESIDENT/INJURY FACILITY',
                   'ABUSE/NEGLECT OF ELDERLY OR DISABLED/CAREGIVER',
                   'ABUSE/NEGLECT OF ELDERLY OR DISABLED/DEATH/CAREGIVER',
                   'AGG ASLT PC OFF/FIREMAN WEAPON',
                   'AGG ASLT PC OFFICER/VOLUNTEER',
                   'AGG ASLT PEACE OFF/FIRE/ER WRK',
                   'AGG ASLT VOLUNTEER W/ WEAPON',
                   'AGG ASLT/DDLY WEAP/AIR RIFLE',
                   'AGG ASLT/PD/SHERIFF EMP W/FIR',
                   'AGG ASLT/USE DDLY WEAP/RIFLE',
                   'AGG ASSAULT/PEACE OFF/FIREMAN',
                   'AGG ASSAULT/TRANSIT EMPLOYEE',
                   'AGG ASSAULT/USE FIR/PEACE OFF',
                   'AGG BATTERY W/ FIREARM/PERSON',
                   'AGG BATTERY/CONTROLLED SUB',
                   'AGG BATTERY/CORR EMP/EMP DHS',
                   'AGG BATTERY/EMP GOVT/SCHOOL',
                   'AGG BATTERY/GREAT BODILY HARM',
                   'AGG BATTERY/PROCESS SERVER',
                   'AGG BATTERY/TAXI DRIVER',
                   'AGG BATTERY/VIDEO/AUDIO REC',
                   'AGG BTRY/GRT BOD HARM/TORTURE',
                   'AGGRAVATED ASSAULT',
                   'AGGRAVATED ASSAULT OF A PEACE OFFICER WITH A MOTOR VEHICLE',
                   'AGGRAVATED ASSAULT/PUBLIC PROP',
                   'AGGRAVATED BATTERY',
                   'AGGRAVATED BATTERY OF A CHILD',
                   'AGGRAVATED BATTERY OF A SENIOR CITIZEN',
                   'AGGRAVATED BATTERY OF AN UNBORN CHILD',
                   'AGGRAVATED BATTERY STRANGULATION',
                   'AGGRAVATED BATTERY WITH A FIREARM',
                   'AGGRAVATED BATTERY/STRANGLE',
                   'AGGRAVATED DOMESTIC BATTERY',
                   'ASSAULT',
                   'ATTEMPT \\(AGGRAVATED DOMESTIC BATTERY\\)',
                   'ATTEMPT AGGRAVATED BATTERY',
                   'ATTEMPT AGGRAVATED BATTERY/STRANGLE',
                   'ATTEMPT HEINOUS BATTERY',
                   'BATTERY/CAUSE BODILY HARM',
                   'BATTERY/MAKES PHYSICAL CONTACT',
                   'CONSPIRACY TO COMMIT AGGRAVATED BATTERY',
                   'CONSPIRACY TO COMMITT BATTERY',
                   'CONSPIRACY\\(AGGRAVATED BATTERY\\)',
                   'HEINOUS BATTERY',
                   'SOLICITATION OF AGGRAVATED BATTERY',
                   'AGGRAVATED DOMESTIC BATTERY',
                   'CRIM AB/NEGL PHYSICAL ABUSE',
                   'CRIM NEGLECT RESIDENT/DEATH')

all_data <- gen_crime_type(assault_codes, all_data, DESCRIPTION, CRIME_TYPE, 'ASSAULT')


## Coding for Sex Offense, Forcible -------------------------------------------
sex_assault_codes <- c('AGG CRIM SEX AB/FORCE/VIC 9-16',
                       'AGG CRIM SEX AB/INTEL DISABL',
                       'AGG CRIM SEX ABUSE/VIC 13-16',
                       'AGG CRIM SEX ABUSE/VICTIM <9',
                       'AGG CRIM SEX ASLT/BODILY HARM',
                       'AGG CRIM SEX ASLT/INTEL DISABL',
                       'AGG CRIM SEX ASLT/VICTIM <13',
                       'AGG CRIM SEX ASLT/VICTIM <9',
                       'AGG CRIM SEX ASSAULT / VICTIM < 9',
                       'AGG CRIM SEXUAL ABUSE/RETARDED',
                       'AGGRAVATED CHILD PORNOGRAPHY',
                       'AGGRAVATED CRIMINAL SEXUAL  ABUSE/THREATEN LIFE',
                       'AGGRAVATED CRIMINAL SEXUAL ABUSE',
                       'AGGRAVATED CRIMINAL SEXUAL ABUSE/BODILY HARM',
                       'AGGRAVATED CRIMINAL SEXUAL ABUSE/CONTROLLED SUBSTANCE',
                       'AGGRAVATED CRIMINAL SEXUAL ABUSE/VICTIM HANDICAP',
                       'AGGRAVATED CRIMINAL SEXUAL ABUSE/WEAPON',
                       'AGGRAVATED CRIMINAL SEXUAL ASSAULT',
                       'AGGRAVATED INVOLUNTARY SERVITUDE',
                       'AGGRAVATED INVOLUNTARY SEXUAL SERVITUDE OF A MINOR',
                       'AGGRAVATED TRAFFICKING IN PERSONS',
                       'ATTEMPT \\(CRIMINAL SEXUAL ASSAULT\\)',
                       'ATTEMPT AGGRAVATED CRIMINAL SEXUAL ABUSE',
                       'ATTEMPT AGGRAVATED CRIMINAL SEXUAL ABUSE OF A FAMILY MEMBER UNDER 18 YEARS OF AGE',
                       'ATTEMPT AGGRAVATED CRIMINAL SEXUAL ABUSE OF A VICTIM 13 TO LESS THAN 17 YEARS OF AGE',
                       'ATTEMPT AGGRAVATED CRIMINAL SEXUAL ABUSE OF A VICTIM LESS THAN 13 YEARS OF AGE',
                       'ATTEMPT AGGRAVATED CRIMINAL SEXUAL ASSAULT',
                       'ATTEMPT CHILD PORNOGRAPHY',
                       'ATTEMPT CRIMINAL SEXUAL ASSAULT',
                       'ATTEMPT CRIMINAL SEXUAL ASSAULT/FORCE',
                       'ATTEMPT PREDATORY CRIMINAL SEXUAL ASSAULT',
                       'ATTEMPT PREDATORY CRIMINAL SEXUAL ASSAULT OF A VICTIM LESS THAN 13 YEARS OF AGE',
                       'ATTEMPT SEX OFFEN/PHOTO/VIDEO CHILD',
                       'CHIL PORN/PHOTO/VIC <13/PRI',
                       'CHIL PORN/POSE/EXHIBIT/VIC <13',
                       'CHIL PORN/REPRODUCE/VIC<13/PRI',
                       'CHIL SEX OFFEN/RESIDE DAY CARE',
                       'CHILD ABANDONMENT',
                       'CHILD ABDUCTION',
                       'CHILD ABDUCTN/FATHER CONCEALS',
                       'CHILD ABDUCTN/LURE CHILD<16/2\\+',
                       'CHILD ABDUCTN/MOTHER/ABANDONED',
                       'CHILD ABDUCTN/PARENT/15 DAYS',
                       'CHILD ABDUCTN/PARENT/PHY FORCE',
                       'CHILD LURING/OFFEN 21\\+/PRECONV',
                       'CHILD PORN/EXHIBITION/MOV DPTN',
                       'CHILD PORN/FONDLING/MOVING DPT',
                       'CHILD PORN/MOUTH/ANUS/MOV DPTN',
                       'CHILD PORN/MOVING DEPICTION',
                       'CHILD PORN/PERS/ANIM/MOV DPTN',
                       'CHILD PORN/POSS COMPUTER PHOTO',
                       'CHILD PORN/POSS PHOTO/VIC <13',
                       'CHILD PORN/POSS/MOVING DPTN',
                       'CHILD PORN/REPRODUCE/MOV DPTN',
                       'CHILD PORN/REPRODUCE/VIC <13',
                       'CHILD PORN/SOL CHILD/MOV DPTN',
                       'CHILD PORN/SOL CHILD/PHOTO',
                       'CHILD PORN/SOL CHILD/VIC <13',
                       'CHILD PORNOGRAPHY',
                       'CHILD PORNOGRAPHY/PHOTOGRAPH',
                       'CHILD PORNOGRAPHY/VICTIM <13',
                       'CHILD SEX OFFEN/COMMU INTERNET',
                       'CHILD SEX OFFEN/PART HOL EVENT',
                       'CHILD SEX OFFENDER/LOITER PARK',
                       'CHILD SEX OFFENDER/PUBLIC PARK',
                       'CHILD SX OFFEN/PROVIDE PROGRAM',
                       'CRIM SEX ASSAULT/CANT CONSENT',
                       'CRIMINAL SEXUAL ABUSE',
                       'CRIMINAL SEXUAL ASSAULT',
                       'CUSTODIAL SEXUAL MISCONDUCT',
                       'ENTICE CHIL REMOVE CLOTHES/2ND',
                       'ENTICE CHIL REMOVE CLOTHES/SCH',
                       'EXPLOIT CHILD<13/EXPOSE SELF',
                       'EXPLOIT CHLD<13/REMOVE CLOTHES',
                       'EXPLOITATION CHILD/SEX ACT/2ND',
                       'EXPLOITATION/EXPOSE ORGANS/2ND',
                       'INDECENT SOL/CHILD/INTERNET',
                       'INDECENT SOLICITATION OF A CHILD',
                       'INDECENT SOLICITATION OF AN ADULT',
                       'INVOLUNTARY SERVITUDE',
                       'INVOLUNTARY SEXUAL SERVITUDE OF A MINOR',
                       'KEEP PLACE OF PROSTITUTION/CERTAINTY',
                       'KEEP PLACE OF PROSTITUTION/CERTAINTY/WITHIN 1000 FEET OF SCHOOL',
                       'KEEP PLACE PROSTITUTION/CONT AFTER AWARE',
                       'KEEP PLACE PROSTITUTION/REASONABLE CERTAINTY',
                       'NON-CONSENSUAL DISSM SEX IMAGE',
                       'PATRN MINOR PRO/1000 FT SCH',
                       'PATRONIZE PROSTITUTE/SEX',
                       'PATRONIZE/1000 FEET OF SCHOOL',
                       'PATRONIZING/1000 FEET SCHOOL',
                       'PERMIT SEXUAL ABUSE OF CHILD',
                       'PERMITTING THE SEXUAL ABUSE OF A CHILD',
                       'PERMITTING UNLAWFUL USE OF A BUILDING',
                       'PERSON >18 COMPEL ORG MBR <18',
                       'PIMPING',
                       'PIMPING WITHIN 1000 FEET OF SCHOOL',
                       'PREDATORY CRIMINAL SEXUAL ASLT',
                       'PREDATORY CRIMINAL SEXUAL ASSAULT OF A CHILD',
                       'PROMOTING JUVENILE PROSTITUTION',
                       'PROMOTING PROSTITUTION',
                       'PUB INDECENCY/EXPOSURE/SCHOOL',
                       'PUB INDECENCY/SEX CONDUCT 3\\+',
                       'PUB INDECENCY/SEX/SCH GROUNDS',
                       'PUB OFFICIAL/EMP TAMPER W/ REC',
                       'PUBLIC INDECENCY',
                       'PUBLIC INDECENCY/EXPOSURE/3\\+',
                       'SEX EXPLOIT CHIL/EXPOSE/SCHOOL',
                       'SEX EXPLOIT CHILD/SEX ACT/SCH',
                       'SEX EXPLOIT CHILD<13/SEX ACT',
                       'SEX MISCON/DISABILITY/DHS EMP',
                       'SEX MISCON/DISABILITY/DHS FUND',
                       'SEX OFFEN/PHOTO/PLAYGROUND',
                       'SEX PREDATOR/PUBLIC PARK/2\\+',
                       'SEX WITH ANIMAL',
                       'SEXUAL EXPLOITATION OF A CHILD',
                       'SEXUAL RELATIONS WITHIN FAMILIES',
                       'SEXUALLY VIOLENT PERSON ESCAPE',
                       'SOL SEX ACT <18/INTEL DISABL',
                       'SOL TO MEET A CHILD/5YR\\+ OLDER',
                       'SOLICITATION OF MURDER',
                       'SOLICITATION OF MURDER FOR HIRE',
                       'SOLICITATION/MURDER',
                       'TRAFFICKING IN PERSONS',
                       'TRAFFICKING IN PERSONS FOR FORCED LABOR OR SERVICES',
                       'TRAVELING TO MEET A MINOR',
                       'UNAUTHD VIDEOTAPING/SEX OFFEN',
                       'UNAUTHD VIDEOTAPING/VICTIM <18',
                       'UNAUTHORIZED VIDEOTAPING',
                       'UNLAWFUL VIDEOTAPING/VICTIM<18',
                       'VIDEO PUBLIC PLACE/SEX OFFEN',
                       'VIDEO PUBLIC PLACE/VICTIM<18',
                       'VIDEO UNDER CLOTHES/VICTIM<18',
                       'VIDEO/OUTSIDE RESID/ VIC <18',
                       'VIDEO/RESIDENCE/VICTIM<18',
                       'VIOLATION OF THE SEX OFFENDER REGISTRATION ACT',
                       'WINDOW PEEPING 3RD\\+')

all_data <- gen_crime_type(sex_assault_codes, all_data, DESCRIPTION, CRIME_TYPE, 'SEX OFFENSE, FORCIBLE')


## Coding for Larceny/Theft ---------------------------------------------------
larceny_codes <- c('ATTEMPT \\(THEFT\\)',
                   'CONSPIRACY TO COMMIT THEFT',
                   'CONSPIRACY TO COMMIT THEFT DECEP INTENT 10K<100K',
                   'CONSPIRACY TO COMMIT THEFT/\\$500-\\$10K/SCHOOL/WORSHIP',
                   'ATTEMPT AGGRAVATED IDENTITY THEFT',
                   'ATTEMPT RETAIL THEFT',
                   'ATTEMPT THEFT',
                   'ATTEMPT THEFT CONTROL INTENT 10K\\<100K',
                   'ATTEMT THEFT BY DECEPTION OF VICTIM 60 YEARS OF AGE OR OLDER GREATER THEN \\$5,000',
                   'ONLILNE SALE/STOLEN PROP/>\\$300',
                   'ONLINE SALE OF STOLEN PROPERTY',
                   'ONLILNE SALE/STOLEN PROP/>\\$150',
                   'ONLILNE SALE/STOLEN PROP>\\$300',
                   'ONLILNE SALE/STOLEN PROP/<\\$300',
                   'ONLINE THEFT/EXCEEDS \\$150',
                   'ONLINE THEFT/NOT EXCEED \\$150',
                   'ONLINE THFT BY DECEPTION/<\\$300',
                   'ONLINE THFT BY DECEPTION/>\\$300',
                   'RET THEFT/DISP MERCH/<\\$150/1ST',
                   'RET THEFT/SWITCH PRICE/<\\$300',
                   'RET THEFT/UNDER-RING/<\\$300',
                   'RETAIL THEFT',
                   'RETAIL THEFT/FALSE REP/<\\$300',
                   'RETAIL THEFT/MOTOR FUEL/<\\$150',
                   'RETAIL THEFT/RETURN/<\\$150',
                   'RETAILTHEFT',
                   'THEFT',
                   'THEFT <\\$150/PREV FEL CONVIC',
                   'THEFT LAW KNOW DPRV 10K<100K',
                   'THEFT LOST/MISLAID PROP/<\\$500',
                   'THEFT LOST/MISLAID PROP/>\\$10K',
                   'THEFT OF LEASED PROPERTY/>\\$500',
                   'THEFT OF LOST/MISLAID PROPERTY',
                   'THEFT OF UTILITY SERVICES',
                   'THEFT STOLEN PROB DPRV 300<10K',
                   'THEFT/\\$10K-100K/SCHOOL/WORSHIP',
                   'THEFT/\\$300-\\$10K/SCH/WRSHP/GOVT',
                   'THEFT/\\$300-\\$10K/SCHOOL/WORSHIP',
                   'THEFT/COIN OP MACHINE/2ND\\+',
                   'THEFT/CON/PRIOR CONVIC <300',
                   'THEFT/CONTROL/PERSON <\\$300',
                   'THEFT/DAMAGE/FACILITY >300',
                   'THEFT/DAMAGE/FACILITY>300-10K',
                   'THEFT/DECEP/FACILITY',
                   'THEFT/DECEP/FACILITY>300-10K',
                   'THEFT/DECEPTION/ > \\$500K',
                   'THEFT/DECEPTION/PRIOR CONVIC',
                   'THEFT/LABOR/SERVICES/PROPERTY',
                   'THEFT/LIBRARY MATERIAL >\\$300',
                   'THEFT/STOL/LAW ENF/>\\$300 <10K',
                   'THEFT/STOLEN/ > \\$500K',
                   'THEFT/STOLEN/>\\$300 <10K',
                   'THEFT/STOLEN/>100K',
                   'THEFT/STOLEN/PRIOR CONVICTION',
                   'THEFT/UNAUTHD CONTROL/>100K',
                   'THEFT/USE EMER EXIT/>\\$150',
                   'THEFT<\\$300/SCHOOL/WORSHIP/GOVT',
                   'THFT LOST/MISLAID PROP/500-10K',
                   'THFT/FLS REP/FUEL/<150/PRECONV')

all_data <- gen_crime_type(larceny_codes, all_data, DESCRIPTION, CRIME_TYPE, 'LARCENY/THEFT')


## Coding for Vehicle Theft ---------------------------------------------------
vehicle_theft_codes <- c('VEHICULAR HIJACKING',
                         'ORGANIZER OF AN AGGRAVATED VEHICLE THEFT CONSPIRACY',
                         'VEHICLE THEFT CONSPIRACY',
                         'AGGRAVATED VEHICULAR HIJACKING',
                         'AGGRAVATED VEHICULAR HIJACKING/FIREARM',
                         'ATTEMPT AGGRAVATED VEHICLE HIJACKING/FIREARM',
                         'ATTEMPT AGGRAVATED VEHICULAR HIJACKING',
                         'ATTEMPT POSSESSION OF A STOLEN MOTOR VEHICLE',
                         'ATTEMPT VEHICULAR HIJACKING',
                         'POSSESSION OF A STOLEN MOTOR VEHICLE',
                         'AID/ABET/POSS/SELL STOLEN VEH',
                         'AGGRAVATED POSSESSION OF A STOLEN MOTOR VEHICLE')

all_data <- gen_crime_type(vehicle_theft_codes, all_data, DESCRIPTION, CRIME_TYPE, 'VEHICLE THEFT')


###############################################################################
##########                   CREATING DUMMY VARIABLES                ##########
###############################################################################
## This section creates dummies for violent crimes, property crimes, auto
## burglaries, gun violence, and gun crimes

## Dummy for Gun Incident -----------------------------------------------------
gun_codes <- c()

all_data <- Crime_type_dummies(gun_codes, all_data, DESCRIPTION, GUN_INCIDENT)


## Dummy for Violent Gun Incident ---------------------------------------------
gun_violence_codes <- c('AGG ASLT PC OFF/FIREMAN WEAPON',
                        'AGG ASLT/PD/SHERIFF EMP W/FIR',
                        'AGG ASLT/USE DDLY WEAP/RIFLE',
                        'AGG BATTERY W/ FIREARM/PERSON',
                        'AGG DISCHARGE FIREARM/OCC VEH',
                        'AGG FLEE/CONCL/ALT REGIS PLT',
                        'AGG FLEE/CONCL/ALT REGIS PLT/2',
                        'AGG POSS 11-20 STOLEN FIREARMS',
                        'AGG POSS 21-30 STOLEN FIREARMS',
                        'AGG POSS 2-5 STOLEN FIREARMS',
                        'AGG POSS 6-10 STOLEN FIREARMS',
                        'AGG POSS/2-5 CONVERTED FIREARM',
                        'AGG POSS/2-5 STOLEN FIREARMS',
                        'AGG POSS/DEL 2-5 STOL FIREARMS',
                        'AGG UUW/LOADED/NO FCCA/FOID',
                        'AGG UUW/LOADED/NO FCCA/FOID/2\\+',
                        'AGG UUW/UNLOADED/NO FCCA',
                        'AGG UUW/VEH/FIR LOADED/NO FOID',
                        'AGG UUW/VEHICLE/LOADED FIREARM',
                        'AGGRAVATED DISCHARGE OF A FIREARM',
                        'AGGRAVATED UNLAWFUL USE OF A WEAPON',
                        'AGGRAVATED UNLAWFUL USE OF WEAPON',
                        'ARMED HABITUAL CRIMINAL',
                        'ARMED VIOLENCE',
                        'ARMED VIOLENCE/CATEGORY I/II',
                        'ARMED VIOLENCE/DISCH WEAPON',
                        'ATTEMPT \\(ARMED ROBBERY\\)',
                        'ATTEMPT AGGRAVATED DISCHARGE OF A FIREARM',
                        'ATTEMPT AGGRAVATED UNLAWFUL USE OF WEAPON',
                        'ATTEMPT ARMED ROBBERY',
                        'ATTEMPT ARMED ROBBERY/DISCH FIREARM',
                        'CARRY CONCEAL FIR/U INFLU/1-2',
                        'CARRY CONCEALED FIR/AIRPORT',
                        'CONPIRACY TO COMMIT SELL FIREARM/NO VALID FOID',
                        'DEFACING IDENTIFICATION MARKS OF FIREARMS',
                        'DEL CONCEALED FIR/MINOR <18',
                        'DEL CONCEALED FIR/MINOR/SCHOOL',
                        'DEL FIREARM BEFORE 72 HOURS',
                        'DEL FIREARM/NO VALID FOID',
                        'DEL FIREARM/NO VALID FOID/3\\+',
                        'DELIVER STOLEN FIREARMS/2-5',
                        'DELIVERY OF CONVERTED FIREARM',
                        'FIREARM W/O VALID FOID/ELIG',
                        'FIREARM W/O VALID FOID/ELIG/2\\+',
                        'FIREARM/FOID INVALID/NOT ELIG',
                        'GUNRUNNING',
                        'GUNRUNNING 11-20 FIREARMS',
                        'MACHINE GUN/AUTO WEAPON/SCH/PK',
                        'POSS FIREARM FOID EXPIRED',
                        'POSS FIREARM FOID NOT ISSUED - NOT ELIGIBLE',
                        'POSS FIREARM FOID REVOKED',
                        'POSS FIREARM/EXPL PENAL INST',
                        'POSSESS FIREARM PROJECTILE',
                        'POSSESSION OF A STOLEN FIREARM',
                        'POSSESSION OF STOLEN FIREARM',
                        'PURCH 1 FIREARM/FALSE INFO',
                        'PURCH 1 FIREARM/INTENT',
                        'PURCH 11<20 FIREARM/FALSE INFO',
                        'PURCH 2<5 FIREARMS/FALSE INFO',
                        'PURCH 2<5 FIREARMS/INTENT',
                        'PURCH 2-5 FIREARMS/FALSE INFO',
                        'PURCH 2-5 FIREARMS/INTENT',
                        'PURCH 6\\+ FIREARMS/FALSE INFO',
                        'PURCH 6<10 FIREARMS/FALSE INFO',
                        'RECKLESS DISCHARGE OF A FIREARM',
                        'SALE CONCEALED FIR/MINOR <18',
                        'SALE FIREARM BEFORE 72 HOURS',
                        'SALE STOLEN FIREARMS/6-10',
                        'UNLAWFUL POSSESSION OF A FIREARM BY A STREET GANG MEMBER',
                        'UNLAWFUL SALE FIREARM TO FELON',
                        'UNLAWFUL SALE OF FIREARM/FELON',
                        'UNLAWFUL USE OF A WEAPON',
                        'UNLAWFUL USE OR POSSESSION OF A WEAPON BY A FELON',
                        'UNLAWFUL USE OR POSSESSION OF A WEAPON BY A PERSON IN THE CUSTODY OF A FACILITY OF THE DEPARTMENT OF CORRECTIONS',
                        'UNLWFL DEL OF STOLEN FIREARM',
                        'UNLWFL POSS FIREARM/<18',
                        'UNLWFL POSS FIREARM/DEL/<21',
                        'UNLWFL POSS HANDGUN UNDER 18',
                        'UNLWFL POSS HANDGUN/DEL/<21',
                        'UNLWFL SALE FIREARMS TO MINOR',
                        'UNLWFL SALE OF STOLEN FIREARM',
                        'UNLWFL USE FIREARM PROJECTILE',
                        'USE STOLEN FIR/COMMIT OFFENSE')

all_data <- Crime_type_dummies(gun_violence_codes, all_data, DESCRIPTION, GUN_VIOLENT_INCIDENT)

## Dummy for Property Crime ---------------------------------------------------
property_codes <- c('RESIDENTIAL BURGLARY', 
                    'AUTO BURGLARY', 
                    'OTHER BURGLARY', 
                    'LARCENY/THEFT', 
                    'VEHICLE THEFT', 
                    'ARSON')

all_data <- Crime_type_dummies(property_codes, all_data, CRIME_TYPE, PROPERTY_INCIDENT)


## Dummy for Violent Incident -------------------------------------------------
violence_codes <- c('^ASSAULT$', 
                    'ROBBERY', 
                    'SEX OFFENSE, FORCIBLE')

all_data <- Crime_type_dummies(violence_codes, all_data, CRIME_TYPE, VIOLENT_INCIDENT)


## Dummy for Auto Burglary ----------------------------------------------------
all_data <- Crime_type_dummies(burg_auto_codes, all_data, DESCRIPTION, AUTOBURG_INCIDENT)


## Dummy for Domestic Violence ------------------------------------------------
dv_codes <- c('DOM BTRY/BOD HARM/3 PRECONV',
              'DOM BTRY/BOD HARM/4\\+ PRECONV',
              'DOM BTRY/CONTACT/1-2  PRECONV',
              'DOM BTRY/CONTACT/3 PRECONV',
              'DOM BTRY/CONTACT/4\\+ PRECONV',
              'DOM BTRY/HARM/1-2  PRECONV',
              'DOMESTIC BATTERY',
              'DOMESTIC BATTERY/OTHER PRIOR',
              'DOMESTIC BTRY/CONTACT/VIO O/P',
              'CRIM NEGLECT RESIDENT/DEATH')

all_data <- Crime_type_dummies(dv_codes, all_data, DESCRIPTION, DV_INCIDENT)


## Dummy for Aggravated Violence ----------------------------------------------
agg_violence_codes <- c('AGG ASLT PC OFF/FIREMAN WEAPON',
                        'AGG ASLT/PD/SHERIFF EMP W/FIR',
                        'AGG ASLT/USE DDLY WEAP/RIFLE',
                        'AGG BATTERY W/ FIREARM/PERSON',
                        'AGG DISCHARGE FIREARM/OCC VEH',
                        'AGG FLEE/CONCL/ALT REGIS PLT',
                        'AGG FLEE/CONCL/ALT REGIS PLT/2',
                        'AGG POSS 11-20 STOLEN FIREARMS',
                        'AGG POSS 21-30 STOLEN FIREARMS',
                        'AGG POSS 2-5 STOLEN FIREARMS',
                        'AGG POSS 6-10 STOLEN FIREARMS',
                        'AGG POSS/2-5 CONVERTED FIREARM',
                        'AGG POSS/2-5 STOLEN FIREARMS',
                        'AGG POSS/DEL 2-5 STOL FIREARMS',
                        'AGG UUW/LOADED/NO FCCA/FOID',
                        'AGG UUW/LOADED/NO FCCA/FOID/2\\+',
                        'AGG UUW/UNLOADED/NO FCCA',
                        'AGG UUW/VEH/FIR LOADED/NO FOID',
                        'AGG UUW/VEHICLE/LOADED FIREARM',
                        'AGGRAVATED DISCHARGE OF A FIREARM',
                        'AGGRAVATED UNLAWFUL USE OF A WEAPON',
                        'AGGRAVATED UNLAWFUL USE OF WEAPON',
                        'AGGRAVATED VEHICULAR HIJACKING',
                        'AGGRAVATED VEHICULAR HIJACKING/FIREARM',
                        'AGGRAVATED POSSESSION OF A STOLEN MOTOR VEHICLE',
                        'AGG CRIM SEX AB/FORCE/VIC 9-16',
                        'AGG CRIM SEX AB/INTEL DISABL',
                        'AGG CRIM SEX ABUSE/VIC 13-16',
                        'AGG CRIM SEX ABUSE/VICTIM <9',
                        'AGG CRIM SEX ASLT/BODILY HARM',
                        'AGG CRIM SEX ASLT/INTEL DISABL',
                        'AGG CRIM SEX ASLT/VICTIM <13',
                        'AGG CRIM SEX ASLT/VICTIM <9',
                        'AGG CRIM SEX ASSAULT / VICTIM < 9',
                        'AGG CRIM SEXUAL ABUSE/RETARDED',
                        'AGGRAVATED CHILD PORNOGRAPHY',
                        'AGGRAVATED CRIMINAL SEXUAL  ABUSE/THREATEN LIFE',
                        'AGGRAVATED CRIMINAL SEXUAL ABUSE',
                        'AGGRAVATED CRIMINAL SEXUAL ABUSE/BODILY HARM',
                        'AGGRAVATED CRIMINAL SEXUAL ABUSE/CONTROLLED SUBSTANCE',
                        'AGGRAVATED CRIMINAL SEXUAL ABUSE/VICTIM HANDICAP',
                        'AGGRAVATED CRIMINAL SEXUAL ABUSE/WEAPON',
                        'AGGRAVATED CRIMINAL SEXUAL ASSAULT',
                        'AGGRAVATED INVOLUNTARY SERVITUDE',
                        'AGGRAVATED INVOLUNTARY SEXUAL SERVITUDE OF A MINOR',
                        'AGGRAVATED TRAFFICKING IN PERSONS',
                        'AGG ASLT PC OFF/FIREMAN WEAPON',
                        'AGG ASLT PC OFFICER/VOLUNTEER',
                        'AGG ASLT PEACE OFF/FIRE/ER WRK',
                        'AGG ASLT VOLUNTEER W/ WEAPON',
                        'AGG ASLT/DDLY WEAP/AIR RIFLE',
                        'AGG ASLT/PD/SHERIFF EMP W/FIR',
                        'AGG ASLT/USE DDLY WEAP/RIFLE',
                        'AGG ASSAULT/PEACE OFF/FIREMAN',
                        'AGG ASSAULT/TRANSIT EMPLOYEE',
                        'AGG ASSAULT/USE FIR/PEACE OFF',
                        'AGG BATTERY W/ FIREARM/PERSON',
                        'AGG BATTERY/CONTROLLED SUB',
                        'AGG BATTERY/CORR EMP/EMP DHS',
                        'AGG BATTERY/EMP GOVT/SCHOOL',
                        'AGG BATTERY/GREAT BODILY HARM',
                        'AGG BATTERY/PROCESS SERVER',
                        'AGG BATTERY/TAXI DRIVER',
                        'AGG BATTERY/VIDEO/AUDIO REC',
                        'AGG BTRY/GRT BOD HARM/TORTURE',
                        'AGGRAVATED ASSAULT',
                        'AGGRAVATED ASSAULT OF A PEACE OFFICER WITH A MOTOR VEHICLE',
                        'AGGRAVATED ASSAULT/PUBLIC PROP',
                        'AGGRAVATED BATTERY',
                        'AGGRAVATED BATTERY OF A CHILD',
                        'AGGRAVATED BATTERY OF A SENIOR CITIZEN',
                        'AGGRAVATED BATTERY OF AN UNBORN CHILD',
                        'AGGRAVATED BATTERY STRANGULATION',
                        'AGGRAVATED BATTERY WITH A FIREARM',
                        'AGGRAVATED BATTERY/STRANGLE',
                        'AGGRAVATED DOMESTIC BATTERY',
                        'ATTEMPT \\(AGGRAVATED DOMESTIC BATTERY\\)',
                        'ATTEMPT AGGRAVATED BATTERY',
                        'ATTEMPT AGGRAVATED BATTERY/STRANGLE',
                        'CONSPIRACY TO COMMIT AGGRAVATED BATTERY',
                        'CONSPIRACY\\(AGGRAVATED BATTERY\\)',
                        'HEINOUS BATTERY',
                        'SOLICITATION OF AGGRAVATED BATTERY',
                        'AGGRAVATED DOMESTIC BATTERY',
                        'AGG ARSON/KNOW PEOPLE PRESENT',
                        'AGGRAVATED ARSON',
                        'CONSPIRACY TO COMMIT ARMED ROBBERY',
                        'AGGRAVATED ROBBERY',
                        'ATTEMPT AGGRAVATED ROBBERY')

all_data <- Crime_type_dummies(agg_violence_codes, all_data, DESCRIPTION, AGG_VIOLENCE_INCIDENT)


###############################################################################
##########                    RACE AND SEX DUMMIES                   ##########
###############################################################################

## Creating Male Dummy --------------------------------------------------------
all_data$MALE <- as.numeric(all_data$GENDER == 'MALE')


## Creating Race Dummies ------------------------------------------------------
all_data$WHITE <- as.numeric(all_data$RACE == 'WHITE'| all_data$RACE == 'WHITE [HISPANIC OR LATINO]')
all_data$BLACK <- as.numeric(all_data$RACE == 'BLACK')
all_data$HISPANIC <- as.numeric(all_data$RACE == 'HISPANIC')
all_data$ASIAN <- as.numeric(all_data$RACE == 'ASIAN')


###############################################################################
##########                  AGGREGATING VARIABLES                    ##########
###############################################################################


## Creating columns that count overall offenses by incident type --------------
all_data <- AggregatedColumns(all_data, AGG_VIOLENCE_INCIDENT, COMMIT_AGG_VIOLENCE, TOTAL_AGG_VIOLENCE)
all_data <- AggregatedColumns(all_data, GUN_INCIDENT, COMMIT_GUN, TOTAL_GUN)
all_data <- AggregatedColumns(all_data, GUN_VIOLENT_INCIDENT, COMMIT_GUN_VIOLENT, TOTAL_GUN_VIOLENT)
all_data <- AggregatedColumns(all_data, PROPERTY_INCIDENT, COMMIT_PROPERTY, TOTAL_PROPERTY)
all_data <- AggregatedColumns(all_data, AUTOBURG_INCIDENT, COMMIT_AUTOBURG, TOTAL_AUTOBURG)
all_data <- AggregatedColumns(all_data, DV_INCIDENT, COMMIT_DV, TOTAL_DV)
all_data <- AggregatedColumns(all_data, VIOLENT_INCIDENT, COMMIT_VIOLENT, TOTAL_VIOLENT)



## Creating column for most recent crime year ---------------------------------
#all_data <- all_data %>%
#  group_by(UID) %>%
#  mutate(MOST_RECENT_CRIME_YEAR = max(year(DATE)))


## Totaling up incidents per UID
all_data <- all_data %>%
  group_by(UID) %>%
  mutate(TOTAL_INCIDENTS = n())



###############################################################################
##########                 CREATING CUMULATIVE COLUMNS               ##########
###############################################################################
# The below code create cumulative summary stats for UIDs, i.e. how many violent
# incidents the suspect had committed up to that point in time.


## Adding columns of rolling cumulative sums for variables of interest --------
all_data <- all_data %>%
  group_by(UID) %>%
  arrange(DATE) %>%
  mutate(CUMULATIVE_PROPERTY_INCIDENTS = cumsum(PROPERTY_INCIDENT),
         CUMULATIVE_GUN_INCIDENTS = cumsum(GUN_INCIDENT),
         CUMULATIVE_GUN_VIOLENT_INCIDENTS = cumsum(GUN_VIOLENT_INCIDENT),
         CUMULATIVE_VIOLENT_INCIDENTS = cumsum(VIOLENT_INCIDENT),
         CUMULATIVE_AUTOBURG_INCIDENTS = cumsum(AUTOBURG_INCIDENT),
         CUMULATIVE_DV_INCIDENTS = cumsum(DV_INCIDENT),
         CUMULATIVE_AGG_VIOLENCE_INCIDENTS = cumsum(AGG_VIOLENCE_INCIDENT),
         CUMULATIVE_INCIDENTS = seq(from = 1, to = n()))


###############################################################################
##########                    CREATING NETWORKS                      ##########
###############################################################################


## Setting up subject graphs --------------------------------------------------
suspect_net <- all_data %>%
  select('UID', 'INCIDENT_NO') %>%
  na.omit() # This creates an edgelist to create igraph object


## Suspects and their incident graphs -----------------------------------------
suspect_incident_network <- suspect_net %>%
  as.data.frame() %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() # This creates a bipartite network of suspects and incidents


V(suspect_incident_network)$type <- bipartite.mapping(suspect_incident_network)$type # Create bipartite labels to be able to isolate the UID network

suspect_network <- bipartite.projection(suspect_incident_network)$proj1 # Create the unipartite graph of just UIDs

## Setting up large network graph of suspects, victims, witnesses, and incidents --
all_net <- all_data %>%
  select('UID', 'INCIDENT_NO') %>%
  na.omit()

## Suspects, victims, witnesses, and incidents --------------------------------
s_v_w_i_network <- all_net %>%
  as.data.frame() %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify() #Bipartite network of S, V, W, and I.


V(s_v_w_i_network)$type <- bipartite.mapping(s_v_w_i_network)$type # Create bipartite labels to be able to isolate the UID network

suspect_victim_network <- bipartite.projection(s_v_w_i_network)$proj1

## Grabbing names to set up labels on second graph ----------------------------
names <- as.data.frame(get.edgelist(s_v_w_i_network))$V1 %>%
  as.character() %>%
  unique()

## Persons w/o role and incidents network -------------------------------------
persons_net <- all_data %>%
  select('INCIDENT_NO', 'UID') %>%
  na.omit()

persons_net <- lapply(persons_net, as.numeric) %>%
  as.data.frame()

## Bipartite Network of suspects and incidnets --------------------------------
persons_network <- persons_net %>%
  as.data.frame() %>%
  graph_from_data_frame(directed = FALSE) %>%
  simplify()

## Dataframe of suspect attributes --------------------------------------------
suspect_attr <- all_data %>%
  select(UID, 
         TOTAL_INCIDENTS,
         TOTAL_PROPERTY,
         TOTAL_AUTOBURG,
         TOTAL_VIOLENT,
         TOTAL_DV,
         TOTAL_AGG_VIOLENCE,
         TOTAL_GUN,
         TOTAL_GUN_VIOLENT,
         COMMIT_AUTOBURG,
         COMMIT_VIOLENT,
         COMMIT_GUN,
         COMMIT_GUN_VIOLENT,
         COMMIT_PROPERTY,
         COMMIT_AGG_VIOLENCE,
         COMMIT_DV) %>%
  distinct(UID, .keep_all = TRUE)


## Dataframe of all incidents -------------------------------------------------
incident_attr <- all_data %>%
  select(UID,
         INCIDENT_NO,
         CRIME_TYPE,
         DATE)

## Dataframe for timevis that renames columns for incident_attr ---------------
timeline_data <- incident_attr %>%
  rename(start = DATE) %>%
  rename(content = INCIDENT_NO)
