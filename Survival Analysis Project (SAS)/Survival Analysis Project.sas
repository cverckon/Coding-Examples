/* Number of Missing Values for Each Variable */
PROC IMPORT OUT = proj
	DATAFILE = '/home/u64301181/BS 852/Project/project.2025.csv'
	DBMS = CSV
	REPLACE;
	GETNAMES = YES;
RUN;

OPTIONS validvarname = any;

/* change names so . does not interfere with future variable calls */
DATA proj;
	SET proj;
	log_il6 = log.il6;
	Age_last_contact = Age.last.contact;
	Age_enrollment = Age.enrollment;
	log_new_hscrp = log.new.hscrp;
	Z_grip_strength = Z.grip.strength;
	Z_bmi = Z.bmi;
	Z_gait_speed = Z.gait.speed;
	Z_fev1_7 = 'Z.fev1.7'n;
	Z_sysbp = Z.sysbp;
RUN;

OPTIONS validvarname = any;

DATA proj;
	SET proj;
	drop 'log.il6'n
		'Age.enrollment'n
		'Age.last.contact'n
		'log.new.hscrp'n
		'Z.grip.strength'n
		'Z.bmi'n
		'Z.gait.speed'n
		'Z.fev1.7'n
		'Z.sysbp'n;
RUN;

PROC FORMAT;
	VALUE $missfmt ' ' = 'Missing' other = 'Not Missing';
	VALUE  missfmt  .  = 'Missing' other = 'Not Missing';
RUN;


PROC FREQ DATA = proj;
	FORMAT _CHAR_ $missfmt.;
	TABLES _CHAR_ / missing missprint nocum nopercent;
	FORMAT _NUMERIC_ missfmt.;
	TABLES _NUMERIC_ / missing missprint nocum nopercent;
RUN;

/* REMOVE INSTANCES WERE ALIVE VARIABLE IS MISSING */


DATA projc;
	SET proj;
	if missing(Alive) then delete;
	if Alive = '' then delete;
RUN;

DATA projc;
	SET proj;
	IF missing(Age_last_contact) THEN delete;
RUN;

/* CREATE TIME VARIABLE */

DATA projc;
	SET projc;
	TIME = Age_last_contact - Age_enrollment;
RUN;

/* CREATE CENSOR VARIABLE */

DATA projc;
	set projc;
	if Alive = 'Yes' then EVENT = 1;
	else EVENT = 0;
run;

/* CREATE CATEGORICAL VARIABLE FOR PROC LIFETEST TO EVALUATE FOR IL6 */

PROC RANK DATA = projc groups = 2 out = projc;
	VAR log_il6;
	RANKS il6_lowhigh;
RUN;

*
ODS SELECT SurvivalPlot;
PROC LIFETEST DATA = projc;
	TIME TIME * EVENT(0);
	STRATA il6_lowhigh;
RUN;
*
ODS SELECT all;

/* REPEAT LAST STEP FOR HSCRP VARIABLE */

PROC RANK DATA = projc groups = 2 out = projc;
	VAR log_new_hscrp;
	RANKS hscrp_lowhigh;
RUN;

*
ODS SELECT SurvivalPlot;


PROC LIFETEST DATA = projc;
	TIME TIME * EVENT(0);
	STRATA hscrp_lowhigh;
RUN;

*
ODS SELECT all;

/* remove missing*/

DATA projc;
	SET projc;
	if missing(Alive) then delete;
	if Alive = '' then delete;
RUN;

DATA projc;
	SET projc;
	IF missing(Age_last_contact) THEN delete;
RUN;

DATA projc;
	SET projc;
	IF missing(educ) THEN delete;
RUN;

DATA projc;
	SET projc;
	IF missing(Z_grip_strength) THEN delete;
RUN;

DATA projc;
	SET projc;
	IF missing(Z_bmi) THEN delete;
RUN;

DATA projc;
	SET projc;
	IF missing(Z_gait_speed) THEN delete;
RUN;

PROC FREQ DATA = projc;
	FORMAT _CHAR_ $missfmt.;
	TABLES _CHAR_ / missing missprint nocum nopercent;
	FORMAT _NUMERIC_ missfmt.;
	TABLES _NUMERIC_ / missing missprint nocum nopercent;
RUN;

/* generate summary information */


PROC MEANS N MEAN MISSING MIN MAX;
VAR sex log_il6 educ Z_grip_strength Z_bmi Z_gait_speed Z_sysbp;
RUN;


/* CREATE CRUDE MODEL AND RUN TESTS */

PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = log_il6 xlog_il6 / TIES = EFRON rl;
	xlog_il6 = log_il6*TIME;
RUN;

PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = log_il6 / TIES = EFRON rl;
RUN;

/* CREATE FULL MODEL AND TEST FOR PH */
PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = sex log_il6 educ Z_grip_strength Z_bmi Z_gait_speed Z_sysbp xlog_il6 xeduc xZ_grip_strength xZ_bmi xZ_gait_speed xZ_sysbp / TIES = EFRON rl;
	xlog_il6 = TIME*log_il6;
	xeduc = TIME*educ;
	xZ_grip_strength = TIME*Z_grip_strength;
	xZ_bmi = TIME*Z_bmi;
	xZ_gait_speed = TIME*Z_gait_speed;
	xZ_sysbp = TIME*Z_sysbp;
RUN;

/* TEST FULL MODEL FOR ASSOCIATION */

PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = sex log_il6 educ Z_grip_strength Z_bmi Z_gait_speed Z_sysbp / TIES = EFRON rl;
RUN;

/* TEST FOR INTERACTION */
PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = sex log_il6 educ Z_grip_strength Z_bmi Z_gait_speed Z_sysbp log_il6*Z_grip_strength log_il6*educ / TIES = EFRON rl;
RUN;

/* RERUN INTERACTION */
PROC PHREG DATA = projc;
	MODEL TIME * EVENT(0) = sex log_il6 educ Z_grip_strength Z_bmi Z_gait_speed Z_sysbp log_il6*educ / TIES = EFRON rl;
	HAZARDRATIO 'HR: 1 unit difference' log_il6 / units = 1;
RUN;