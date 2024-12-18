PROC IMPORT OUT=baggage
    DATAFILE="/home/u63069202/Time Series Analysis/Project/baggagecomplaints.csv"
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN;

data baggage;
    set work.baggage;
    where airline ne 'Hawaiian';
run;

data work.baggage;
    set work.baggage;
    by airline;
    if first.airline then do;
        dbaggage = .; /* Initialize the first difference for each airline */
        prev_baggage = .; /* Initialize variable to hold previous baggage value */
    end;

    if first.airline then output; /* Output only when it's the first observation for each airline */
    else do;
        dbaggage = dif(baggage); /* Calculate the first difference using the DIF function */
        cube_root_dbaggage = sign(dbaggage) * abs(dbaggage) ** (1/3); /* Take the cube root of the first difference */
		departed_passengers = Enplaned/(Scheduled - Cancelled); /* departed passengers per flight */    
       output;
    end;

    prev_baggage = baggage; /* Store current baggage value for the next iteration */
run;

data American United;
    set work.baggage;
    if airline = "American Eagle" then output American;
    else if airline = "United" then output United;
run;

proc sgplot data=baggage;
	series x=date y=baggage / group = airline;
run;

proc sgplot data=baggage;
	series x=date y=dbaggage / group = airline;
run;

proc sgplot data=baggage;
	series x=date y=cube_root_dbaggage / group = airline;
run;

/* CCF  */

data baggage_indicator;
    set baggage;
    if airline = 'United' then united = 1;
    else united = 0;
    if airline = 'American Eagle' then american = 1;
    else american = 0;
run;

proc arima data=baggage_indicator;
identify var=cube_root_dbaggage crosscor=(united american) nlag=21 scan;
estimate p=(11 12) q=(12) noint;
run;

/* only american  */

PROC AUTOREG DATA=american PLOTS(ONLY)= (ACF WN PACF);
			MODEL cube_root_dbaggage = / partial NLAG=21 method=ml backstep ;
RUN;

PROC ARIMA DATA=american;
	IDENTIFY VAR=cube_root_dbaggage SCAN ;
	RUN;
	
PROC ARIMA DATA=american;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(1 12) Q=(12) PRINTALL PLOT; /* found through autoreg */
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=american;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=1 Q=0 PRINTALL PLOT; /* found through scan */
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=american;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(1 11 12) Q=(12) PRINTALL PLOT; /* found through autoreg plots*/
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=american;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(11 12) Q=(12) PRINTALL PLOT; /* found through autoreg sig. */
	FORECAST LEAD=60;
RUN;

/* only united  */

PROC AUTOREG DATA=united PLOTS(ONLY)= (ACF WN PACF);
			MODEL cube_root_dbaggage = / partial NLAG=21 method=ml backstep ;
RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage SCAN ;
	RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(1 12) Q=(5 11 12) PRINTALL PLOT; /* found through autoreg */
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=2 Q=2 PRINTALL PLOT; /* found through scan */
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(1 11 12) Q=(12) PRINTALL PLOT; /* found through autoreg plots*/
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(11 12) Q=(12) PRINTALL PLOT; /* found through autoreg sig. */
	FORECAST LEAD=60;
RUN;

PROC ARIMA DATA=united;
	IDENTIFY VAR=cube_root_dbaggage nlag=21;
	ESTIMATE P=(11 12) Q=(5 12) PRINTALL PLOT; /* found through autoreg AIC */
	FORECAST LEAD=60;
RUN;