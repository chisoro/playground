/* Program.................dbrpt04.p
   Notes:................. Tarif Distribution STATISTICS
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation     "PORTRAIT"
&SCOPED-DEFINE pagesize            66
DEF STREAM a.
DEF STREAM b.
DEF NEW SHARED VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsTitle  AS CHAR FORM "X(60)".
DEF VAR wsTitle0  AS CHAR FORM "X(60)".
DEF VAR     wsPer LIKE dbhtf.accper.
DEF VAR    wsstat LIKE dbcmf.dbacc.
DEF VAR    wsFile AS CHAR FORM "x(40)".
DEF VAR X AS INT.

DEF BUFFER tt FOR dbhtf.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "PRINT".

DEF TEMP-TABLE dbtemp
    FIELD wsWard  LIKE dbwrd.Ward
    FIELD wsdbacc LIKE dbcmf.dbacc
    FIELD wstarif AS   INT
    FIELD wsunit  AS   DEC
    FIELD wsAmt   LIKE dbcmf.bal FORM "zzz,zzz,zz9.99-"
    FIELD wstype AS INT.

FORM dbcmf.NAME  LABEL "ACCOUNT NAME"
     dbtemp.wsdbacc LABEL "ACCOUNT"
     dbtemp.wsTarif LABEL "TARIF"
     dbtemp.wsUnit  LABEL "UNITS"
     dbtemp.wsAmt   LABEL "VALUE"
     WITH DOWN STREAM-IO NO-LABEL CENTERED NO-BOX FRAME a.

FORM dbcmf.NAME  AT 10 NO-LABEL
     dbtemp.wsAmt   LABEL "VALUE"
     dbtemp.wsUnit  LABEL "UNITS"
     HEADER SKIP(4) wsTitle0 AT 20 SKIP(1) wsTitle AT 20 skip(2) "TARIF DISTRIBUTION STATISTICS" AT 5
     "Page:" AT 61 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-LABEL NO-BOX FRAME B.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wsStat    LABEL "Processing...." COLON 30 VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    SPACE(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "Tarif Distribution Statistics" VIEW-AS DIALOG-BOX.


/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged}
   APPLY 'close' TO THIS-PROCEDURE.
END.

/*ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   OUTPUT STREAM b TO VALUE(wsFile).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
   APPLY 'close' TO THIS-PROCEDURE.
END. */

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
wsTitle0 = simctr.coname.
ASSIGN wsFile = simctr.repDir + "Dbrpt04" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv". 
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
    /* create temp-table */
    FOR EACH dbcmf:
        DISPLAY dbcmf.dbacc @ wsStat WITH FRAME frm-main.
        PAUSE 0.
        FIND FIRST dbwrd WHERE dbwrd.Ward = dbcmf.ward NO-LOCK NO-ERROR.
        DO X = 1 TO 14:
            IF dbcmf.tarif[X] <> 0 AND dbcmf.unit[X] <> 0 THEN DO:
              FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
              CREATE dbtemp.
              ASSIGN dbtemp.wsWard = dbwrd.Ward
                     dbtemp.wsdbacc = dbcmf.dbacc
                     dbtemp.wsTarif = dbcmf.tarif[X]
                     dbtemp.wsUnit  = dbcmf.unit[X]
                     dbtemp.wsAmt   = ROUND((dbtmf.Charge * dbcmf.unit[X]),2)
                     dbtemp.wstype  = 3.
            END. 
        END.
    END.
    /* Output Listing */
/*    FOR EACH dbtemp NO-LOCK BREAK BY wsWard BY wsTarif :
        FIND FIRST dbcmf WHERE dbcmf.dbacc = dbtemp.wsdbacc NO-LOCK NO-ERROR.
         EXPORT STREAM b DELIMITER "," wsWard wsdbAcc dbcmf.NAME wsTarif wsUnit wsAmt.
    END.
    */
    /* Statistical Analysis */
    FOR EACH dbtemp NO-LOCK BREAK BY wsWard BY wsTarif :
       ACCUMULATE wsAmt (SUB-TOTAL BY wsTarif).
       ACCUMULATE wsAmt (SUB-TOTAL BY wsWard).
       ACCUMULATE wsUnit (SUB-TOTAL BY wsTarif).
       ACCUMULATE wsUnit (SUB-TOTAL BY wsWard).
       IF FIRST-OF(wsWard) THEN DO:
           FIND FIRST dbwrd WHERE dbwrd.ward = dbtemp.wsward NO-LOCK NO-ERROR.
           DISPLAY STREAM a "WARD:- " + dbwrd.descrip @ dbcmf.NAME WITH FRAME b.
           DOWN STREAM a WITH FRAME b.
       END.
       IF LAST-OF(wsTarif) THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.tarif = dbtemp.wstarif AND dbtmf.TYPE = dbtemp.wstype NO-LOCK NO-ERROR.
           DISPLAY STREAM a "       " + dbtmf.descrip @ dbcmf.NAME 
                            ACCUM SUB-TOTAL BY wsTarif (wsAmt) @ dbtemp.wsAmt
                            ACCUM SUB-TOTAL BY wsTarif (wsUnit)@ dbtemp.wsUnit  WITH FRAME b.
           DOWN STREAM a WITH FRAME b.
       END.  
       IF LAST-OF(wsWard) THEN DO:
           UNDERLINE STREAM a dbcmf.NAME dbtemp.wsAmt dbtemp.wsUnit WITH FRAME b.
           DOWN STREAM a WITH FRAME b.
           DISPLAY STREAM a "WARD TOTALS" @ dbcmf.NAME 
                            ACCUM SUB-TOTAL BY wsWard (wsAmt) @ dbtemp.wsAmt
                            ACCUM SUB-TOTAL BY wsWard (wsUnit)@ dbtemp.wsUnit  WITH FRAME b.
           DOWN STREAM a WITH FRAME b.
           UNDERLINE STREAM a dbtemp.wsAmt dbtemp.wsUnit WITH FRAME b.
           DOWN 2 STREAM a WITH FRAME b.
       END. 
    END. 
    /*Tarif summary */
    PAGE STREAM a.
    DISPLAY STREAM a "All WARDS" @ dbcmf.NAME WITH FRAME b.
    DOWN STREAM a WITH FRAME b.
    FOR EACH dbtemp NO-LOCK BREAK BY wsTarif :
       ACCUMULATE wsAmt (SUB-TOTAL BY wsTarif).
       ACCUMULATE wsUnit (SUB-TOTAL BY wsTarif).
       IF LAST-OF(wsTarif) THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.tarif = dbtemp.wstarif AND dbtmf.TYPE = dbtemp.wstype NO-LOCK NO-ERROR.
           DISPLAY STREAM a "       " + dbtmf.descrip @ dbcmf.NAME 
                            ACCUM SUB-TOTAL BY wsTarif (wsAmt) @ dbtemp.wsAmt
                            ACCUM SUB-TOTAL BY wsTarif (wsUnit)@ dbtemp.wsUnit  WITH FRAME b.
           DOWN STREAM a WITH FRAME b.
       END.  
    END.
END.
