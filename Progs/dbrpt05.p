/* Program.................dbrpt05.p
   Notes:................. Pick unbilled Accounts
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF STREAM a.
DEF VAR wsPer LIKE dbhtf.accper.
DEF BUFFER tt FOR dbhtf.
DEF VAR    wsstat LIKE dbcmf.dbacc.
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "EXPORT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wspER     LABEL "Accounts not billed by period" COLON 30
    HELP "List Accounts that where not billed as from the given period"
    SKIP(2.5)
    wsStat    LABEL "Processing...." COLON 30 VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    SPACE(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "CHECK FOR UNBILLED ACCOUNTS AS OF GIVEN PERIOD" VIEW-AS DIALOG-BOX.


/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsPer.
   OUTPUT STREAM a TO VALUE(wsFile).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM a CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
   APPLY 'close' TO THIS-PROCEDURE.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
ASSIGN wsPer:SCREEN-VALUE = STRING(CURPER).
ASSIGN wsFile = simctr.repDir + "Dbrpt05" 
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
    EXPORT STREAM a DELIMITER ',' "ACCOUNT" "NAME" "LAST-BILLED" "COMMENTS".
    FOR EACH dbcmf NO-LOCK:
        DISPLAY dbcmf.dbacc @ wsStat WITH FRAME frm-main.
        PAUSE 0.
        FIND LAST dbhtf WHERE prog = "dbsg07.p" AND dbhtf.dbacc = dbcmf.dbacc 
            AND dbhtf.accper >= wsPer NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbhtf THEN DO:
            FIND LAST tt WHERE tt.prog = "dbsg07.p" AND tt.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE tt THEN DO:
               EXPORT STREAM a DELIMITER ',' dbcmf.dbacc dbcmf.NAME tt.accper tt.ref.
            END.
            ELSE IF NOT AVAILABLE tt THEN DO:
                EXPORT STREAM a DELIMITER ',' dbcmf.dbacc dbcmf.NAME "" "Never Billied".
            END.
        END.  
    END.
END.
