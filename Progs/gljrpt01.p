/* Program.................gljrpt01.p
   Notes:................. Posted Journals Report
   Author:.................S. Mawire
   Modified.................s. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "LANDSCAPE"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  glmf.acct.
DEF VAR wsend LIKE  glmf.acct.
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsAmt   AS DEC   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsVat   AS DEC  FORM "Z,ZZZ,ZZ9.99-".
DEF VAR st-per  LIKE gltdf.period.
DEF VAR end-per  LIKE gltdf.period.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF VAR wsDesc   AS CHAR.
DEF VAR wsBut AS CHAR.
DEF VAR wsFile AS CHAR FORM "x(40)".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-Export LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(2.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(1.5)
    st-per      LABEL "START Period" COLON 30 SPACE(1)
    end-per     LABEL "END Period"  SKIP(0.5)
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Export SPACE(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "GENERAL LEDGER POSTED JOURNAL REPORT" VIEW-AS DIALOG-BOX.

FORM 
     glmf.acct      LABEL "ACOUNT"
     glmf.Description       LABEL "ACCOUNT NAME" FORM "x(25)"
     gltdf.Ref        LABEL "REFERENCE"
     gltdf.trDate     LABEL "DATE"
     gltdf.DESCRIP    LABEL "NARRATION" FORM "x(20)"
     gltdf.Amt        LABEL "AMOUNT" FORM "ZZZZZZZZZ9.99-"
     gltdf.UID        LABEL "USER (I/U)" FORM "x(8)"
     HEADER skip(1) "       GENERAL LEDGER POSTED JOURNAL REPORT FOR THE PERIOD: " AT 10 SPACE(2)
    STRING(ST-PER) " TO " STRING(END-PER)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle FORM "X(80)"
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABELS NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   wsBut = "Print".
   ASSIGN wsStart wsEnd  st-per end-per.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   ASSIGN wsStart wsEnd  st-per end-per.
   OUTPUT STREAM b TO VALUE(wsFile).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-Per:SCREEN-VALUE = string(SIMCTR.CURPER).
ASSIGN wsFile = simctr.repDir + "GLJrpt01" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv". 
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE report.ip:
    IF wsBut = "Export" THEN DO:
        EXPORT STREAM b DELIMITER ',' "GENERAL LEDGER POSTED JOURNALS REPORT FOR THE PERIOD: " + STRING(st-per) + " TO " + STRING(end-per). 
        EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "REFERENCE" "DATE" "DESCRIPTION" "AMOUNT" "USER (I/U)".
    END.
   /* FOR EACH gltdf WHERE gltdf.acct >= wsStart AND gltdf.acct <= wsEnd
                       AND gltdf.period >= int(st-per) AND gltdf.period <= int(end-per) 
                       AND SUBSTRING(ref, index(ref,".") + 1)<> "p" BY gltdf.TransID BY gltdf.ref:
        */
    FOR EACH gltdf WHERE gltdf.acct >= wsStart AND gltdf.acct <= wsEnd
                        AND gltdf.period >= int(st-per) AND gltdf.period <= int(end-per) 
                        AND gltdf.SOURCE = "GL" BY gltdf.TransID BY gltdf.ref:
        wsStatus = STRING(gltdf.acct).
        DISPLAY wsStatus WITH FRAME frm-main.
        FIND FIRST glmf WHERE glmf.acct = gltdf.acct  NO-LOCK NO-ERROR.
        IF wsBut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' glmf.acct glmf.Description gltdf.Ref gltdf.trDate 
                gltdf.DESCRIP gltdf.Amt "'" + STRING(gltdf.UID) + "/" + STRING(gltdf.UID2).
        ELSE DO:
            DISPLAY STREAM a glmf.acct glmf.Description gltdf.Ref gltdf.trDate 
                     gltdf.DESCRIP gltdf.Amt STRING(gltdf.UID) + "/" + STRING(gltdf.UID2) @ gltdf.UID WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        
   END.
   APPLY 'ENTRY' TO btn-exit.
END.
   
