
/* Program.................dbage.p
   Notes:................. Age Analysis Report
   Author:.................S. Mawire
*/

&SCOPED-DEFINE w-orientation           "PORTRAIT" 

session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsStatus AS CHAR.
DEF VAR wsSgrp   AS CHAR FORM "x(30)".
DEF VAR curAmt AS DEC EXTENT 6 FORM "ZZZZZZZZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 6   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC  FORM "ZZZZZZZZZ9.99-".
DEF VAR wsThresh AS DEC.
DEF VAR wsZero AS LOGICAL.
DEF VAR j AS INT.
DEF VAR wsdes LIKE dbcmf.NAME.
DEF VAR X AS INT.

DEF BUTTON btn-Exp LABEL "Export".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

FORM 
     dbcmf.dbAcc      LABEL "ACCOUNT"
     DBcmf.NAME       LABEL "NAME"      FORM "x(25)"
     DBcmf.StandNo    LABEL "STAND"
     DBcmf.stno       LABEL "STREET NO" 
     dbsmf.descrip    LABEL "SUBURB"    FORM "x(20)"
     wsAmt            LABEL "BALANCE" 
    HEADER skip(1) "         DEBTORS CUT-OFF LIST AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

DEF FRAME frm-main
    SKIP(1.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    "Enter service codes, comma seperated" COLON 30 SKIP
    wsSgrp     COLON 30 LABEL "SERVICE" 
    SPACE(1) "(Leave blank for all Services)"  SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
    wsThresh      LABEL "Minimun Balance" COLON 30
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Exp space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "DEBTORS CUT-OFF ANALYSIS REPORT" VIEW-AS DIALOG-BOX.

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").       
    wsFile = simctr.repDir + "dbcut" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".pdf".
   ASSIGN  wsStart wsEnd wsSgrp wsZero wsThresh.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="rpt-ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-exp  
DO:
  session:set-wait-state("").         
   wsFile = simctr.repDir + "dbcut" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
   ASSIGN  wsStart wsEnd wsSgrp wsZero wsThresh.
   OUTPUT STREAM b TO VALUE(wsFile).
   RUN Rep-exp.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST tblforex WHERE tblforex.txtcur = simctr.dbCur NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE   = "999999999998"
       wsSgrp:SCREEN-VALUE  = ""
       wsZero:SCREEN-VALUE  = "NO"
       wsthresh:SCREEN-VALUE = "0.00".

WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE rpt-ip:
    IF wsSgrp = "" THEN DO:
        FOR EACH dbsgr:
            wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
        END.
    END.
    FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd:
        DISPLAY dbcmf.dbacc @ wsstatus WITH FRAME frm-main.
    PAUSE 0.
        wsAmt = 0.
        FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsdes = dbsmf.descrip.
        ELSE wsdes = "Invalid Suburb ********".
        DO X = 1 TO NUM-ENTRIES(wsSgrp):
           j = INT(ENTRY(x,wsSgrp)).
           FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.Sgrp = j NO-LOCK:
               wsAmt  = wsAmt  + dbblf.dbamt[1] + dbblf.Int.
           END.
        END.
        IF wsAmt = 0 AND wsZero = NO THEN NEXT.
        ELSE IF wsAmt < wsThresh AND wsThresh <> 0.00 THEN NEXT.
        ELSE DO:
            DISPLAY STREAM a dbcmf.dbacc  StandNo stno wsdes @ dbsmf.descrip wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END. 
    END.
END PROCEDURE.

PROCEDURE rep-exp:
IF wsSgrp = "" THEN DO:
    FOR EACH dbsgr:
        wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
    END.
END.
EXPORT STREAM b DELIMITER ',' "DEBTORS AGE ANALYSIS REPORT FOR  SERVICES: " wsSgrp.
EXPORT STREAM b DELIMITER ',' "".
EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CELL-PHONE" "STREET NO." "SUBURB" "ADDRESS1" "ADDRESS2" "ADDRESS3"
                              "CURRENT"  "30-DAYS"  "60-DAYS" "90-DAYS" "120+ DAYS" "TOTAL" "RTGS VALUE".
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd:
    DISPLAY dbcmf.dbacc @ wsstatus WITH FRAME frm-main.
    PAUSE 0.
    curAmt = 0.
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-ERROR.
    IF AVAILABLE dbsmf THEN
        wsdes = dbsmf.descrip.
    ELSE wsdes = "Invalid Suburb ********".
    DO X = 1 TO NUM-ENTRIES(wsSgrp):
       j = INT(ENTRY(x,wsSgrp)).
       FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.Sgrp = j NO-LOCK:
             DISPLAY wsStatus WITH FRAME frm-main.
             ASSIGN curAmt[1] = curAmt[1] + Amt[1] + Amt[2]
                    curAmt[2] = curAmt[2] + Amt[3]
                    curAmt[3] = curAmt[3] + Amt[4] + dbblf.Int
                    curAmt[4] = curAmt[4] + Amt[5]
                    curAmt[5] = curAmt[5] + Amt[6] + Amt[7] + Amt[8] + Amt[9] +  Amt[10] + Amt[11] 
                              + Amt[12] + Amt[13] + Amt[14] + Amt[15].                
       END.
     END.
    ASSIGN curAmt[6] = CurAmt[1] + CurAmt[2] + CurAmt[3] + CurAmt[4] + CurAmt[5].
    IF curAmt[6] = 0 AND wsZero = NO THEN NEXT.
    ELSE IF curAmt[6] < wsThresh AND wsThresh <> 0.00 THEN NEXT.
    ELSE
    EXPORT STREAM b DELIMITER ',' dbcmf.dbacc dbcmf.NAME dbcmf.cell stno street wsdes  Add1 add2 Add3 
        CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6] ROUND((CurAmt[6] * tblforex.decRate),2) .
END.
END PROCEDURE.
