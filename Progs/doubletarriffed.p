/* Program.................emailsend.p
   Notes:................. Send Email
   Author:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR st-ward LIKE dbcmf.ward.
DEF VAR end-ward LIKE dbcmf.ward.
DEF VAR st-con LIKE dbcmf.cons.
DEF VAR end-con LIKE dbcmf.cons.
DEF VAR st-sub LIKE dbsmf.suburb.
DEF VAR end-sub LIKE dbsmf.suburb.
DEF VAR wsChoice AS CHAR FORM "x(1)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsa     AS DEC EXTENT 5 FORM "ZZZZZZZZZ9.99-".
DEF VAR wsbf    AS DEC.
DEF VAR wsStatus AS CHAR.
DEF VAR wsBut AS CHAR   FORM "x(8)".
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsFile1 AS CHAR FORM "x(40)".
DEF VAR wsFile2 AS CHAR FORM "x(40)".
DEF VAR wsSgrp AS CHAR FORM "x(50)".
DEF VAR X AS INT.
DEF VAR m AS INT.
DEF VAR j AS INT.
DEF VAR wsAccper AS INT.
DEF VAR wsDescrip AS CHAR.
DEF VAR wsCur AS DEC.
DEF VAR    ws30Day AS DEC.
DEF VAR    ws60Day AS DEC.
DEF VAR   ws90Day AS DEC.
DEF VAR wsAmnt AS DEC.
DEF VAR varAmt AS DEC.
DEF VAR wsCredit AS DEC.
DEF VAR     varVat AS DEC.
DEF TEMP-TABLE tmpwork LIKE dbmtf.
DEF VAR wsAccDate AS DATE.
DEF VAR wsDueDate AS DATE.
DEF VAR wsMessage1 AS CHAR FORM "X(80)" .
DEF VAR wsMessage2 AS CHAR FORM "X(80)".
DEF VAR wsMessage3 AS CHAR FORM "X(80)".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 18.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsChoice       LABEL "Report By?" COLON 30 
    view-as combo-box size 30 by 4 list-item-pairs
      "All - All Accounts","A","C - By Consumer","C","W - By Ward","W","S - By Suburb","S"
    SKIP(0.5)
    st-Con      LABEL "Consumer FROM" COLON 30 VIEW-AS FILL-IN SIZE 6 BY 1 SPACE(1)
    end-Con     LABEL "TO" VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    st-sub      LABEL "Suburb FROM" COLON 30 SPACE(1)
    end-sub     LABEL "TO"  SKIP(0.5)
    st-ward     LABEL "Ward FROM" VIEW-AS FILL-IN SIZE 6 BY 1 COLON 30 SPACE(1)
    end-ward    LABEL "TO"  VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 
    "Enter service codes, comma seperated,  blank for ALL" COLON 30 SKIP
    wsSgrp     COLON 30 LABEL "Services"  SKIP(0.5)
    wsAccDate  LABEL "Accounting Date" COLON 30 SPACE(10)
    wsDueDate  LABEL "Last Receipt Date"                   SKIP(0.5)
     wsmessage1 LABEL "STATEMENT" COLON 15 SKIP          
     wsmessage2 NO-LABEL        COLON 15 SKIP(0.5)
     wsmessage3 LABEL "EMAIL" COLON 15 SKIP 
    SKIP(0.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-Ok AT ROW 20.7 COL 20
     space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 105 BY 24 KEEP-TAB-ORDER
    TITLE "EMAIL STATEMENTS" VIEW-AS DIALOG-BOX.


ON 'tab':U OF wsChoice 
    OR 'enter' OF wsChoice
DO:
    ASSIGN wsChoice = wsChoice:SCREEN-VALUE.
    CASE wsChoice:
        WHEN "A" THEN DO:
            DISABLE st-con end-con st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            APPLY 'tab' TO SELF.
        END.
        WHEN "C" THEN DO:
            DISABLE st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            ENABLE st-con end-con WITH FRAME frm-main.
            APPLY 'entry' TO st-con.
        END.     
       WHEN "W" THEN DO:
            DISABLE st-con end-con st-sub end-sub WITH FRAME frm-main.
            ENABLE st-ward end-ward WITH FRAME frm-main.
            APPLY 'entry' TO st-ward.
       END. 
       WHEN "S" THEN DO:
            DISABLE st-con end-con st-ward end-ward  WITH FRAME frm-main.
            ENABLE st-sub end-sub  WITH FRAME frm-main.
            APPLY 'entry' TO st-sub.
       END. 
    END.
    RETURN.
END.

ON 'choose':U OF btn-Ok  
DO:
   session:set-wait-state("").
   OUTPUT STREAM b TO VALUE(wsFile).
   OUTPUT STREAM a TO VALUE(wsFile1).
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice st-sub end-sub wsSgrp.
   RUN Report.ip.
   wsFile = "START " + wsFile.
   wsFile1 = "START " + wsFile1.
   OUTPUT STREAM b CLOSE.
   OUTPUT STREAM a CLOSE.
   /*OS-COMMAND NO-WAIT VALUE(wsFile2).*/
  /* OS-COMMAND NO-WAIT VALUE(wsFile2 + "bin\sendmail\sendmail.exe").*/
   APPLY 'close' TO THIS-PROCEDURE.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN st-con:SCREEN-VALUE = "0"
       end-con:SCREEN-VALUE = "99"
       st-sub:SCREEN-VALUE = "0"
       end-sub:SCREEN-VALUE = "999"
       st-ward:SCREEN-VALUE = "0"
       end-ward:SCREEN-VALUE = "99"
       wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsChoice:SCREEN-VALUE = "ALL"
       wsSgrp:SCREEN-VALUE = "".
wsFile = simctr.repDir + "DoubleTarif.csv".
wsFile1 = simctr.repDir + "Static.csv".
wsAccPer = simctr.curper.
FILE-INFO:FILE-NAME = ".".
/*wsFile2 = FILE-INFO:FULL-PATHNAME + "\Progs\sendmail.bat".*/
wsFile2 =  substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)).
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE DataExtract-ip:
    wsa = 0.
    wsbf = 0.
    wsCur = 0.
    ws30Day = 0.
    ws60Day = 0.
    ws90Day = 0.
    wsCredit = 0.
    DO X = 1 TO NUM-ENTRIES(wsSgrp):
       j = INT(ENTRY(x,wsSgrp)). 
       FOR EACH dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc AND dbmtf.sgrp = j:
            ASSIGN wsa[2] = wsa[2] + amt WHEN prog = "dbsg07.p" OR  prog = "dbint.p"
                   wsa[3] = wsa[3] + amt WHEN prog = "recupd.p"
                   wsa[4] = wsa[4] + amt WHEN prog = "dbjnl02.p".
       END.
       
       FIND FIRST dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.sgrp = j NO-LOCK NO-ERROR.
       IF AVAILABLE dbblf THEN DO:
            assign
                wsBf = wsbf + dbblf.dbamt[1]
                wsCredit = wsCredit + dbblf.amt[1] WHEN dbblf.amt[1] < 0
                wsCur = wsCur + dbblf.amt[1]  WHEN dbblf.amt[1] > 0
                ws30Day = ws30Day + dbblf.amt[2] + dbblf.INT
                ws60Day = ws60Day + dbblf.amt[3]
                ws90Day = ws90Day + dbblf.amt[4] + dbblf.amt[5] + dbblf.amt[6] + dbblf.amt[7] + dbblf.amt[8] + dbblf.amt[9] + dbblf.amt[10] + dbblf.amt[11] + dbblf.amt[12] + dbblf.amt[13] + dbblf.amt[14] + dbblf.amt[15].
        END.
                     
    END.
 IF wsBf = ? THEN
     wsBf = 0.
    EXPORT STREAM b DELIMITER ','  dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell "Balance CF" wsbf wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
    DO m = 1 TO 4:
        ASSIGN 
            wsDescrip = "Current" WHEN m = 1
            wsDescrip = "30-Days" WHEN m = 2
            wsDescrip = "60-Days" WHEN m = 3
            wsDescrip = "Credit" WHEN m = 4
            wsAmnt = wsCur WHEN m = 1
            wsAmnt = ws30Day WHEN m = 2
            wsAmnt = ws60Day WHEN m = 3
            wsAmnt = wsCredit WHEN m = 4.
             IF wsAmnt = ? THEN
                wsAmnt = 0.
            EXPORT STREAM b DELIMITER ','  dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main  wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell wsDescrip wsAmnt wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
    END.
    wsDescrip = "90-days+".
    wsAmnt = ws90Day.
     IF wsAmnt = ? THEN
                wsAmnt = 0.
   EXPORT STREAM b DELIMITER ','  dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main  wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell wsDescrip wsAmnt wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
  varVat = 0.
    RUN tmpwork-ip.
    RUN rep-ip.
    wsAmnt =  dbcmf.BalBf.
     IF wsAmnt = ? THEN
                wsAmnt = 0.
   EXPORT STREAM b DELIMITER ','  dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main  wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell "Balance BF" wsAmnt wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
   
END PROCEDURE.


PROCEDURE report.ip:
    IF wsSgrp = "" THEN DO:
        FOR EACH dbsgr:
            wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
        END.
    END.         
    EXPORT STREAM b DELIMITER ',' "Account"    "Due Date"	"Accounting Date"	"Period" 	"Name"	"Address1"	"Address2"	"Address3"	"Stand Number"	"Street"	"Email" "Cell"	"ITEM"	"AMOUNT"	"Message1"	"Message2".
.
    IF wsChoice = "C" THEN DO: /* by Consumer */
        FOR EACH dbctf WHERE dbctf.cons >= st-Con AND dbctf.cons <= end-Con NO-LOCK.
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.cons = dbctf.cons AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
               DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "W" THEN DO:  /* by ward */
        FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
           FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.ward = dbwrd.ward AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "S" THEN DO: /* by surbub */
        FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.suburb = dbsmf.suburb AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE DO:
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
           AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK: /* AND dbcmf.cell <> 0*/
           wsStatus = string(dbcmf.dbacc).
            DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
            RUN DataExtract-ip.
        END.
    END.
    FIND FIRST simctr NO-LOCK NO-ERROR.
    FIND FIRST simctr1 NO-LOCK NO-ERROR.
   EXPORT STREAM a DELIMITER ','  SIMCTR.CONAME SIMCTR.Add1 SIMCTR.add2 SIMCTR.Add3   SIMCTR.REGNO  SIMCTR.Phone wsMessage3:SCREEN-VALUE IN FRAME frm-main simctr1.serveradd simctr1.serverport simctr1.serverpass simctr1.serveremail.

END PROCEDURE.



PROCEDURE rep-ip:
    FIND FIRST tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
    IF AVAILABLE tmpwork THEN DO:
       varAmt = tmpwork.Amt.
       varVat = varVat + tmpwork.Vat.
       wsDescrip = string(tmpwork.trDate) + " " +  tmpwork.ref  + "  "  + upper(tmpwork.DESCRIP)  .
        IF varAmt = ? THEN
            varAmt = 0.
       EXPORT STREAM b DELIMITER ',' dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main  wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell wsDescrip   varAmt   wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
     
    END.
    REPEAT:
        FIND NEXT tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE tmpwork THEN DO:
            varAmt = tmpwork.Amt.
            varVat = varVat + tmpwork.Vat.
           wsDescrip = string(tmpwork.trDate) + " " +  tmpwork.ref  + "  "  + upper(tmpwork.DESCRIP)  .
            EXPORT STREAM b DELIMITER ',' dbcmf.dbacc wsDueDate :SCREEN-VALUE IN FRAME frm-main wsAccDate:SCREEN-VALUE IN FRAME frm-main  wsAccPer  dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.street dbcmf.StandNo dbcmf.emailAdd dbcmf.Cell wsDescrip   varAmt   wsMessage1:SCREEN-VALUE IN FRAME frm-main wsMessage2:SCREEN-VALUE IN FRAME frm-main.
     END.   
        IF NOT AVAILABLE tmpwork THEN LEAVE.
    END.
END PROCEDURE.

PROCEDURE tmpwork-ip: /*consolidate Receipts and Journals */
    FOR EACH tmpwork.
      DELETE tmpwork.
    END.
    FOR EACH dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc NO-LOCK:
        IF dbmtf.prog = "dbint.p" THEN DO: /* Interest */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbint.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Interest".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "dbjnl02.p" THEN DO: /* Journals */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbjnl02.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Adjusting Journals".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "recupd.p" THEN DO: /* Receipts */
           FIND FIRST tmpwork WHERE tmpwork.prog = "recupd.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Receipts".
           END.
           ELSE ASSIGN tmpwork.amt = tmpwork.amt + dbmtf.amt
                       /* tmpwork.descrip = wsmeter WHEN dbmtf.ref  = dbmmf.serial*/.
        END.
        ELSE DO:
           CREATE tmpwork.
           BUFFER-COPY dbmtf TO tmpwork.
        END.
    END.
END PROCEDURE
