/* Program.................kadsms.p
   Notes:................. SMS Extract
   Author:.................S. Mawire
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
DEF VAR wsSgrp AS CHAR FORM "x(50)".
DEF VAR X AS IN.
DEF VAR j AS INT.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

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
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-Ok AT ROW 20.7 COL 20
     space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "SMS FILE EXTRACTION" VIEW-AS DIALOG-BOX.


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
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice st-sub end-sub wsSgrp.
   RUN Report.ip.
   wsFile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
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
wsFile = simctr.repDir + "Sms" 
          + string(day(today),"99")
          + string(month(today),"99")
          + string(year(today),"9999")
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE DataExtract-ip:
    wsa = 0.
    wsbf = 0.
    DO X = 1 TO NUM-ENTRIES(wsSgrp):
       j = INT(ENTRY(x,wsSgrp)). 
       FOR EACH dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc AND dbmtf.sgrp = j:
            ASSIGN wsa[2] = wsa[2] + amt WHEN prog = "dbsg07.p" OR  prog = "dbint.p"
                   wsa[3] = wsa[3] + amt WHEN prog = "recupd.p"
                   wsa[4] = wsa[4] + amt WHEN prog = "dbjnl02.p".
       END.
       FIND FIRST dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.sgrp = j NO-LOCK NO-ERROR.
       IF AVAILABLE dbblf THEN
            wsBf = wsbf + dbblf.dbamt[1].
    END.
   wsbf = wsbf - wsa[2] - wsa[3] - wsa[4].
   wsa[5] = wsbf + wsa[2] + wsa[3] + wsa[4].
   IF wsa[5] <> 0 THEN
       EXPORT STREAM b DELIMITER ',' dbcmf.NAME dbcmf.dbacc dbcmf.cell wsbf  wsa[2]  wsa[3] wsa[4] wsa[5].
END PROCEDURE.


PROCEDURE report.ip:
    IF wsSgrp = "" THEN DO:
        FOR EACH dbsgr:
            wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
        END.
    END.         
    EXPORT STREAM b DELIMITER ',' "NAME" "AACOUNT" "CELL" "BFBAL" "BILLED" "RECEIPTS" "JOUNAL" "BALANCE".
    IF wsChoice = "C" THEN DO: /* by Consumer */
        FOR EACH dbctf WHERE dbctf.cons >= st-Con AND dbctf.cons <= end-Con NO-LOCK.
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.cons = dbctf.cons AND dbcmf.AccStat = 0 AND dbcmf.cell <> 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
               DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "W" THEN DO:  /* by ward */
        FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
           FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.ward = dbwrd.ward AND dbcmf.AccStat = 0 AND dbcmf.cell <> 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "S" THEN DO: /* by surbub */
        FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.suburb = dbsmf.suburb AND dbcmf.AccStat = 0 AND dbcmf.cell <> 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN DataExtract-ip.
            END.
        END.
    END.
    ELSE DO:
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
           AND dbcmf.AccStat = 0 AND dbcmf.cell <> 0 USE-INDEX dbacc NO-LOCK:
           wsStatus = string(dbcmf.dbacc).
            DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
            RUN DataExtract-ip.
        END.
    END.
END PROCEDURE.
