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
DEF VAR wsstart LIKE  hsecmf.dbAcc.
DEF VAR wsend LIKE  hsecmf.dbAcc.
DEF VAR st-ward LIKE hsecmf.ward.
DEF VAR end-ward LIKE hsecmf.ward.
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
    st-sub      LABEL "Suburb FROM" COLON 30 SPACE(1)
    end-sub     LABEL "TO"  SKIP(0.5)
    st-ward     LABEL "Ward FROM" VIEW-AS FILL-IN SIZE 6 BY 1 COLON 30 SPACE(1)
    end-ward    LABEL "TO"  VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 
    
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
            DISABLE st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            APPLY 'tab' TO SELF.
        END.
        WHEN "W" THEN DO:
            DISABLE st-sub end-sub WITH FRAME frm-main.
            ENABLE st-ward end-ward WITH FRAME frm-main.
            APPLY 'entry' TO st-ward.
       END. 
       WHEN "S" THEN DO:
            DISABLE  st-ward end-ward  WITH FRAME frm-main.
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
   ASSIGN  wsStart wsEnd st-ward end-ward wsChoice st-sub end-sub.
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
ASSIGN st-sub:SCREEN-VALUE = "0"
       end-sub:SCREEN-VALUE = "999"
       st-ward:SCREEN-VALUE = "0"
       end-ward:SCREEN-VALUE = "99"
       wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsChoice:SCREEN-VALUE = "ALL".
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

PROCEDURE report.ip:
    IF wsSgrp = "" THEN DO:
        FOR EACH dbsgr:
            wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
        END.
    END.         
    EXPORT STREAM b DELIMITER ',' "ACOUNT" "NAME" "CELL" "AMOUNT".
    IF wsChoice = "W" THEN DO:  /* by ward */
        FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
           FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd 
                AND hsecmf.ward = dbwrd.ward AND hsecmf.AccStat = 0 AND hsecmf.cell[1] <> 0 AND hsecmf.amtdue[1] <> 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(hsecmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
                EXPORT STREAM b DELIMITER ',' hsecmf.dbacc hsecmf.NAME  hsecmf.cell Hsecmf.AmtDue[1].
            END.
        END.
    END.
    ELSE IF wsChoice = "S" THEN DO: /* by surbub */
        FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
            FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd 
                AND hsecmf.suburb = dbsmf.suburb AND hsecmf.AccStat = 0 AND hsecmf.cell[1] <> 0 AND hsecmf.amtdue[1] <> 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(hsecmf.dbacc).
               DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               EXPORT STREAM b DELIMITER ',' hsecmf.dbacc hsecmf.NAME  hsecmf.cell Hsecmf.AmtDue[1].
            END.
        END.
    END.
    ELSE DO:
        FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd AND hsecmf.AccStat = 0 AND hsecmf.cell[1] <> 0 AND hsecmf.amtdue[1] <> 0 USE-INDEX dbacc NO-LOCK:
           wsStatus = string(hsecmf.dbacc).
            DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
            EXPORT STREAM b DELIMITER ',' hsecmf.dbacc hsecmf.NAME  hsecmf.cell[1] Hsecmf.AmtDue[1].
        END.
    END.
END PROCEDURE.
