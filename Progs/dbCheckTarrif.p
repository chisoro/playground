
/* Program.................dbCheckTarrif.p
   Notes:................. Check for accounts with double tarrif set
   Author:.................S. Chisoro
*/

DEF VAR sTar AS CHAR.
DEF VAR X AS INTEGER.
DEF VAR n AS INTEGER.
DEF VAR WSA AS CHAR.
DEF VAR i AS INT.
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF STREAM c.
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
DEF VAR wsSgrp AS CHAR FORM "x(50)".
DEF VAR wsStatus AS CHAR.

DEF VAR wsFile3 AS CHAR FORM "x(40)".

DEF TEMP-TABLE tmpDTarrif no-undo
    FIELD dbAcc  LIKE dbcmf.dbAcc
    FIELD tarif LIKE dbtmf.tarif.

DEF TEMP-TABLE tmpConsum no-undo
    FIELD dbAcc  LIKE dbcmf.dbAcc.


DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 25.5.

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
    wsEnd       LABEL "End Account"  COLON 30 SKIP 
    SKIP(0.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-Ok AT ROW 27.6 COL 20
     space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 27 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 105 BY 30 KEEP-TAB-ORDER
    TITLE "CHECK DUPLICATE TARRIFS" VIEW-AS DIALOG-BOX.

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
   OUTPUT STREAM a TO VALUE(wsFile3).
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice st-sub end-sub.
   RUN checktar.ip.
   wsFile3 = "START " + wsFile3.
   OUTPUT STREAM a CLOSE.
   MESSAGE "Please correct the following DOUBLE TARRIFED ACCOUNTS before billing!" VIEW-AS ALERT-BOX.
   OS-COMMAND NO-WAIT VALUE(wsFile3).
  APPLY 'close' TO THIS-PROCEDURE.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST simctr1 NO-LOCK NO-ERROR.
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
       wsChoice:SCREEN-VALUE = "ALL".
wsFile3 = simctr.repDir + "doubleTarrif.csv".
FILE-INFO:FILE-NAME = ".".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE checktar.ip:
    EXPORT STREAM a DELIMITER ',' "Account"    "Tarrif".
    IF wsChoice = "C" THEN DO: /* by Consumer */
        FOR EACH dbctf WHERE dbctf.cons >= st-Con AND dbctf.cons <= end-Con NO-LOCK.
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.cons = dbctf.cons AND dbcmf.AccStat = 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
               DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN dtExtract.ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "W" THEN DO:  /* by ward */
        FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
           FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.ward = dbwrd.ward AND dbcmf.AccStat = 0 USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN dtExtract.ip.
            END.
        END.
    END.
    ELSE IF wsChoice = "S" THEN DO: /* by surbub */
        FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
            FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
                AND dbcmf.suburb = dbsmf.suburb AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK:
               wsStatus = string(dbcmf.dbacc).
                DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
               RUN dtExtract.ip.
            END.
        END.
    END.
    ELSE DO:
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
           AND dbcmf.AccStat = 0  USE-INDEX dbacc NO-LOCK: /* AND dbcmf.cell <> 0*/
           wsStatus = string(dbcmf.dbacc).
            DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
            RUN dtExtract.ip.
        END.
    END.
    RUN writetoFile.ip.
END PROCEDURE.

PROCEDURE dtExtract.ip:
     sTar = "".
    DO X = 1 TO 14:
         IF sTar <> "" THEN DO:
            IF dbcmf.tarif[X] <> 0  THEN DO:
                 sTar = sTar + "," + STRING(dbcmf.tarif[X]).
            END.
       END.
       IF sTar = "" THEN DO:
                IF dbcmf.tarif[X] <> 0 THEN DO:
                    sTar = string(dbcmf.tarif[X]).
                END.
        END.
   END.

    FOR EACH dbtmf WHERE dbtmf.type = 3:
            n = 0.
            DO i = 1 to num-entries(sTar):
                wsa = STRING(dbtmf.tarif).
                IF (entry(i,sTar)) = wsa THEN DO:
                    n = n + 1.
                END.
            END.
            IF n > 1 THEN DO:
                CREATE tmpDTarrif.
                ASSIGN
                     tmpDTarrif.dbAcc = dbcmf.dbAcc
                     tmpDTarrif.tarif = dbtmf.tarif.
            END.
      END.
END PROCEDURE.

PROCEDURE writetoFile.ip:
    FOR EACH tmpDTarrif:
        EXPORT STREAM a DELIMITER ',' tmpDTarrif.dbAcc    tmpDTarrif.tarif.
    END.
END PROCEDURE.
/*
FOR EACH dbcmf WHERE dbacc =  65016411010:
   
   
  END.
FOR EACH dbcmf:
    wsStatus = string(dbcmf.dbacc).
    DISPLAY wsStatus WITH FRAME frm-main. PAUSE 0.
    FIND FIRST tmpDTarrif  WHERE tmpDTarrif.dbAcc = dbcmf.dbAcc NO-ERROR.
    IF AVAILABLE  tmpDTarrif THEN NEXT.
    IF NOT AVAILABLE tmpDTarrif THEN DO:
        CREATE  tmpConsum.
        ASSIGN
            tmpConsum.dbacc = dbcmf.dbacc.
    END.
END.
.*/
