/* Program.................dbmrpt1.p
   Notes:................. Meter consumption report
   Author:.................S. Mawire & S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF SHARED VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsAvg     AS DEC FORM "zzzzz9.99-". 
DEF VAR wsvarian  AS DEC FORM "zzzzz9.99-".
DEF VAR wsconsum  AS DEC FORM "zzzzz9.99-". 
DEF VAR wardTotal AS DEC.
DEF VAR subTotal AS DEC.
DEF VAR wsNo      AS INT. /* Count of months with meter readings */
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsStart   LIKE dbcmf.dbacc.
DEF VAR wsEnd     LIKE dbcmf.dbacc.
DEF VAR X AS INT.
DEF VAR t AS INT.
DEF VAR varReport     AS CHAR FORM "x(1)" INITIAL "W".
DEF VAR wsWardStart AS CHAR FORM "xxx" INITIAL "0".
DEF VAR wsWardEnd AS CHAR FORM "xxx" INITIAL "999".
DEF VAR wsSubStart AS CHAR FORM "xxxx" INITIAL "0".
DEF VAR wsSubEnd AS CHAR FORM "xxxx" INITIAL "9999".
DEF VAR wsStartAcc LIKE dbcmf.dbacc FORM "99999999999999" INITIAL 0.
DEF VAR wsEndAcc LIKE dbcmf.dbAcc FORM "99999999999999" INITIAL 999999999999.
DEF  VAR wsTitle AS CHAR  FORM "x(60)".
DEF VAR wsdev AS DEC FORM "zz9.99-".
DEF VAR wsopt AS INT INITIAL 1 VIEW-AS RADIO-SET 
             RADIO-BUTTONS "METER READING LIST",0,
                           "HIGH CONSUMPTION REPORT", 1,
                           "LOW CONSUMPTION REPORT",2, 
                           "METER TURNS REPORT", 3,
                            "All", 4.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEF BUFFER bfdbmmf FOR dbmmf.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEF FRAME frm-main
    SKIP(1.5)
    varReport   AT ROW 2.5 COL 5         LABEL "REPORT BY" AUTO-RETURN
               view-as combo-box size 20 by 2 LIST-ITEM-PAIRS
      "W - Ward","W","S - Surbub","S" SKIP(.5)
    "START" COLON 20 SPACE (30) "END" SKIP(.5)
    "WARD" COLON 10 SPACE(4) wsWardStart NO-LABEL SPACE (29) wsWardEnd NO-LABEL SKIP (1)
    "SURBUB" COLON 7 SPACE(5) wsSubStart NO-LABEL SPACE(28) wsSubEnd NO-LABEL SKIP(1)
    "ACCOUNT" COLON 6 SPACE (5) wsStartAcc NO-LABEL SPACE(19) wsEndAcc NO-LABEL
    btn-ok AT ROW 12.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "METER CONSUMPTION REPORT" VIEW-AS DIALOG-BOX.

FORM
    bfdbmmf.dbacc       LABEL "ACCOUNT" 
    dbcmf.NAME          LABEL "NAME"
    bfdbmmf.READ[12]    LABEL "PRE-READ"
    bfdbmmf.READ[13]    LABEL "CUR-READ"
    wsConsum            LABEL "CONSUMPTION"
    
    HEADER skip(4) wsTitle TODAY  "Page: " AT  72 PAGE-NUMBER(a)
     SKIP(3)
    WITH NO-BOX DOWN  STREAM-IO WIDTH 132 NO-LABELS FRAME frm-rpt1.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN varReport = varReport:SCREEN-VALUE.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.

   ON 'enter':U OF varReport IN FRAME frm-main
   OR  'LEAVE':U OF varReport IN FRAME frm-main
   DO:
       ASSIGN varReport.
       IF varReport:SCREEN-VALUE = "W" THEN DO:
           wsWardStart:SCREEN-VALUE = "0".
           wsWardEnd:SCREEN-VALUE = "9999".
           wsStartAcc:SCREEN-VALUE = "0".
           wsEndAcc:SCREEN-VALUE = "9999999999999".
           wsSubStart:SCREEN-VALUE = "".
           wsSubEnd:SCREEN-VALUE = "".
           ENABLE wsWardStart wsWardEnd WITH FRAME frm-main.
           DISABLE wsSubStart wsSubEnd  WITH FRAME frm-main.
           APPLY 'entry' TO wsWardStart.
           
       END.
       ELSE IF (varReport:SCREEN-VALUE = "S") THEN DO:
           wsSubStart:SCREEN-VALUE = "0".
           wsSubEnd:SCREEN-VALUE = "999".
           wsStartAcc:SCREEN-VALUE = "0".
           wsEndAcc:SCREEN-VALUE = "9999999999999".
           wsWardStart:SCREEN-VALUE = "".
           wsWardEnd:SCREEN-VALUE = "".
           ENABLE wsSubStart wsSubEnd WITH FRAME frm-main.
           DISABLE wsWardStart wsWardEnd  WITH FRAME frm-main. 
           APPLY 'entry' TO wsSubStart.
       END.
       RETURN NO-APPLY.
   END.



/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.

WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
  IF varReport = "W" THEN DO:
      wsTitle = "WATER CONSUMPTION REPORT BY WARD ".

    FOR EACH dbWrd  WHERE dbWrd.ward >= INT(wsWardStarT:SCREEN-VALUE IN FRAME frm-main)  AND dbWrd.ward <= INT(wsWardEnd:SCREEN-VALUE IN FRAME frm-main) NO-LOCK:
        wardTotal = 0.
        DISPLAY STREAM a dbWrd.descrip @ bfdbmmf.dbacc  WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.


            FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 NO-LOCK: /* Meter turns */
               FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc  AND dbcmf.ward = dbWrd.ward AND dbCmf.dbacc >= dec(wsStartAcc:SCREEN-VALUE IN FRAME frm-main) AND dbCmf.dbacc <= dec(wsEndAcc:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
               IF AVAILABLE dbcmf THEN DO: 
                   DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME bfdbmmf.read[12] bfdbmmf.read[13] bfdbmmf.consum[13] @ wsconsum
                                 WITH FRAME frm-rpt1.
                   DOWN STREAM a WITH FRAME frm-rpt1.
                   wardTotal = wardTotal + bfdbmmf.consum[13].
               END.
                                  
            END.


        DISPLAY STREAM a "Total" @ bfdbmmf.dbacc wardTotal @ wsconsum WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.
    END.
  END.
  ELSE IF varReport = "S"  THEN DO:
    wsTitle = "WATER CONSUMPTION REPORT BY SURBUB ".

    FOR EACH dbsmf  WHERE dbsmf.suburb >= INT(wsSubStarT:SCREEN-VALUE IN FRAME frm-main)  AND dbsmf.suburb <= INT(wsSubEnd:SCREEN-VALUE IN FRAME frm-main) NO-LOCK:
        wardTotal = 0.
        DISPLAY STREAM a dbsmf.descrip @ bfdbmmf.dbacc  WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.


            FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 NO-LOCK: /* Meter turns */
                FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc  AND dbcmf.ward = dbsmf.suburb AND dbCmf.dbacc >= dec(wsStartAcc:SCREEN-VALUE IN FRAME frm-main) AND dbCmf.dbacc <= dec(wsEndAcc:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
                IF AVAILABLE dbcmf THEN DO:
                    DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME bfdbmmf.read[12] bfdbmmf.read[13] bfdbmmf.consum[13] @ wsconsum
                                  WITH FRAME frm-rpt1.
                    DOWN STREAM a WITH FRAME frm-rpt1.
                    wardTotal = wardTotal + bfdbmmf.consum[13].
                END.
            END.


        DISPLAY STREAM a "Total" @ bfdbmmf.dbacc wardTotal @ wsconsum WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.
        DOWN STREAM a WITH FRAME frm-rpt1.
    END.
 END.
END PROCEDURE.
    
    
     
