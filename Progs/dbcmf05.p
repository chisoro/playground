/* Program.................dbcmf05.p
   Notes:................. Masterfile listing Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.

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
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

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
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
   SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "MASTERFILE LISTING REPORT" VIEW-AS DIALOG-BOX.

FORM
    dbcmf.dbacc LABEL "ACCOUNT"
    dbcmf.Name  LABEL "ACCOUNT NAME"
    dbcmf.StandNo LABEL "STAND"
    dbcmf.AccBal  LABEL "BALANCE"
    HEADER skip(2) "         ACCOUNTS RECEVABLE MASTERFILE LISTING AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 264 FRAME frm-rpt.

/* ******** Triggers  ***********/
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
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice
            st-sub end-sub.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
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
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
    FOR EACH dbcmf NO-LOCK BREAK BY ward BY suburb BY dbacc:
        IF FIRST-OF(dbcmf.ward) THEN DO:
            FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-LOCK NO-ERROR.
            DISPLAY STREAM a "WARD:" + dbwrd.descrip @ dbAcc WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(dbcmf.suburb) THEN DO:
            FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
            DISPLAY STREAM  a "SUBURB:" + dbsmf.descrip @ NAME WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a dbacc Name StandNo AccBal WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        IF LAST-OF(dbcmf.ward) THEN DO:
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF LAST-OF(dbcmf.suburb) THEN DO:
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
    END.
END.
