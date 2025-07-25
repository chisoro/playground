/* Program.................hsevast.p
   Notes:................. Vacant Stands Report
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
DEF VAR wsstart LIKE  hsecmf.dbAcc.
DEF VAR wsend LIKE  hsecmf.dbAcc.
DEF VAR st-ward LIKE hsecmf.ward.
DEF VAR end-ward LIKE hsecmf.ward.
DEF VAR st-sub LIKE dbsmf.suburb.
DEF VAR end-sub LIKE dbsmf.suburb.
DEF VAR wsChoice AS CHAR FORM "x(1)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsSer   AS CHAR FORM "X(40)".
DEF VAR curAmt AS DEC EXTENT 6 FORM "ZZZZZZZZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 6   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC EXTENT 2 FORM "ZZZZZZZZZ9.99-".
DEF VAR st-fund LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF  VAR wsFilter LIKE hsesch.scheme.
DEF VAR wsBut AS CHAR   FORM "x(8)".
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF BUTTON btn-Export LABEL "Export".
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
    st-sub      LABEL "Suburb FROM" COLON 30 SPACE(1)
    end-sub     LABEL "TO"  SKIP(0.5)
    st-ward     LABEL "Ward FROM" VIEW-AS FILL-IN SIZE 6 BY 1 COLON 30 SPACE(1)
    end-ward    LABEL "TO"  VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
   
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Export space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "VACANT STANDS REPORT" VIEW-AS DIALOG-BOX.
FORM 
     hsecmf.dbAcc      LABEL "ACCOUNT"
     hsesch.descrip       LABEL "SCHEME"      FORM "x(25)"
     hsecmf.StandNo        LABEL "STAND NUMBER"
     dbsmf.descrip        LABEL "SURBUB"
     dbwrd.descrip        LABEL "WARD"
     hsecmf.SIZE        LABEL "STAND SIZE"
     HEADER skip(1) "          VACANT STANDS REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/


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
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   wsBut = "Print".
    
   ASSIGN  wsStart wsEnd st-ward end-ward wsChoice
            st-sub end-sub.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   OUTPUT STREAM b TO VALUE(wsFile).
   ASSIGN  wsStart wsEnd st-ward end-ward wsChoice  
            st-sub end-sub.
    EXPORT STREAM b DELIMITER ',' "VACANT STANDS REPORT".
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
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
        wsFile = simctr.repDir + "vacantstand" 
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
IF wsChoice = "W" THEN DO:
    IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "SCHEME" "STAND NUMBER"  "SURBUB"  "WARD" "STAND SIZE".
    FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
        wsTitle = "          WARD: - " + dbwrd.Descrip.
        
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "WARD: " + dbwrd.Descrip.
        FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd 
            AND hsecmf.ward = dbwrd.ward AND hsecmf.AccStat <> 1 USE-INDEX acc NO-LOCK:
            wsStatus = string(hsecmf.dbacc).
           RUN idata.ip.
        END.
           DOWN STREAM a WITH FRAME frm-rpt.
           PAGE STREAM a.
        END.
END.
ELSE IF wsChoice = "S" THEN DO:
    IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "SCHEME" "STAND NUMBER"  "SURBUB"  "WARD" "STAND SIZE".
    FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
        wsTitle = "          SUBURB: - " + dbsmf.Descrip.
        
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "SUBURB: " + dbsmf.Descrip.
        FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd 
            AND hsecmf.suburb = dbsmf.suburb AND hsecmf.AccStat <> 1 USE-INDEX acc NO-LOCK:
           wsStatus = string(hsecmf.dbacc).
           RUN idata.ip.
        END.
           DOWN STREAM a WITH FRAME frm-rpt.
           PAGE STREAM a.
   END.
END.
ELSE DO:
   ASSIGN 
          wsTitle = "    SERVICE: " + wsSer.
  IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "SCHEME" "STAND NUMBER"  "SURBUB"  "WARD" "STAND SIZE".
   FOR EACH hsecmf WHERE hsecmf.dbAcc >= wsStart AND hsecmf.dbAcc <= wsEnd 
       AND hsecmf.AccStat <> 1 USE-INDEX acc NO-LOCK:
       wsStatus = string(hsecmf.dbacc).
       RUN idata.ip.
   END.
   DOWN STREAM a WITH FRAME frm-rpt.
   END.
   APPLY 'close' TO SELF.
   APPLY 'close' TO THIS-PROCEDURE.
RETURN.
END.

PROCEDURE idata.ip:
FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme. 
FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb.
FIND FIRST dbwrd WHERE dbwrd.ward = hsecmf.ward. 
DISPLAY wsStatus WITH FRAME frm-main.
    IF wsBut = "Export" THEN
        EXPORT STREAM b DELIMITER ',' hsecmf.dbacc hsesch.descrip hsecmf.standno  dbsmf.descrip dbwrd.descrip hsecmf.SIZE.
    ELSE DO:
         DISPLAY STREAM a hsecmf.dbacc hsesch.descrip hsecmf.standno  dbsmf.descrip dbwrd.descrip hsecmf.SIZE
            WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
     END.

END.
