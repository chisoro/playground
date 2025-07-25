/* Program.................hse24.p
   Notes:................. Landsale Analysis Report
   Author:.................S. Mawire
   Edited:.................S. Chisoro
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
DEF VAR st-sch  LIKE hsesch.scheme.
DEF VAR end-sch LIKE hsesch.scheme.
DEF VAR st-Date  LIKE hsecmf.pdate.
DEF VAR end-Date LIKE hsecmf.pdate.
DEF VAR wsChoice AS CHAR FORM "x(1)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsSer   AS CHAR FORM "X(40)".
DEF VAR curAmt AS DEC EXTENT 6 FORM "ZZZZZZZZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 5   FORM "ZZZZZZZZZ9.99-".
DEF VAR GTAmt AS DEC EXTENT 5   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC EXTENT 2 FORM "ZZZZZZZZZ9.99-". 
DEF VAR wsincome AS DEC.
DEF VAR st-fund LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR X AS INT.
DEF VAR wsper LIKE gltdf.per.
DEF VAR wsStatus AS CHAR.
DEF VAR wsThresh AS DEC.
DEF VAR wsZero AS LOGICAL.
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
     SIZE 95 by 12.5.

DEF FRAME frm-main
    SKIP(1.5)
    st-Date LABEL "Date FROM" COLON 30 SPACE(1)
    end-Date     LABEL "TO"  SKIP(0.5)
    wsChoice       LABEL "Report By?" COLON 30 
    view-as combo-box size 30 by 4 list-item-pairs
      "All - All","A","P - Scheme","P","W - By Ward","W","S - By Suburb","S"
    SKIP(0.5)
    st-sch      LABEL "Scheme FROM" COLON 30 SPACE(1)
    end-sch     LABEL "TO"  SKIP(0.5)
    st-sub      LABEL "Suburb FROM" COLON 30 SPACE(1)
    end-sub     LABEL "TO"  SKIP(0.5)
    st-ward     LABEL "Ward FROM" VIEW-AS FILL-IN SIZE 6 BY 1 COLON 30 SPACE(1)
    end-ward    LABEL "TO"  VIEW-AS FILL-IN SIZE 6 BY 1
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 14.7 COL 20
    space(25) btn-Export space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 14 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 17.5 KEEP-TAB-ORDER
    TITLE "LANDSALE ANALYSIS REPORT" VIEW-AS DIALOG-BOX.
FORM 
     hsecmf.dbAcc      LABEL "ACCOUNT" FORM "999999999"
     hsecmf.NAME       LABEL "NAME"      FORM "x(10)"
     hsecmf.standno       column-LABEL "STAND NUMBER" FORM "x(5)"
     hsesch.descrip        COLUMN-LABEL "SCHEME" FORM "x(5)"
     /*dbsmf.descrip        COLUMN-LABEL "SURBURB" FORM "x(10)"
     dbwrd.descrip        COLUMN-LABEL "WARD" FORM "x(10)"*/
     hsecmf.pdate        COLUMN-LABEL "PURCHASE DATE"
     hsecmf.pprice        COLUMN-LABEL "PURCHASE PRICE" FORM "-zzzzzzz9.99"
     hsecmf.damt        COLUMN-LABEL "DEPOSIT" FORM "-zzzzzzz9.99"
     hsecmf.InsAmt        COLUMN-LABEL "INSTALLMENT" FORM "-zzzzzzz9.99"
     hsecmf.bal        COLUMN-LABEL "CAPITAL BALANCE" FORM "-zzzzzzz9.99"
     hsecmf.AmtDue[1]        COLUMN-LABEL "REVENUE BALANCE" FORM "-zzzzzzz9.99"
    HEADER skip(1) "          LANDSALE ANALYSIS REPORT AS AT: " AT 10 SPACE(2)
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
            DISABLE st-sch end-sch st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            APPLY 'tab' TO SELF.
        END.
       WHEN  "P" THEN DO:    
            DISABLE   st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            ENABLE st-sch end-sch  WITH FRAME frm-main.
            APPLY 'entry' TO st-sch.
       END.
       WHEN "W" THEN DO:
            DISABLE  st-sch end-sch st-sub end-sub WITH FRAME frm-main.
            ENABLE st-ward end-ward WITH FRAME frm-main.
            APPLY 'entry' TO st-ward.
       END. 
       WHEN "S" THEN DO:
            DISABLE  st-sch end-sch st-ward end-ward  WITH FRAME frm-main.
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
    curAmt = 0.
    TAmt = 0.
   ASSIGN  st-sch end-sch  st-ward end-ward wsChoice 
            st-sub end-sub .
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   OUTPUT STREAM b TO VALUE(wsFile).
    curAmt = 0.
    TAmt = 0.
   ASSIGN  st-sch end-sch  st-ward end-ward wsChoice 
            st-sub end-sub .
    EXPORT STREAM b DELIMITER ',' "LANDSALE ANALYSIS REPORT".
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
       st-sch:SCREEN-VALUE = "0"
       end-sch:SCREEN-VALUE = "999999"
       st-ward:SCREEN-VALUE = "0"
       end-ward:SCREEN-VALUE = "99"
     wsChoice:SCREEN-VALUE = "ALL".
        wsFile = simctr.repDir + "hse25" 
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

IF wsChoice = "P" THEN DO:
    GTAmt = 0.
     IF wsbut = "Export" THEN
      EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME"  "STAND NUMBER" "SCHEME" "SURBURB" "WARD" 
         "PURCHASE DATE" "PURCHASE PRICE" "DEPOSIT" "INSTALLMENT"  "CAPITAL BALANCE" "REVENUE BALANCE" "PRATE".
    FOR EACH hsesch WHERE hsesch.scheme >= int(st-sch:SCREEN-VALUE IN FRAME frm-main) AND hsesch.scheme <= int(end-sch:SCREEN-VALUE IN FRAME frm-main) NO-LOCK:
        wsTitle = "          SCHEME: - " + hsesch.Descrip.
        
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "SCHEME: " + hsesch.Descrip.
        
        FOR EACH hsecmf WHERE hsecmf.pDate >= date(st-Date:SCREEN-VALUE IN FRAME frm-main) AND hsecmf.pDate <= date(end-Date:SCREEN-VALUE IN FRAME frm-main) 
            AND hsecmf.SCHEME = hsesch.scheme  USE-INDEX acc NO-LOCK:
            ASSIGN curAmt = 0
            wsStatus = string(hsecmf.dbacc).
           FIND FIRST dbwrd WHERE dbwrd.ward = hsecmf.ward NO-LOCK NO-ERROR.
           FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
           GTAmt[1] = GTAmt[1] + hsecmf.damt.
           GTAmt[2] = GTAmt[2] + hsecmf.InsAmt.
           GTAmt[3] = GTAmt[3] + hsecmf.pprice.
           GTAmt[4] = GTAmt[4] + hsecmf.bal.
           GTAmt[5] = GTAmt[5] + hsecmf.AmtDue[1].
          
           TAmt[1] = TAmt[1] + hsecmf.damt.
           TAmt[2] = TAmt[2] + hsecmf.InsAmt.
           TAmt[3] = TAmt[3] + hsecmf.pprice.
           TAmt[4] = TAmt[4] + hsecmf.bal.
           TAmt[5] = TAmt[5] + hsecmf.AmtDue[1].
           RUN idata.ip.
        END.
       UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         DISPLAY STREAM a "TOTAL" @ hsecmf.NAME TAmt[1] @ hsecmf.pprice TAmt[2] @ hsecmf.damt TAmt[3] @ hsecmf.InsAmt TAmt[4] @ hsecmf.bal TAmt[5] @ hsecmf.AmtDue[1]
                WITH FRAME frm-rpt.
         DOWN STREAM a WITH FRAME frm-rpt.
         UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         PAGE STREAM a.
         
    END.
     UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
     DISPLAY STREAM a "GRAND TOTAL" @ hsecmf.NAME GTAmt[1] @ hsecmf.pprice GTAmt[2] @ hsecmf.damt GTAmt[3] @ hsecmf.InsAmt GTAmt[4] @ hsecmf.bal GTAmt[5] @ hsecmf.AmtDue[1]
            WITH FRAME frm-rpt. 
      DOWN STREAM a WITH FRAME frm-rpt.
END.
ELSE IF wsChoice = "W" THEN DO:
    GTAmt = 0.
    IF wsbut = "Export" THEN
      EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME"  "STAND NUMBER" "SCHEME" "SURBURB" "WARD" "PURCHASE DATE" "PURCHASE PRICE" "DEPOSIT" "INSTALLMENT"  "CAPITAL BALANCE" "REVENUE BALANCE".
    FOR EACH dbwrd WHERE dbwrd.ward >= int(st-ward:SCREEN-VALUE IN FRAME frm-main) AND dbwrd.ward <= int(end-ward:SCREEN-VALUE IN FRAME frm-main) NO-LOCK:
        wsTitle = "          WARD: - " + dbwrd.Descrip.
        
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "WARD: " + dbwrd.Descrip.
        
        FOR EACH hsecmf WHERE hsecmf.pDate >= date(st-Date:SCREEN-VALUE IN FRAME frm-main) AND hsecmf.pDate <= date(end-Date:SCREEN-VALUE IN FRAME frm-main) 
            AND hsecmf.ward = dbwrd.ward  USE-INDEX acc NO-LOCK:
            ASSIGN curAmt = 0
            wsStatus = string(hsecmf.dbacc).
           FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
           FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
           GTAmt[1] = GTAmt[1] + hsecmf.damt.
           GTAmt[2] = GTAmt[2] + hsecmf.InsAmt.
           GTAmt[3] = GTAmt[3] + hsecmf.pprice.
           GTAmt[4] = GTAmt[4] + hsecmf.bal.
           GTAmt[5] = GTAmt[5] + hsecmf.AmtDue[1].

           TAmt[1] = TAmt[1] + hsecmf.damt.
           TAmt[2] = TAmt[2] + hsecmf.InsAmt.
           TAmt[3] = TAmt[3] + hsecmf.pprice.
           TAmt[4] = TAmt[4] + hsecmf.bal.
           TAmt[5] = TAmt[5] + hsecmf.AmtDue[1].
           RUN idata.ip.
        END.
         UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         DISPLAY STREAM a "TOTAL" @ hsecmf.NAME TAmt[1] @ hsecmf.pprice TAmt[2] @ hsecmf.damt TAmt[3] @ hsecmf.InsAmt TAmt[4] @ hsecmf.bal TAmt[5] @ hsecmf.AmtDue[1]
                WITH FRAME frm-rpt.
         DOWN STREAM a WITH FRAME frm-rpt.
         UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         PAGE STREAM a.
       
    END.
    UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
     DISPLAY STREAM a "GRAND TOTAL" @ hsecmf.NAME GTAmt[1] @ hsecmf.pprice GTAmt[2] @ hsecmf.damt GTAmt[3] @ hsecmf.InsAmt GTAmt[4] @ hsecmf.bal GTAmt[5] @ hsecmf.AmtDue[1]
            WITH FRAME frm-rpt. 
     DOWN STREAM a WITH FRAME frm-rpt.
END.
ELSE IF wsChoice = "S" THEN DO:
    GTAmt = 0.
    IF wsbut = "Export" THEN
      EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME"  "STAND NUMBER" "SCHEME" "SURBURB" "WARD" "PURCHASE DATE" "PURCHASE PRICE" "DEPOSIT" "INSTALLMENT" "CAPITAL BALANCE" "REVENUE BALANCE".
    FOR EACH dbsmf WHERE dbsmf.suburb >= int(st-sub:SCREEN-VALUE IN FRAME frm-main) AND dbsmf.suburb <= int(end-sub:SCREEN-VALUE IN FRAME frm-main) NO-LOCK:
        wsTitle = "          SURBURB: - " + dbsmf.Descrip.
        
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "SURBURB: " + dbSMF.Descrip.
        
        FOR EACH hsecmf WHERE hsecmf.pDate >= date(st-Date:SCREEN-VALUE IN FRAME frm-main) AND hsecmf.pDate <= date(end-Date:SCREEN-VALUE IN FRAME frm-main) 
            AND hsecmf.suburb = dbsmf.suburb  USE-INDEX acc NO-LOCK:
            ASSIGN curAmt = 0
            wsStatus = string(hsecmf.dbacc).
           FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
           FIND FIRST dbwrd WHERE dbwrd.ward = hsecmf.ward NO-LOCK NO-ERROR.
           GTAmt[1] = GTAmt[1] + hsecmf.damt.
           GTAmt[2] = GTAmt[2] + hsecmf.InsAmt.
           GTAmt[3] = GTAmt[3] + hsecmf.pprice.
           GTAmt[4] = GTAmt[4] + hsecmf.bal.
           GTAmt[5] = GTAmt[5] + hsecmf.AmtDue[1].

           TAmt[1] = TAmt[1] + hsecmf.damt.
           TAmt[2] = TAmt[2] + hsecmf.InsAmt.
           TAmt[3] = TAmt[3] + hsecmf.pprice.
           TAmt[4] = TAmt[4] + hsecmf.bal.
           TAmt[5] = TAmt[5] + hsecmf.AmtDue[1].
           RUN idata.ip.
        END.
         UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         DISPLAY STREAM a "TOTAL" @ hsecmf.NAME TAmt[1] @ hsecmf.pprice TAmt[2] @ hsecmf.damt TAmt[3] @ hsecmf.InsAmt TAmt[4] @ hsecmf.bal TAmt[5] @ hsecmf.AmtDue[1]
                WITH FRAME frm-rpt.
         DOWN STREAM a WITH FRAME frm-rpt.
         UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
         PAGE STREAM a.
       
    END.
    UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
     DISPLAY STREAM a "GRAND TOTAL" @ hsecmf.NAME GTAmt[1] @ hsecmf.pprice GTAmt[2] @ hsecmf.damt GTAmt[3] @ hsecmf.InsAmt GTAmt[4] @ hsecmf.bal GTAmt[5] @ hsecmf.AmtDue[1]
            WITH FRAME frm-rpt. 
      DOWN STREAM a WITH FRAME frm-rpt.
    
END.
ELSE DO:
    GTAmt = 0.
    IF wsbut = "Export" THEN
      EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME"  "STAND NUMBER" "SCHEME" "SURBURB" "WARD" "PURCHASE DATE"
         "PURCHASE PRICE" "DEPOSIT" "INSTALLMENT"  "CAPITAL BALANCE" "REVENUE BALANCE" "PERIOD" "LEDGER-AMT".
   
        ASSIGN TAmt = 0.
        
        FOR EACH hsecmf WHERE hsecmf.pDate >= date(st-Date:SCREEN-VALUE IN FRAME frm-main) AND hsecmf.pDate <= date(end-Date:SCREEN-VALUE IN FRAME frm-main) 
              USE-INDEX acc NO-LOCK:
            ASSIGN curAmt = 0
            wsStatus = string(hsecmf.dbacc).
           FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
           FIND FIRST dbwrd WHERE dbwrd.ward = hsecmf.ward NO-LOCK NO-ERROR.
           FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
           GTAmt[1] = GTAmt[1] + hsecmf.damt.
           GTAmt[2] = GTAmt[2] + hsecmf.InsAmt.
           GTAmt[3] = GTAmt[3] + hsecmf.pprice.
           GTAmt[4] = GTAmt[4] + hsecmf.bal.
           GTAmt[5] = GTAmt[5] + hsecmf.AmtDue[1].
           RUN idata.ip.
        END.
       
    UNDERLINE STREAM a hsecmf.NAME hsecmf.pprice hsecmf.damt  hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]
                    WITH FRAME frm-rpt.
     DISPLAY STREAM a "GRAND TOTAL" @ hsecmf.NAME GTAmt[1] @ hsecmf.pprice GTAmt[2] @ hsecmf.damt GTAmt[3] @ hsecmf.InsAmt GTAmt[4] @ hsecmf.bal GTAmt[5] @ hsecmf.AmtDue[1]
            WITH FRAME frm-rpt. 
      DOWN STREAM a WITH FRAME frm-rpt.
   APPLY 'close' TO SELF.
   RETURN.
END.
APPLY 'close' TO THIS-PROCEDURE.
END.

PROCEDURE idata.ip:
 IF wsBut = "Export" THEN DO:
     ASSIGN wsper = DEC(STRING(YEAR(hsecmf.PDate)) + STRING(MONTH(hsecmf.pDate),"99"))
            wsincome = ROUND((hsecmf.Pprice * Hsecmf.PRate * 100 / (hsecmf.vat + 100)),2).
     EXPORT STREAM b DELIMITER ',' hsecmf.dbAcc hsecmf.NAME hsecmf.standno hsesch.descrip dbsmf.descrip dbwrd.descrip hsecmf.pdate 
                        hsecmf.pprice hsecmf.damt hsecmf.InsAmt  hsecmf.bal hsecmf.AmtDue[1]  wsper wsincome.
 END.
    ELSE DO:
        DISPLAY STREAM a hsecmf.dbAcc hsecmf.NAME hsecmf.standno hsesch.descrip hsecmf.pdate hsecmf.pprice hsecmf.damt hsecmf.InsAmt hsecmf.bal hsecmf.AmtDue[1]
            WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
     END.

END.

