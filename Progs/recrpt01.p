/* Program.................RecRpt01.p
   Notes:................. Posted Receipt by Income Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsCode LIKE dbRecH.rCode.
DEF VAR st-date AS DATE.
DEF VAR END-date AS DATE.
DEF VAR wsStatus AS DEC FORM "zzzzzzzzzzzzz9".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btncODE    LABEL "RECEIPT CODE".
DEF BUTTON btn-Export LABEL "Export".
DEF BUTTON btn-ok   LABEL "EXIT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF    QUERY qry-pickCode FOR dbRcod SCROLLING.
DEF BROWSE brw-pickCode QUERY qry-pickCode
        DISPLAY dbRCod.rCode dbRCod.Descrip
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickCode 
        brw-pickCode AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-exit colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Income Code Selection".

DEF FRAME frm-main
    SKIP(2.5)
    btnCode      NO-LABEL COLON 13 wsCode NO-LABEL dbRCod.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    st-date      LABEL "FROM DATE" COLON 30 SKIP(1)
    end-DATE     LABEL "TO DATE"  COLON 30 SKIP(1)
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Cashbook......." VIEW-AS TEXT
    btn-Export AT ROW 20.7 COL 20
     SPACE(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "POSTED RECEIPTS BY RECEIPTING CODE" VIEW-AS DIALOG-BOX.

ON CHOOSE OF btnCode IN FRAME frm-Main
DO:
  VIEW FRAME frm-pickCode.
  OPEN QUERY qry-PickCode FOR EACH dbRCod NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickCode.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickCode 
          OR close of THIS-PROCEDURE IN FRAME frm-pickCode
          OR CHOOSE OF btn-ok IN FRAME frm-pickCode 
          OR 'enter':u OF brw-PickCode
          OR 'mouse-select-dblclick' OF brw-PickCode.
  CLOSE QUERY qry-PickCode.
  HIDE FRAME frm-pickCode.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickCode 
    OR 'enter':u OF brw-PickCode
    OR 'mouse-select-dblclick' OF brw-PickCode
DO: 
   GET CURRENT qry-PickCode EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbRCod.rCode @ wsCode  WITH FRAME frm-Main.
   RETURN.
END.

ON 'tab':U OF wsCode IN FRAME frm-Main
    OR 'enter':U OF wsCode IN FRAME frm-Main
DO:
    FIND FIRST dbRCod WHERE dbRCod.rCode = wsCode:SCREEN-VALUE NO-LOCK NO-ERROR.  
    DISPLAY dbRCod.Descrip  WITH FRAME frm-Main.
    RETURN.
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   ASSIGN wsCode st-date end-date.
   OUTPUT STREAM a TO VALUE(wsFile).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM a CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN st-date:SCREEN-VALUE = STRING(TODAY)
       End-date:SCREEN-VALUE = STRING(TODAY)
       wsCode:SCREEN-VALUE = "0".
ASSIGN wsFile = simctr.repDir + "recRpt01" 
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
        
            
PROCEDURE Report.ip:
     EXPORT STREAM a DELIMITER ',' "INCOME COLLECTION FOR " +  dbRCod.Descrip 
         + " " + STRING(st-date) + " TO " +  STRING(END-date).
     EXPORT STREAM a.
    EXPORT STREAM a DELIMITER ',' "REC-DATE" "REC-NUMBER" "ACCOUNT" "PAYEE" "AMOUNT" "CURRENCY" "RATE".
    FOR EACH dbrech WHERE dbRecH.rCode = wscode AND dbRecH.RecDate >= st-date
                      AND dbRecH.RecDate <= end-date BREAK BY dbRecH.Account:
        DISPLAY dbRecH.RecNo @ wsStatus WITH FRAME frm-Main.
        PAUSE 0.
        EXPORT STREAM a DELIMITER ','  dbRecH.RecDate dbRecH.RecNo dbRecH.Account dbRecH.Descrip dbRecH.Amount
                dbRecH.txtCur dbRecH.decRate.
    END.
 END.
