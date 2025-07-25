/* Program.................hse21.p
   Notes:................. Posted Tranasaction Report
   Author:.................S. Chisoro
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR wsVar    AS CHAR FORM "x(20)".
DEF VAR wsDet AS CHAR FORM "x(30)".
DEF VAR wsAcc LIKE hsehtf.dbAcc.
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR  wsFile AS CHAR FORM "x(40)".
DEF VAR  startdate LIKE hsehtf.trdate.
DEF VAR ENDDate LIKE hsehtf.trdate.
DEF VAR txtCur LIKE hsesch.txtCur EXTENT 2.
DEF VAR amountDue LIKE hsecmf.amtDue EXTENT 2.
DEF VAR xRate AS DEC.
DEF VAR balbf LIKE hsehtf.AMT.
DEF VAR rBal LIKE hsehtf.AMT.
DEF BUTTON btn-print   LABEL "Print".
DEF BUTTON btn-close     LABEL "Close".
DEF BUTTON btn-exp    LABEL "Export".
DEF BUTTON btnclose     LABEL "Close".
DEF BUTTON btn-ok    LABEL "OK".
define variable hCol as widget-handle.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsSearch   LIKE hsecmf.NAME.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btnAcc LABEL "ACCOUNT".

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

FORM 
    hsehtf.accper COLUMN-LABEL "PERIOD" AT 10
    hsehtf.trdate COLUMN-LABEL "TRANSACTION!DATE"
    hsehtf.ref COLUMN-LABEL "REFERENCE"
    hsehtf.descrip COLUMN-LABEL "DESCRIPTION" FORM "x(40)"
    hsehtf.amt COLUMN-LABEL "AMOUNT"
    rBal COLUMN-LABEL "BALANCE"
    HEADER  skip(1) wsTitle AT 10  skip(1) wsDet AT 10 "   TRANSACTION LISTING FROM " SPACE(2)
    STRING(STARTDATE) space(1) " TO " space(1) STRING(ENDDATE) SKIP(1)
    "AMOUNT DUE.:" AT 10 SPACE(10) txtCur[1] FORM "x(3)" amountDue[1] SPACE(3) "/" txtCur[2] FORM "x(3)" amountDue[2]  SPACE (10) "(Exchange Rate @ "  xRate 
    "/1)" SKIP(1)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

define frame frm-main
    SKIP(1)
    btnAcc     COLON 10 hsecmf.dbAcc NO-LABEL  hsecmf.NAME NO-LABEL VIEW-AS TEXT SKIP(0.5)
    startdate  COLON 22 LABEL "Start Date" SKIP(0.5)
    endDate    COLON 22 LABEL "End Date" 
    btn-Print at row 8.6 COL 5 SPACE(20) btn-Exp SPACE(20) btn-close SKIP(1)
   WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
   TITLE "POSTED TRANSACTIONS".


DEF    QUERY qry-hsecmf FOR hsecmf SCROLLING.
DEF BROWSE brw-hsecmf QUERY qry-hsecmf
    DISPLAY hsecmf.dbAcc hsecmf.Name hsecmf.StandNo wsVar LABEL "Scheme" 
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
   brw-hsecmf AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".



/*triggers*/
ON 'choose':U OF btn-EXP IN FRAME frm-main 
DO:
    session:set-wait-state("").
    wsFile = simctr.repDir + "PTR" 
             + string(day(today),"99")    + "_"
             + string(month(today),"99")  + "_"
             + string(year(today),"9999") + "_"
             + substr(string(time,"HH:MM:SS"),1,2)
             + substr(string(time,"HH:MM:SS"),4,2)
             + substr(string(time,"HH:MM:SS"),7,2)
             + ".csv".    
      OUTPUT STREAM b TO VALUE(wsFile).
      
      wsfile = "START " + wsFile.
      
    StartDate = DATE(StartDate:SCREEN-VALUE).
    EndDate = DATE(EndDate:SCREEN-VALUE).
    wsAcc = Integer(hsecmf.dbacc:SCREEN-VALUE).
    FIND FIRST hsecmf WHERE hsecmf.dbacc = wsAcc.
    wsDet = hsecmf.NAME + " " + "(" + STRING(hsecmf.dbacc) + ")".
    EXPORT STREAM b DELIMITER ',' wsTitle.
    EXPORT STREAM b DELIMITER ','  wsDet + "   TRANSACTION LISTING FROM " + string(Startdate) + " TO " + string(enddate).
    RUN Expo.ip.  
    OUTPUT STREAM b CLOSE.
    OS-COMMAND NO-WAIT VALUE(wsFile).
    RETURN.
END.

ON 'choose':U OF btn-print IN FRAME frm-main 
DO:
    session:set-wait-state("").
    StartDate = DATE(StartDate:SCREEN-VALUE).
    EndDate = DATE(EndDate:SCREEN-VALUE).
    wsAcc = Integer(hsecmf.dbacc:SCREEN-VALUE).
    FIND FIRST hsecmf WHERE hsecmf.dbacc = wsAcc.
    wsDet = hsecmf.NAME + " " + "(" + STRING(hsecmf.dbacc) + ")".
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged}
    RETURN.
END.

ON  'enter':u OF hsecmf.dbacc IN FRAME  frm-main
     OR 'tab':u OF hsecmf.dbacc IN FRAME  frm-main
DO:
   FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(hsecmf.dbacc:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsecmf THEN DO:
      MESSAGE "Account does not exist, please try again" VIEW-AS ALERT-BOX.
      hsecmf.NAME:SCREEN-VALUE = "".
      hsecmf.dbacc:SCREEN-VALUE = "".
      RETURN NO-APPLY.
   END.
   ELSE DO:
      DISPLAY hsecmf.Name WITH FRAME frm-main.
      APPLY 'TAB' TO startdate.
    END.
   RETURN.    
END.

ON CHOOSE OF btnAcc IN FRAME frm-main
DO:
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-hsecmf FOR EACH hsecmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-hsecmf
          OR 'mouse-select-dblclick' OF brw-hsecmf.
  CLOSE QUERY qry-hsecmf.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO hsecmf.dbAcc.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-hsecmf 
    OR 'mouse-select-dblclick' OF brw-hsecmf 
DO: 
   GET CURRENT qry-hsecmf  NO-LOCK NO-WAIT.
    wsAcc = hsecmf.dbacc.
   DISPLAY  hsecmf.dbacc WITH FRAME frm-main.
   RETURN.
END.


ON row-display OF brw-hsecmf DO:
    FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
    wsVar = hsesch.Descrip.
END.

on 'start-search':u of browse brw-hsecmf
    run Search.ip.

ON 'choose':U OF btnSearch IN FRAME Search-opt 
    OR 'enter':U OF btnSearch IN FRAME Search-opt
    OR 'tab':U OF btnSearch IN FRAME Search-opt
DO:
    open query qry-hsecmf preselect 
                    each hsecmf no-lock 
                        where hsecmf.SortName >= wsSearch:SCREEN-VALUE
                           BY hsecmf.Sortname.
 END.


/********** MAIN LOGIC **********/
FIND FIRST simctr.
wsTitle = simctr.COName.
ENABLE ALL  WITH FRAME frm-main.
browse brw-hsecmf:allow-column-searching = true.
WAIT-FOR CHOOSE OF btn-Close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE   report.ip.
  FIND FIRST hsecmf WHERE hsecmf.dbacc = wsAcc.
  FIND FIRST hsesch WHERE hsecmf.scheme = hsecmf.scheme.
  FIND FIRST tblForex WHERE tblForex.txt = hsesch.txtCur.
  txtCur[1] = hsesch.txtCur.
  xRate = tblForex.DecRate.
  amountDue[1] = hsecmf.AmtDue[1].
  amountDue[2] = hsecmf.AmtDue[1] * tblForex.DecRate.
  FIND FIRST tblForex WHERE decRate = 1.
  txtCur[2] = tblForex.txtCur.
  balbf = 0.
  rBal = 0.
  FOR EACH hsehtf WHERE hsehtf.dbacc = wsAcc AND hsehtf.trdate < startdate:
      balbf = balbf + hsehtf.amt.
  END.
  DISPLAY STREAM a  "Balance B/F" @ hsehtf.trdate balbf @ rbal WITH FRAME frm-rpt.
      DOWN STREAM a WITH FRAME frm-rpt.
  rbal = balbf.
  FOR EACH hsehtf WHERE hsehtf.dbacc = wsAcc AND hsehtf.trdate >= startdate AND hsehtf.trdate <=enddate:
      rbal = rbal + hsehtf.amt.
      DISPLAY STREAM a  hsehtf.accper hsehtf.trdate hsehtf.REF hsehtf.descrip hsehtf.amt rbal WITH FRAME frm-rpt.
      DOWN STREAM a WITH FRAME frm-rpt.
   END.
END.

PROCEDURE   EXPO.ip.
  FIND FIRST hsecmf WHERE hsecmf.dbacc = wsAcc.
  FIND FIRST hsesch WHERE hsecmf.scheme = hsecmf.scheme.
  FIND FIRST tblForex WHERE tblForex.txt = hsesch.txtCur.
  txtCur[1] = hsesch.txtCur.
  xRate = tblForex.DecRate.
  amountDue[1] = hsecmf.AmtDue[1].
  amountDue[2] = hsecmf.AmtDue[1] * tblForex.DecRate.
  FIND FIRST tblForex WHERE decRate = 1.
  txtCur[2] = tblForex.txtCur.
  balbf = 0.
  rBal = 0.
  EXPORT STREAM b DELIMITER ',' "" "" "" "AMOUNT DUE" txtCur[1] + STRING(amountDue[1]) txtCur[2] + STRING(amountDue[2]). 
  EXPORT STREAM b DELIMITER ','  "EXCHANGE RATE @" STRING(xRate) + "/USD1".
  FOR EACH hsehtf WHERE hsehtf.dbacc = wsAcc AND hsehtf.trdate < startdate:
      balbf = balbf + hsehtf.amt.
  END.
  EXPORT STREAM b DELIMITER ',' "PERIOD" "TRANSACTION DATE" "REFERENCE" "DESCRIPTION" "AMOUNT" "BALANCE".
  EXPORT STREAM b DELIMITER ',' "" "" "" "Balance B/F" "" balbf.
  rbal = balbf.
  FOR EACH hsehtf WHERE hsehtf.dbacc = wsAcc AND hsehtf.trdate >= startdate AND hsehtf.trdate <=enddate:
      rbal = rbal + hsehtf.amt.
      EXPORT STREAM b DELIMITER ','  hsehtf.accper hsehtf.trdate hsehtf.REF hsehtf.descrip hsehtf.amt rbal.
  END.
END.

procedure Search.ip.
hCol = browse brw-hsecmf:current-column.
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    case trim(hCol:label):
        when "Name" then
        do:
           open query qry-hsecmf preselect 
                    EACH hsecmf no-lock 
                        where hsecmf.NAME >= wsSearch:SCREEN-VALUE
                           BY hsecmf.NAME.

        END.
        when "Account" then
        do:
           open query qry-hsecmf preselect 
                    EACH hsecmf no-lock 
                        where hsecmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE)
                           BY hsecmf.dbAcc.

        END.
        when "StandNo" then
        do:
           open query qry-hsecmf preselect 
                    EACH hsecmf no-lock 
                        where hsecmf.StandNo >= wsSearch:SCREEN-VALUE
                           BY hsecmf.StandNo.

        END.
    END.
    RETURN.
END.
