/* Program.................hse03.p
   Notes:................. Account Enquiries
   Author:.................S. Mawire
   Modified:...............S. Chisoro
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsScheme AS CHAR FORM "x(15)".
DEF VAR wsStand LIKE hsecmf.StandNo.
DEF VAR wsCurr    LIKE hsehtf.amt.
DEF VAR wsArr     LIKE wsCurr.
DEF VAR wsTotal   LIKE wsCurr.
DEF VAR wsVar    AS CHAR FORM "x(20)".
DEF VAR wsdes AS CHAR FORM "x(10)".
DEF VAR X AS INT.
DEF VAR wsAcc LIKE hsecmf.dbacc.
/*DEF VAR wst LIKE wsCurr EXTENT 3.*/
DEF VAR varDescrip AS CHAR FORM "x(20)".
DEF VAR wsPer LIKE hsehtf.Accper.
DEF VAR wsTitle AS CHAR FORM "x(80)" INITIAL "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: ".
DEF VAR wsTitle2 AS CHAR FORM "x(80)".
DEF VAR wsamt  LIKE hsehtf.amt.
DEF VAR amDue LIKE hsehtf.amt.
DEF VAR wsbf LIKE hsehtf.amt LABEL "BALANCE".
DEF VAR wsFilter LIKE hsehtf.scheme.
DEF VAR wsSearch   LIKE hsecmf.NAME.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btnAcc LABEL "ACCOUNT".
DEF BUTTON btnTrans LABEL "View Transaction" SIZE 20 BY 2.
DEF BUTTON btnAge LABEL "Age Analysis" SIZE 20 BY 2.
DEF BUTTON btnAgeH LABEL "Age Analysis History" SIZE 20 BY 2.
DEF BUTTON btnHist  LABEL "Print History".
DEF BUTTON btnClose LABEL "Close".
DEF BUTTON btn-ok   LABEL "Ok".
DEF BUTTON btnNext   LABEL "Next Account>".
DEF BUTTON btnPrev   LABEL "<Prev Account".
DEF BUTTON btnTrack LABEL "Tracking".
DEF VAR i AS INTEGER NO-UNDO.
DEF TEMP-TABLE hsetrans 
    FIELD AccPer LIKE hsehtf.Accper
    FIELD TransID LIKE hsehtf.TransID
    FIELD trDate LIKE hsehtf.trDate 
    FIELD ref    LIKE hsehtf.Ref 
    FIELD scheme   LIKE hsehtf.scheme 
    FIELD descrip LIKE hsehtf.DESCRIP
    FIELD Amt    LIKE hsehtf.Amt 
    FIELD Vat    LIKE  hsehtf.Vat
    FIELD Bf     LIKE  hsehtf.Amt LABEL "BALANCE"
    FIELD Alloc  LIKE hsehtf.ILedger LABEL "ALLOACTION".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 185 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 185 by 16.

DEFINE RECTANGLE rect-3
    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
    SIZE 85 BY 3.25.

DEFINE RECTANGLE rect-4
    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
    SIZE 85 BY 5.
DEFINE RECTANGLE rect-5
    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
    SIZE  85 BY 4.
DEFINE RECTANGLE rect-6
    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
    SIZE 90 BY 13.25.

DEF TEMP-TABLE tmptat LIKE dbtat.

DEF    QUERY qry-trans FOR hsetrans SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY hsetrans.Accper LABEL "PERIOD" hsetrans.trDate LABEL "DATE" hsetrans.Ref LABEL "REFERENCE" WIDTH 15 
    hsetrans.scheme LABEL "SERVICE"  hsetrans.DESCRIP LABEL "DESCRIPTION"
    WIDTH 30  hsetrans.Vat LABEL "VAT" hsetrans.Amt LABEL "AMOUNT" hsetrans.bf LABEL "BALANCE"
    hsetrans.alloc LABEL "ALLOCATION" WITH 20 DOWN SEPARATORS NO-LABEL.

DEF    QUERY qry-Ttrans FOR hsetrans SCROLLING.
DEF BROWSE brw-Ttrans QUERY qry-Ttrans
    DISPLAY hsetrans.Accper LABEL "PERIOD" hsetrans.trDate LABEL "DATE" hsetrans.Ref LABEL "REFERENCE" WIDTH 15 
    hsetrans.DESCRIP LABEL "DESCRIPTION"
    WIDTH 25 hsetrans.Vat LABEL "VAT" hsetrans.Amt LABEL "AMOUNT" 
    WITH 12 DOWN SEPARATORS NO-LABEL.

DEF    QUERY qry-hsecmf FOR hsecmf SCROLLING.
DEF BROWSE brw-hsecmf QUERY qry-hsecmf
    DISPLAY hsecmf.dbAcc hsecmf.Name hsecmf.StandNo wsVar LABEL "Scheme" 
     WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-track FOR dbtrack scrolling.
DEF BROWSE brw-track QUERY qry-track
    DISPLAY dbTrack.txtSource dbTrack.EDate dbTrack.Ref dbTrack.txtEvent 
    WITH 8 DOWN SEPARATORS.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEFINE FRAME frmPeriod
    skip(2)
    wsPer        colon 10 LABEL "Start Period"  SKIP(0.5)
    skip(1.5)
    btn-ok COLON 5
    btnclose COLON 20
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Period Selection".


DEFINE FRAME frm-trans 
    brw-trans AT ROW 2 COL 5
    skip(0.5)
    btnclose COLON 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Listing".

DEFINE FRAME frm-track 
    brw-track AT ROW 2 COL 5
    skip(0.5)
    btnclose COLON 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ACCOUNT TRACKING EVENTS".

define frame frm-enq
    SKIP(.5)
    btnAcc          COLON 5 NO-TAB-STOP SPACE(1)
    hsecmf.dbAcc     NO-LABEL NO-TAB-STOP
    "ACCOUNT DETAILS" COLON 5 SKIP
    hsecmf.NAME      COLON 15 LABEL "NAME" view-as text    no-tab-stop
    hsecmf.Add1      COLON 15 LABEL "ADDRESS"  FORM "x(25)" VIEW-AS TEXT SPACE (10)
    hsecmf.cell[1]     LABEL "CELL" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.add2      COLON 15 FORM "x(20)" NO-LABEL VIEW-AS TEXT SPACE (21)
    hsecmf.cell[2]    no-LABEL  VIEW-AS TEXT NO-TAB-STOP
    hsecmf.Add3      COLON 15 FORM "X(20)" NO-LABEL VIEW-AS TEXT SPACE (15)
    hsecmf.emailAd   LABEL "E-Mail"  FORM "x(30)" VIEW-AS TEXT NO-TAB-STOP
          
    "PROPERTY DETAILS" AT ROW 7 COL 7 SKIP
    hsecmf.scheme COLON 21 LABEL "Scheme" VIEW-AS TEXT
    hsesch.descrip NO-LABEL VIEW-AS TEXT NO-TAB-STOP
    hsecmf.standNo COLON 21 LABEL "Stand" VIEW-AS TEXT SPACE(30)
    hsecmf.DeedNo LABEL "Deed" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.SIZE     COLON 21 LABEL "Property Size " FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.BldValue  COLON 21 LABEL "Building Value" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.SiteValue COLON 21 LABEL "Land Value" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    dbwrd.Descrip      COLON 21 LABEL "Ward" VIEW-AS TEXT NO-TAB-STOP
    dbsmf.Descrip    COLON 21 LABEL "Suburb" VIEW-AS TEXT NO-TAB-STOP

     "PURCHASE DETAILS" AT ROW 12.5 COL 7 SKIP
    hsecmf.PDate COLON 25 LABEL "Purchase Date" VIEW-AS TEXT SPACE(22) 
    hsecmf.prate LABEL "Exchange Rate" FORM "->>>,>>9.9999" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.pprice COLON 25 LABEL "Purchase Price" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT SPACE (20.5)
    hsecmf.dAmt LABEL "Deposit" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.period COLON 25 LABEL "Amortization Period" FORM "99" VIEW-AS TEXT SPACE (1)
    wsdes NO-LABEL VIEW-AS TEXT SPACE(11)
    hsesch.intRate LABEL "Annual Interest Rate" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    hsecmf.bal COLON 25 LABEL "Capital Balance"FORM "->>>,>>>,>>9.99"  VIEW-AS TEXT SPACE(18)
    hsecmf.insAmt LABEL "Installment" FORM "->>>,>>>,>>9.99" VIEW-AS TEXT NO-TAB-STOP
    
    hsecmf.AmtDue[1] AT ROW 3.5 COLUMN 98 LABEL "Trading Amount Due" FORM "->>>,>>>,>>9.99" VIEW-AS text
    hsecmf.AmtDue[2]LABEL "Accounting Amount Due" FORM "->>>,>>>,>>9.99" view-as text NO-TAB-STOP
    /*"LAST 12 TRANSACTIONS"*/ "" AT ROW 4.5 COLUMN 98 SKIP
    brw-Ttrans AT ROW 5.5 COL 98
    skip(0.5)
    rect-2 AT ROW 1 COLUMN 3
    rect-5 AT ROW 3 COLUMN 6
    rect-4 AT ROW 7.5 COLUMN 6
    rect-3 AT ROW 13 COLUMN 6
    rect-1 AT ROW 17.5 COLUMN 3 SKIP(0.5)
    rect-6 AT ROW 3 COLUMN 95
    btnTrans at row 17.6 column 5
   btnNext AT ROW 20.5 COLUMN 8 space(30) btnPrev space(30)  btnHist  space(30) btnTrack space(30)btnclose SKIP(.5)
   with view-as dialog-box keep-tab-order NO-VALIDATE
         side-labels no-underline three-d SCROLLABLE CENTERED WIDTH 190
    TITLE "ACCOUNT ENQUIRY".
FORM 
    hsehtf.Accper LABEL "PERIOD"
    hsehtf.trDate  LABEL "DATE"
    hsehtf.scheme    LABEL "SC"
    hsehtf.Ref     LABEL "REFERENCE" form "x(12)"
    hsehtf.DESCRIP LABEL "DESCRIPTION" form "x(40)"
    hsehtf.Vat     LABEL "VAT"
    hsehtf.Amt     LABEL "AMOUNT"
    wsAmt         LABEL "BALANCE"
    HEADER skip(5) wsTitle AT 10 "Page: "  PAGE-NUMBER(a)
    SKIP(1)
    wsTitle2 AT 10
    SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX no-label WIDTH 132 FRAME frm-rpt.

DEFINE FRAME frm-pickAcc 
   brw-hsecmf AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

/******** Triggers ***********/

ON  'enter':u OF hsecmf.dbacc IN FRAME  frm-Enq
     OR 'tab':u OF hsecmf.dbacc IN FRAME  frm-Enq
DO:
   FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(hsecmf.dbacc:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsecmf THEN DO:
      MESSAGE "Account does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      FIND FIRST dbwrd WHERE dbwrd.ward = hsecmf.ward NO-LOCK NO-ERROR.
       FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
       FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
       IF hsesch.Freq = "M"  THEN
          wsdes = "Months".
       IF hsesch.Freq = "Q"  THEN
          wsdes = "Quarters".
       IF hsesch.Freq = "H"  THEN
          wsdes = "Half-Years".
       IF hsesch.Freq = "Y"  THEN
          wsdes = "Years".
       FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur.
       amDue = hsecmf.AmtDue[1] * tblForex.decRate.
       DISPLAY hsecmf.Name hsecmf.SiteValue hsecmf.Size dbwrd.Descrip dbsmf.Descrip hsecmf.schem
           hsesch.descrip hsecmf.Add1 hsecmf.add2 hsecmf.Add3 hsecmf.standNo hsecmf.DeedNo hsecmf.BldValue hsecmf.cell hsecmf.emailad hsecmf.AmtDue[1] amDue @ hsecmf.AmtDue[2]
           hsecmf.PDate hsesch.intRate wsdes hsecmf.prate hsecmf.pprice hsecmf.dAmt hsecmf.period hsecmf.bal hsecmf.insAmt WITH FRAME frm-Enq.
      END.
      RUN ViewTTrans.ip.
   RETURN.    
END.

ON CHOOSE OF btnNext IN FRAME frm-Enq
DO:
    FIND NEXT hsecmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE hsecmf THEN
        FIND LAST hsecmf NO-LOCK NO-ERROR.
    hsecmf.dbacc:SCREEN-VALUE = STRING(hsecmf.dbAcc).
    APPLY 'tab' TO hsecmf.dbAcc IN FRAME frm-Enq.
end.

ON CHOOSE OF btnPrev IN FRAME frm-Enq
DO:
   FIND PREV hsecmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE hsecmf THEN
        FIND FIRST hsecmf NO-LOCK NO-ERROR.
    hsecmf.dbacc:SCREEN-VALUE = STRING(hsecmf.dbAcc).
     APPLY 'tab' TO hsecmf.dbAcc IN FRAME frm-Enq.
end.

ON CHOOSE OF btnAcc IN FRAME frm-Enq
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


ON CHOOSE OF btnTrack IN FRAME frm-Enq 
DO:
   VIEW FRAME frm-track.
   ENABLE ALL WITH FRAME frm-track.
   OPEN QUERY qry-track FOR EACH dbTrack WHERE dbTrack.acc = DEC(hsecmf.dbacc:screen-value) NO-LOCK.
   WAIT-FOR CHOOSE OF btnCLOSE OR close of THIS-PROCEDURE IN FRAME frm-Track.
   CLOSE QUERY qry-track.
   HIDE FRAME frm-track.
   RETURN.
END.

ON row-display OF brw-hsecmf DO:
    FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
    wsVar = hsesch.Descrip.
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-hsecmf 
    OR 'mouse-select-dblclick' OF brw-hsecmf 
DO: 
   GET CURRENT qry-hsecmf  NO-LOCK NO-WAIT.
    wsAcc = hsecmf.dbacc.
   DISPLAY  hsecmf.dbacc WITH FRAME frm-enq.
   RETURN.
END.

ON CHOOSE OF btn-Ok IN FRAME frmPeriod 
DO:
    ASSIGN wsPer = INT(wsPer:SCREEN-VALUE).
    HIDE FRAME frmPeriod.
    APPLY "close" TO THIS-PROCEDURE IN FRAME frmPeriod.
    
    RETURN.
END.

ON CHOOSE OF btnTrans IN FRAME frm-Enq 
DO:
  find first SIMCTR NO-LOCK no-error.
  if  hsecmf.dbacc:screen-value = "" then
     return no-apply.
  VIEW FRAME frmPeriod.
  ENABLE ALL WITH FRAME frmPeriod.
  assign wsPer:screen-value = string(SIMCTR.CURPER).
  WAIT-FOR CHOOSE OF btnclose IN FRAME frmPeriod 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmPeriod.
  assign wsPer     = int(wsPer:screen-value).
  HIDE FRAME frmPeriod.
  RUN ViewTrans.ip.
  RETURN.
END.


ON CHOOSE OF btnHist IN FRAME frm-Enq 
DO:
 if  hsecmf.dbacc:screen-value = "" then
     return no-apply.
  find first SIMCTR NO-LOCK no-error.
  VIEW FRAME frmPeriod.
  assign wsPer:screen-value = string(SIMCTR.CURPER).
  ENABLE ALL WITH FRAME frmPeriod.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frmPeriod 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmPeriod.
  assign wsPer    = int(wsPer:screen-value).
  HIDE FRAME frmPeriod.
  {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged}
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

ON ROW-DISPLAY OF brw-Trans
DO:
    IF hsetrans.Amt < 0  THEN
    ASSIGN hsetrans.Amt:FGCOLOR IN BROWSE brw-trans = 12 /* assign red colour */
           hsetrans.vat:FGCOLOR IN BROWSE brw-trans = 12
           hsetrans.bf:FGCOLOR  IN BROWSE brw-trans = 12.
END.

ON ROW-DISPLAY OF brw-TTrans
DO:
    IF hsetrans.Amt < 0  THEN
    ASSIGN hsetrans.Amt:FGCOLOR IN BROWSE brw-Ttrans = 12 /* assign red colour */
           hsetrans.vat:FGCOLOR IN BROWSE brw-Ttrans = 12.
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
ENABLE ALL  WITH FRAME frm-enq.
browse brw-hsecmf:allow-column-searching = true.
WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frm-enq.
HIDE FRAME frm-enq.

/******************INTERNAL PROCEDURES *****************/
PROCEDURE ViewTrans.ip:
    wsbf = 0.
    FOR EACH hsetrans.
        DELETE hsetrans.
    END.
  VIEW FRAME frm-trans.
  ENABLE ALL WITH FRAME frm-trans.
  CREATE hsetrans.
  ASSIGN hsetrans.descrip = "Balance B/F".
  
      FOR EACH hsehtf WHERE hsehtf.dbacc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                       AND hsehtf.Accper < int(wsPer) NO-LOCK BY hsehtf.AccPer BY hsehtf.TransID:
        ASSIGN hsetrans.bf = hsetrans.bf + hsehtf.Amt
               wsbf       = wsbf + hsehtf.Amt.
      END.
      FOR EACH hsehtf WHERE hsehtf.dbacc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                       AND hsehtf.Accper >= int(wsPer) NO-LOCK BY hsehtf.AccPer BY hsehtf.TransID:
          CREATE hsetrans.
          wsbf       = wsbf + hsehtf.Amt.
          ASSIGN hsetrans.AccPer = hsehtf.Accper
                hsetrans.TransID = hsehtf.TransID
                hsetrans.trDate = hsehtf.trDate 
                hsetrans.ref    =hsehtf.Ref 
                hsetrans.scheme   = hsehtf.scheme 
                hsetrans.descrip = hsehtf.DESCRIP
                hsetrans.Amt    = hsehtf.Amt 
                hsetrans.Vat    =  hsehtf.Vat
                hsetrans.Bf   =  wsBf
                hsetrans.Alloc = hsehtf.iledger.

      END.
      OPEN QUERY qry-trans FOR EACH hsetrans BY hsetrans.AccPer BY hsetrans.TransID.
  
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-trans 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frm-trans.
  RELEASE hsecmf.
  CLOSE QUERY qry-trans.
  HIDE FRAME frm-trans.
  RETURN.
END.

PROCEDURE ViewTTrans.ip:
    i = 0.
    FOR EACH hsetrans.
        DELETE hsetrans.
    END.
  VIEW FRAME frm-enq.
  ENABLE ALL WITH FRAME frm-enq.
FOR EACH hsehtf WHERE hsehtf.dbacc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
          BY hsehtf.trdate DESCENDING /*WHILE i < 12 */:
    CREATE hsetrans.
    ASSIGN hsetrans.AccPer = hsehtf.Accper
       hsetrans.TransID = hsehtf.TransID
       hsetrans.trDate = hsehtf.trDate 
       hsetrans.ref    =hsehtf.Ref 
       hsetrans.scheme   = hsehtf.scheme 
       hsetrans.descrip = hsehtf.DESCRIP
       hsetrans.Amt    = hsehtf.Amt 
       hsetrans.Vat    =  hsehtf.Vat
   i = i + 1.
 END.
OPEN QUERY qry-ttrans FOR EACH hsetrans.
RETURN.
END.


PROCEDURE report.ip:
    FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq) NO-LOCK NO-ERROR.
    wsTitle2 = "ACCOUNT: " + hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq + " ("  +  hsecmf.NAME + ")".
    wsAmt = AmtDue[1].
    
    FOR EACH hsehtf WHERE hsehtf.dbAcc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq) 
                                            AND hsehtf.Accper >= int(wsPer) NO-LOCK:
         wsAmt = wsAmt - hsehtf.Amt.
    END.    
    DISPLAY STREAM a "Balance b/f" @ hsehtf.DESCRIP wsAmt WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    FOR EACH hsehtf WHERE hsehtf.dbacc = DEC(hsecmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                    AND hsehtf.Accper >= int(wsPer) NO-LOCK BY hsehtf.AccPer BY hsehtf.TransID :
        wsAmt = wsAmt + hsehtf.Amt. 
        DISPLAY STREAM a hsehtf.Accper hsehtf.trDate hsehtf.scheme hsehtf.Ref  
                        hsehtf.DESCRIP hsehtf.Amt hsehtf.Vat wsAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
return.
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
                        where hsecmf.sortname >= wsSearch:SCREEN-VALUE USE-INDEX sortname.

        END.
        when "Account" then
        do:
           open query qry-hsecmf preselect 
                    EACH hsecmf no-lock 
                        where hsecmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbAcc.

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
