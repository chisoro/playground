/* Program.................dbEnq.p
   Notes:................. Account Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsService AS CHAR FORM "x(15)".
DEF VAR wsCurr    AS DEC FORM "zzzzzzzz9.9999-".
DEF VAR wsReceipts    AS DEC FORM "zzzzzzzz9.9999-".
DEF VAR wsArr     LIKE wsCurr FORM "zzzzzzzzzz9.99-".
DEF VAR wsTotal   LIKE wsCurr FORM "zzzzzzzzzz9.99-".
DEF VAR wsVar    AS CHAR FORM "x(20)".
DEF VAR X AS INT.
DEF VAR wsAcc LIKE dbcmf.dbacc.
DEF VAR wst LIKE wsCurr EXTENT 4 FORM "zzzzzz9.99-".
DEF VAR varDescrip AS CHAR FORM "x(20)".
DEF VAR wsPer LIKE dbhtf.Accper.
DEF VAR wsTitle AS CHAR FORM "x(80)" INITIAL "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: ".
DEF VAR wsTitle2 AS CHAR FORM "x(80)".
DEF VAR wsamt  LIKE dbhtf.amt.
DEF VAR wsbf LIKE dbhtf.amt LABEL "BALANCE".
DEF VAR wsFilter LIKE dbhtf.sgrp.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsdbAmt   LIKE wsAmt.
DEF VAR wsrate    LIKE tblforex.decRate.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btnAcc LABEL "ACCOUNT".
DEF BUTTON btnTrans LABEL "View Transaction" SIZE 20 BY 2.
DEF BUTTON btnHist  LABEL "Print History".
DEF BUTTON btnTar   LABEL "View Tariffs" SIZE 20 BY 2.
DEF BUTTON btnClose LABEL "Close".
DEF BUTTON btn-ok   LABEL "Ok".
DEF BUTTON btn-WTar   LABEL "Water Tarif/Readings" SIZE 20 BY 2.
DEF BUTTON btnNext   LABEL "Next Account>".
DEF BUTTON btnPrev   LABEL "<Prev Account".
DEF BUTTON btnHse   LABEL "LandSale Details" SIZE 20 BY 2.
DEF BUTTON btnTrack LABEL "Tracking".


DEF TEMP-TABLE dbtrans 
    FIELD AccPer LIKE dbhtf.Accper
    FIELD TransID LIKE dbhtf.TransID
    FIELD trDate LIKE dbhtf.trDate 
    FIELD ref    LIKE dbhtf.Ref 
    FIELD Sgrp   LIKE dbhtf.Sgrp 
    FIELD Tarif  LIKE dbhtf.tarif 
    FIELD descrip LIKE dbhtf.DESCRIP
    FIELD Amt    LIKE dbhtf.Amt 
    FIELD Vat    LIKE  dbhtf.Vat
    FIELD Bf     LIKE  dbhtf.Amt LABEL "BALANCE"
    FIELD Alloc  LIKE dbhtf.ILedger LABEL "ALLOACTION".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 17.

DEF TEMP-TABLE tmptat LIKE dbtat.

DEF    QUERY qry-trans FOR dbTrans  SCROLLING .
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY dbTrans.Accper LABEL "PERIOD" dbTrans.trDate LABEL "DATE" dbTrans.Ref LABEL "REFERENCE" WIDTH 15 
    dbTrans.Sgrp LABEL "SERVICE" dbTrans.tarif LABEL "TARIFF" dbTrans.DESCRIP LABEL "DESCRIPTION"
    WIDTH 30 dbTrans.Amt LABEL "AMOUNT" dbTrans.Vat LABEL "VAT"  dbTrans.bf LABEL "BALANCE"
    dbtrans.alloc LABEL "ALLOCATION" WITH 20 DOWN SEPARATORS NO-LABEL.

DEF BUFFER bfrdbblf FOR dbblf.
DEF    QUERY qry-dbEnq FOR bfrdbblf SCROLLING.
DEF BROWSE brw-dbEnq QUERY qry-dbEnq
    DISPLAY bfrdbblf.Sgrp COLUMN-LABEL "CODE" wsService COLUMN-LABEL "SERVICE" WIDTH 40 wsCurr FORM "zzzzzzzzzz9.99-" COLUMN-LABEL "CURRENT" 
    wsArr FORM "zzzzzzzzzz9.99-" COLUMN-LABEL "ARREARS"  wsTotal  FORM "zzzzzzzzzz9.99-" COLUMN-LABEL "AMOUNT!(USD)"
     wsdbAmt FORM "zzzzzzzzzz9.99-" COLUMN-LABEL "AMOUNT!(ZWL)" 
    WITH 10 DOWN SEPARATORS.

DEFINE QUERY qry-tarif FOR tmptat scrolling.
DEF BROWSE brw-tarif QUERY qry-tarif
    DISPLAY tmptat.tarif varDescrip LABEL "Tariff" tmptat.Units 
    WITH 8 DOWN SEPARATORS.

DEF    QUERY qry-dbcmf FOR dbcmf SCROLLING.
DEF BROWSE brw-dbcmf QUERY qry-dbcmf
    DISPLAY dbcmf.dbAcc dbcmf.Name dbcmf.StandNo dbcmf.stno LABEL "StreetNo" dbcmf.street LABEL "Street" wsVar LABEL "Suburb" 
     WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-rec FOR dbrec SCROLLING.
DEF BROWSE brw-rec QUERY qry-rec
    DISPLAY dbrec.recno dbrec.recdate dbrec.amount dbrec.decrate dbrec.txtcur 
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
    wsFilter     COLON 10 LABEL "SERVICE"  HELP "Enter 0 for all Account Service Charges"
    SPACE(1) "(Enter 0 FOR ALL services)"  
    skip(1.5)
    btn-ok COLON 5
    btnclose COLON 20
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Period Selection".

DEFINE FRAME frmWater
    dbmmf.Meter at 2 LABEL "METER NUMBER# "  space(1)
    dbmmf.Tarif COLON 15 LABEL "TARIF# " space(1)
    dbtmf.Descrip no-label skip
    "LAST 12 METER READINGS" COLON 5 SKIP
    "READING DATE           READING     CONSUMPTION      COMMENTS" COLON 2 SKIP
    dbmmf.RDate[1] AT 8 NO-LABEL space(4) dbmmf.read[1] NO-LABEL space(4) 
    dbmmf.consum[1] NO-LABEL space(3) dbmmf.comm[1] NO-LABEL skip
    dbmmf.RDate[2] AT 8 NO-LABEL space(4) dbmmf.read[2] NO-LABEL space(4) 
    dbmmf.consum[2] NO-LABEL space(3) dbmmf.comm[2] NO-LABEL skip
    dbmmf.RDate[3] AT 8 NO-LABEL space(4) dbmmf.read[3] NO-LABEL space(4) 
    dbmmf.consum[3] NO-LABEL space(3) dbmmf.comm[3] NO-LABEL skip
    dbmmf.RDate[4] AT 8 NO-LABEL space(4) dbmmf.read[4] NO-LABEL space(4) 
    dbmmf.consum[4] NO-LABEL space(3) dbmmf.comm[4] NO-LABEL skip
    dbmmf.RDate[5] AT 8 NO-LABEL space(4) dbmmf.read[5] NO-LABEL space(4) 
    dbmmf.consum[5] NO-LABEL space(3) dbmmf.comm[5] NO-LABEL skip
    dbmmf.RDate[6] AT 8 NO-LABEL space(4) dbmmf.read[6] NO-LABEL space(4) 
    dbmmf.consum[6] NO-LABEL space(3) dbmmf.comm[6] NO-LABEL skip
    dbmmf.RDate[7] AT 8 NO-LABEL space(4) dbmmf.read[7] NO-LABEL space(4) 
    dbmmf.consum[7] NO-LABEL space(3) dbmmf.comm[7] NO-LABEL skip
    dbmmf.RDate[8] AT 8 NO-LABEL space(4) dbmmf.read[8] NO-LABEL space(4) 
    dbmmf.consum[8] NO-LABEL space(3) dbmmf.comm[8] NO-LABEL skip
    dbmmf.RDate[9] AT 8 NO-LABEL space(4) dbmmf.read[9] NO-LABEL space(4) 
    dbmmf.consum[9] NO-LABEL space(3) dbmmf.comm[9] NO-LABEL skip
    dbmmf.RDate[10] AT 8 NO-LABEL space(4) dbmmf.read[10] NO-LABEL space(4) 
    dbmmf.consum[10] NO-LABEL space(3) dbmmf.comm[10] NO-LABEL skip
    dbmmf.RDate[11] AT 8 NO-LABEL space(4) dbmmf.read[11] NO-LABEL space(4) 
    dbmmf.consum[11] NO-LABEL space(3) dbmmf.comm[11] NO-LABEL skip
    dbmmf.RDate[12] AT 8 NO-LABEL space(4) dbmmf.read[12] NO-LABEL space(4) 
    dbmmf.consum[12] NO-LABEL space(3) dbmmf.comm[12] NO-LABEL skip
    dbmmf.RDate[13] AT 8 NO-LABEL space(4) dbmmf.read[13] NO-LABEL space(4) 
    dbmmf.consum[13] NO-LABEL space(3) dbmmf.comm[13] NO-LABEL skip(0.5)
    btnclose COLON 15
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         side-labels NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "WATER CONSUMPTION DETAILS".

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
    dbcmf.dbAcc     NO-LABEL
    dbcmf.NAME      view-as text no-label NO-TAB-STOP  
    wsRate LABEL "TODAY'S USD RATE " COLON 95 VIEW-AS TEXT FONT 6 skip(.5)
    dbcmf.Add1      COLON 15 LABEL "ADDRESS"  VIEW-AS TEXT NO-TAB-STOP SPACE(15)
    dbcmf.SIZE      LABEL "Property Size " VIEW-AS TEXT NO-TAB-STOP
    dbcmf.add2      COLON 15 NO-LABEL VIEW-AS TEXT NO-TAB-STOP SPACE(14.5) 
    dbcmf.BldValue  LABEL "Building Value" VIEW-AS TEXT NO-TAB-STOP
    dbcmf.Add3      COLON 15 NO-LABEL VIEW-AS TEXT NO-TAB-STOP SPACE(17)
    dbcmf.SiteValue LABEL "Land Value" VIEW-AS TEXT NO-TAB-STOP SKIP(0.2)
    dbcmf.street    COLON 15 LABEL "Street Address" VIEW-AS TEXT NO-TAB-STOP SPACE(15)
    dbcmf.lpdate    LABEL "Last Payment Date" VIEW-AS TEXT NO-TAB-STOP SKIP(0.2)
    dbsmf.Descrip    COLON 15 LABEL "Suburb" VIEW-AS TEXT NO-TAB-STOP
    dbwrd.Descrip      COLON 15 LABEL "Ward" VIEW-AS TEXT NO-TAB-STOP
    "Unposted Receipts:" AT ROW 16.5 COLUMN 10 FONT 6
    wsreceipts AT ROW 16.6 COLUMN 27  NO-TAB-STOP NO-LABEL VIEW-AS TEXT FONT 6 FORM "zzzzzzz9.99-"
    wsT[1] AT ROW 16.5 COLUMN 48.3 NO-TAB-STOP NO-LABEL FORM "zzzzzzz9.99-"
    wsT[2] AT ROW 16.5 COLUMN 63.3 NO-TAB-STOP NO-LABEL FORM "zzzzzzz9.99-"
    wsT[3] AT ROW 16.5 COLUMN 78 NO-TAB-STOP NO-LABEL FORM "zzzzzzz9.99-"
    wsT[4] AT ROW 16.5 COLUMN 93 NO-TAB-STOP NO-LABEL FORM "zzzzzzzzzz9.99-"
    rect-2 AT ROW 1 COLUMN 3
    rect-1 AT ROW 21 COLUMN 3 SKIP(0.5)
    brw-dbEnq AT ROW 7.3 COLUMN 4
    "Press F6 for unposted Receipts" FGCOLOR 4 FONT 6 AT ROW 17.8 COL 10
    "Press F7 to Update Cell and eMail" FGCOLOR 9 FONT 6 AT ROW 17.8 COL 60
   btnTrans at row 18.5 column 5 space(5) btnHse space(8)  btnTar space(8) btn-Wtar 
   btnNext AT ROW 21.5 COLUMN 8 space(9) btnPrev space(9)  btnHist  space(9) btnTrack space(9)btnclose
   with view-as dialog-box keep-tab-order NO-VALIDATE
         side-labels no-underline three-d SCROLLABLE CENTERED WIDTH 115
    TITLE "ACCOUNT ENQUIRY".
FORM 
    dbhtf.Accper LABEL "PERIOD"
    dbhtf.trDate  LABEL "DATE"
    dbhtf.Sgrp    LABEL "SS"
    dbhtf.Tarif   LABEL "TAR"
    dbhtf.Ref     LABEL "REFERENCE" form "x(12)"
    dbhtf.DESCRIP LABEL "DESCRIPTION" form "x(40)"
    dbhtf.Vat     LABEL "VAT"
    dbhtf.Amt     LABEL "AMOUNT"
    wsAmt         LABEL "BALANCE"
    HEADER skip(5) wsTitle AT 10 "Page: "  PAGE-NUMBER(a)
    SKIP(1)
    wsTitle2 AT 10
    SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX no-label WIDTH 132 FRAME frm-rpt.

DEFINE FRAME frm-rec 
    brw-rec AT ROW 2 COL 5
    skip(0.5)
    btnclose colon 15
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "UNPOSTED RECEIPTS".

DEFINE FRAME frm-Tarif 
    brw-Tarif AT ROW 2 COL 5
    skip(0.5)
    btnclose colon 15
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Attached tariffs".

DEFINE FRAME frm-pickAcc 
   brw-dbcmf AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

/******** Triggers ***********/
ON  'enter':u OF dbcmf.dbacc IN FRAME  frm-Enq
     OR 'tab':u OF dbcmf.dbacc IN FRAME  frm-Enq
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbcmf THEN DO:
      MESSAGE "Account does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:
       wsT = 0.
       IF CAN-FIND (FIRST landsale WHERE landsale.dbacc = dbcmf.dbacc) THEN
           ENABLE btnHse WITH FRAME frm-Enq.
       ELSE DISABLE  btnHse WITH FRAME frm-Enq.
       FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-LOCK NO-ERROR.
       FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.  /* */
       DISPLAY dbcmf.Name dbcmf.stno + "," + dbcmf.street @ dbcmf.street dbcmf.SiteValue dbcmf.Size dbwrd.Descrip dbsmf.Descrip 
           dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.BldValue dbcmf.lpdate WITH FRAME frm-Enq.
       RUN blfdata-ip.
      OPEN QUERY qry-dbEnq FOR EACH bfrdbblf WHERE bfrdbblf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE)  NO-LOCK.
     DISPLAY wsT wsReceipts WITH FRAME frm-Enq.
   END.
   RETURN.    
END.

ON CHOOSE OF btnNext IN FRAME frm-Enq
DO:
    wsT = 0.
    FIND NEXT dbcmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN
        FIND LAST dbcmf NO-LOCK NO-ERROR.
    dbcmf.dbacc:SCREEN-VALUE = STRING(dbcmf.dbAcc).
    APPLY 'tab' TO dbcmf.dbAcc IN FRAME frm-Enq.
    RUN blfdata-ip.
    FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-LOCK NO-ERROR.
       FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR. /*  */ 
       DISPLAY dbcmf.Name dbcmf.stno + "," + dbcmf.street @ dbcmf.street dbcmf.SiteValue dbcmf.Size dbwrd.Descrip dbsmf.Descrip 
           dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.BldValue dbcmf.lpdate WITH FRAME frm-Enq.
      /*OPEN QUERY qry-dbEnq FOR EACH bfrdbblf WHERE bfrdbblf.dbacc = dbcmf.dbacc  NO-LOCK.
     DISPLAY wsT WITH FRAME frm-Enq. */
end.

ON CHOOSE OF btnPrev IN FRAME frm-Enq
DO:
    wsT = 0.
    FIND PREV dbcmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN
        FIND FIRST dbcmf NO-LOCK NO-ERROR.
    dbcmf.dbacc:SCREEN-VALUE = STRING(dbcmf.dbAcc).
     APPLY 'tab' TO dbcmf.dbAcc IN FRAME frm-Enq.
     RUN blfdata-ip.
    FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-LOCK NO-ERROR.
       FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR. /*  */ 
       DISPLAY dbcmf.Name  dbcmf.stno + "," + dbcmf.street @ dbcmf.street dbcmf.SiteValue dbcmf.Size dbwrd.Descrip dbsmf.Descrip 
           dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.BldValue dbcmf.lpdate WITH FRAME frm-Enq.
      /*OPEN QUERY qry-dbEnq FOR EACH bfrdbblf WHERE bfrdbblf.dbacc = dbcmf.dbacc  NO-LOCK.
     DISPLAY wsT WITH FRAME frm-Enq. */
end.

ON CHOOSE OF btnAcc IN FRAME frm-Enq
DO:
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-dbcmf FOR EACH dbcmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-dbcmf
          OR 'mouse-select-dblclick' OF brw-dbcmf.
  CLOSE QUERY qry-dbcmf.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO dbcmf.dbAcc.
  RETURN. 
END.

ON 'F6':u ANYWHERE 
DO:
   VIEW FRAME frm-rec.
   ENABLE ALL WITH FRAME frm-rec.
   OPEN QUERY qry-rec FOR EACH dbrec 
        WHERE dbrec.account = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-enq).
   WAIT-FOR CHOOSE OF btnCLOSE OR close of THIS-PROCEDURE IN FRAME frm-rec.
   CLOSE QUERY qry-rec.
   HIDE FRAME frm-rec.
   RETURN.
END.

ON 'choose':U OF btnclose IN FRAME frm-rec
DO:
    APPLY 'close' TO THIS-PROCEDURE.
    CLOSE QUERY qry-rec.
    HIDE FRAME frm-rec.
    RETURN.
END.
ON 'PAGE-DOWN':U OF brw-dbcmf
DO:
   REPOSITION qry-dbcmf FORWARDS  + 19.
    RETURN.
END.

ON 'PAGE-UP':U OF brw-dbcmf
DO:
   REPOSITION qry-dbcmf BACKWARDS  + 19.
    RETURN.
END.

ON CHOOSE OF btnTar IN FRAME frm-Enq 
DO:
  if  dbcmf.dbacc:screen-value = "" then
     return no-apply.
  FOR EACH tmptat.
      DELETE tmptat.
  END.
  FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:screen-value) NO-ERROR.
  DO X = 1 TO 14:
        IF dbcmf.tarif[X] <> 0 THEN DO: 
            CREATE tmptat.
            ASSIGN tmptat.tarif = dbcmf.tarif[X]
                   tmptat.units = dbcmf.units[X].
        END.
   END.
   VIEW FRAME frm-tarif.
   ENABLE ALL WITH FRAME frm-tarif.
   OPEN QUERY qry-tarif FOR EACH tmptat NO-LOCK.
   WAIT-FOR CHOOSE OF btnCLOSE OR close of THIS-PROCEDURE IN FRAME frm-Tarif.
   CLOSE QUERY qry-tarif.
   HIDE FRAME frm-tarif.
   RETURN.
END.

ON CHOOSE OF btnTrack IN FRAME frm-Enq 
DO:
   VIEW FRAME frm-track.
   ENABLE ALL WITH FRAME frm-track.
   OPEN QUERY qry-track FOR EACH dbTrack WHERE dbTrack.acc = DEC(dbcmf.dbacc:screen-value) NO-LOCK.
   WAIT-FOR CHOOSE OF btnCLOSE OR close of THIS-PROCEDURE IN FRAME frm-Track.
   CLOSE QUERY qry-track.
   HIDE FRAME frm-track.
   RETURN.
END.

ON row-display OF brw-dbEnq DO:
    wsCurr = bfrdbblf.amt[1]  + bfrdbblf.amt[2].
    wsArr = bfrdbblf.Int.
    DO X = 3 TO 15:
        wsArr = wsArr + bfrdbblf.amt[X].
    END.
    wsTotal = wsCurr + wsArr.
    wsdbAmt  = (wsTotal * wsrate).
    FIND FIRST dbsgr WHERE dbsgr.Sgrp = bfrdbblf.sgrp NO-LOCK NO-ERROR.
    wsService = dbsgr.Descrip.
    /*ASSIGN wsT[1] = wsT[1] + wsCurr
           wsT[2] = wsT[2] + wsArr
           wsT[3] = wsT[3] + wsTotal
           wsT[4] = wsT[4] + wsdbAmt.*/
END.

ON row-display OF brw-Tarif DO:
    FIND FIRST dbtmf WHERE dbtmf.Tarif = tmptat.tarif AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
    varDescrip = dbtmf.Descrip.
END.

ON row-display OF brw-dbcmf DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
    wsVar = dbsmf.Descrip.
END. 

/*ON row-display OF brw-trans DO:
   wsbf = wsbf + dbhtf.Amt.
END. */

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-dbcmf 
    OR 'mouse-select-dblclick' OF brw-dbcmf 
DO: 
   GET CURRENT qry-dbcmf  NO-LOCK NO-WAIT.
    wsAcc = dbcmf.dbacc.
   DISPLAY  dbcmf.dbacc WITH FRAME frm-enq.
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
  if  dbcmf.dbacc:screen-value = "" then
     return no-apply.
  VIEW FRAME frmPeriod.
  ENABLE ALL WITH FRAME frmPeriod.
  assign wsPer:screen-value = string(SIMCTR.CURPER)
         wsFilter:screen-value = "0".
  WAIT-FOR CHOOSE OF btnclose IN FRAME frmPeriod 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmPeriod.
  assign wsPer     = int(wsPer:screen-value)
          wsFilter = int(wsFilter:screen-value).
  HIDE FRAME frmPeriod.
  RUN ViewTrans.ip.
  RETURN.
END.

ON CHOOSE OF btn-WTAr IN FRAME frm-Enq 
DO:
  if  dbcmf.dbacc:screen-value = "" then
     return no-apply.
  find first dbmmf where dbmmf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq) NO-LOCK no-error.
  if not available dbmmf then do:
      message "No Metered Water on this Account" view-as alert-box.
      return no-apply.
  end.
  else do:
       find first dbtmf where dbtmf.tarif = dbmmf.tarif and dbtmf.type = 1 NO-LOCK NO-ERROR.
       IF AVAILABLE dbtmf THEN
           varDescrip = dbtmf.Descrip.
       VIEW FRAME frmWater.
       display dbmmf.Meter dbmmf.Tarif varDescrip @ dbtmf.Descrip
               dbmmf.rdate dbmmf.read
               dbmmf.consum dbmmf.comm
                  with frame frmWater.
              
       ENABLE  btnclose  WITH FRAME frmWater.        
  end.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frmWater 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmWater.
  HIDE FRAME frmWater.
  RETURN.
END.

ON CHOOSE OF btnHist IN FRAME frm-Enq 
DO:
 if  dbcmf.dbacc:screen-value = "" then
     return no-apply.
  find first SIMCTR NO-LOCK no-error.
  VIEW FRAME frmPeriod.
  assign wsPer:screen-value = string(SIMCTR.CURPER)
         wsFilter:screen-value = "0".
  ENABLE ALL WITH FRAME frmPeriod.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frmPeriod 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmPeriod.
  assign wsPer    = int(wsPer:screen-value)
         wsFilter = int(wsFilter:screen-value).
  HIDE FRAME frmPeriod.
  {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged}
END.

on 'start-search':u of browse brw-dbcmf
    run Search.ip.

ON 'choose':U OF btnSearch IN FRAME Search-opt 
    OR 'enter':U OF btnSearch IN FRAME Search-opt
    OR 'tab':U OF btnSearch IN FRAME Search-opt
DO:
    open query qry-dbcmf preselect 
                    each dbcmf no-lock 
                        where dbcmf.SortName >= wsSearch:SCREEN-VALUE
                           BY dbcmf.Sortname.
    /*RETURN.*/
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND TODAY >= tblForex.DtRate  NO-LOCK NO-ERROR.
IF AVAILABLE tblForex THEN DO:
   ASSIGN wsRate = tblForex.decRate.
END.
IF NOT AVAILABLE tblForex  THEN DO:
   FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND tblForexH.dtRate <= TODAY NO-LOCK NO-ERROR.
        IF AVAILABLE tblForexH THEN DO:
           ASSIGN wsRate = tblForexH.decRate.
         END.
END.
ENABLE ALL /*EXCEPT brw-dbEnq */ WITH FRAME frm-enq.
DISPLAY wsRate WITH FRAME frm-enq.
browse brw-dbcmf:allow-column-searching = true.
WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frm-enq.
HIDE FRAME frm-enq.

/******************INTERNAL PROCEDURES *****************/
PROCEDURE ViewTrans.ip:
    wsbf = 0.
    FOR EACH dbtrans.
        DELETE dbtrans.
    END.
  VIEW FRAME frm-trans.
  ENABLE ALL WITH FRAME frm-trans.
  CREATE dbTrans.
  ASSIGN dbtrans.descrip = "Balance B/F".
  if wsFilter = 0 THEN DO:
      FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                       AND dbhtf.Accper < int(wsPer) NO-LOCK BY dbhtf.AccPer BY dbhtf.TransID:
        ASSIGN dbTrans.bf = dbTrans.bf + dbhtf.Amt
               wsbf       = wsbf + dbhtf.Amt.
      END.
      FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                       AND dbhtf.Accper >= int(wsPer) NO-LOCK BY dbhtf.AccPer BY dbhtf.TransID:
          CREATE dbTrans.
          wsbf       = wsbf + dbhtf.Amt.
          ASSIGN dbTrans.AccPer = dbhtf.Accper
                dbTrans.TransID = dbhtf.TransID
                dbTrans.trDate = dbhtf.trDate 
                dbTrans.ref    =dbhtf.Ref 
                dbTrans.Sgrp   = dbhtf.Sgrp 
                dbTrans.Tarif  = dbhtf.tarif 
                dbTrans.descrip = dbhtf.DESCRIP
                dbTrans.Amt    = dbhtf.Amt 
                dbTrans.Vat    =  dbhtf.Vat
                dbTrans.Bf   =  wsBf
                dbTrans.Alloc = dbhtf.iledger.

      END.
      OPEN QUERY qry-trans FOR EACH dbTrans BY dbTrans.AccPer BY dbTrans.trDate BY dbTrans.TransID.
  END.
  ELSE DO:
      /*FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                     AND dbhtf.Accper >= int(wsPer) AND dbhtf.Sgrp = int(wsFilter) NO-LOCK:
        wsbf = wsbf + dbhtf.Amt.
      END.
      OPEN QUERY qry-trans FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                     AND dbhtf.Accper >= int(wsPer) AND dbhtf.Sgrp = int(wsFilter) NO-LOCK.
  END.*/
       FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                        AND dbhtf.Accper < int(wsPer)AND dbhtf.Sgrp = int(wsFilter) NO-LOCK:
        ASSIGN dbTrans.bf = dbTrans.bf + dbhtf.Amt
               wsbf       = wsbf + dbhtf.Amt.
      END.
      FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                       AND dbhtf.Accper >= int(wsPer) AND dbhtf.Sgrp = int(wsFilter) NO-LOCK:
        
          CREATE dbTrans.
           wsbf       = wsbf + dbhtf.Amt.
          ASSIGN dbTrans.AccPer = dbhtf.Accper
                dbTrans.trDate = dbhtf.trDate 
                dbTrans.ref    =dbhtf.Ref 
                dbTrans.Sgrp   = dbhtf.Sgrp 
                dbTrans.Tarif  = dbhtf.tarif 
                dbTrans.descrip = dbhtf.DESCRIP
                dbTrans.Amt    = dbhtf.Amt 
                dbTrans.Vat    =  dbhtf.Vat
                dbTrans.Bf   =  wsBf
                dbTrans.Alloc = dbhtf.iledger.
      END.
      OPEN QUERY qry-trans FOR EACH dbTrans BY dbTrans.AccPer BY dbTrans.trDate BY dbTrans.TransID.
   
          END.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-trans 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frm-trans.
  RELEASE dbcmf.
  CLOSE QUERY qry-trans.
  HIDE FRAME frm-trans.
  RETURN.
END.

PROCEDURE report.ip:
FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq) NO-LOCK NO-ERROR.
wsTitle2 = "ACCOUNT: " + dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq + " ("  +  DBcmf.NAME + ")".
wsAmt = AccBal.
IF wsFilter = 0 THEN DO:
    FOR EACH dbhtf WHERE dbhtf.dbAcc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq) 
                     AND dbhtf.Accper >= int(wsPer) NO-LOCK:
         wsAmt = wsAmt - dbhtf.Amt.
    END.    
    DISPLAY STREAM a "Balance b/f" @ dbhtf.DESCRIP wsAmt WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                    AND dbhtf.Accper >= int(wsPer) NO-LOCK BY dbhtf.AccPer BY dbhtf.TransID :
        wsAmt = wsAmt + dbhtf.Amt. /* + dbhtf.Vat */
        DISPLAY STREAM a dbhtf.Accper dbhtf.trDate dbhtf.Sgrp dbhtf.Tarif dbhtf.Ref  
                        dbhtf.DESCRIP dbhtf.Amt dbhtf.Vat wsAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
END.
ELSE DO:
   FIND FIRST dbsgr WHERE dbsgr.sgrp = wsFilter NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbsgr THEN DO:
      MESSAGE "NO Service records for this account" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   wsAmt = 0.
   wsTitle2 = "ACCOUNT: " + dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq + " ("  +  DBcmf.NAME + ")" 
            + "- " + dbsgr.Descrip + " Services".     
   FOR EACH bfrdbblf WHERE bfrdbblf.dbAcc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                    AND bfrdbblf.Sgrp = wsFilter NO-LOCK:
       DO X = 1 TO 15:
          wsAmt = wsAmt + bfrdbblf.Amt[X] +  bfrdbblf.INT.
      END.
   END.
   FOR EACH dbhtf WHERE dbhtf.dbAcc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                    AND dbhtf.Accper >= int(wsPer) AND dbhtf.Sgrp = int(wsFilter) NO-LOCK BY dbhtf.AccPer BY dbhtf.TransID:
       wsAmt = wsAmt - dbhtf.Amt. /* - dbhtf.Vat. */
   END.
   DISPLAY STREAM a "Balance b/f" @ dbhtf.DESCRIP wsAmt WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   FOR EACH dbhtf WHERE dbhtf.dbacc = DEC(dbcmf.dbAcc:SCREEN-VALUE IN FRAME frm-Enq)
                     AND dbhtf.Accper >= int(wsPer) AND dbhtf.Sgrp = wsFilter NO-LOCK
                     BY dbhtf.AccPer BY dbhtf.TransID :
       wsAmt = wsAmt + dbhtf.Amt. /* + dbhtf.Vat. */
       DISPLAY STREAM a dbhtf.Accper dbhtf.trDate dbhtf.Sgrp dbhtf.Tarif dbhtf.Ref  
                 dbhtf.DESCRIP dbhtf.Amt dbhtf.Vat wsAmt WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
   END.
END.
return.
END.

procedure Search.ip.
hCol = browse brw-dbcmf:current-column.
    /*assign  frame Search-opt:title = hCol:label + " Column"
            frame Search-opt:x     = hCol:x
            frame Search-opt:y     = browse b-{&t_zoom_table}:y. */
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    /*if ll-sort = ? then return.*/
    case trim(hCol:label):
        when "Name" then
        do:
           open query qry-dbcmf
                    FOR EACH dbcmf no-lock 
                        where dbcmf.SortName >= wsSearch:SCREEN-VALUE USE-INDEX NAME. 

        END.
        when "Account" then
        do:
           open query qry-dbcmf 
                   FOR  EACH dbcmf no-lock 
                        where dbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbAcc.
                           

        END.
        when "StandNo" then
        do:
           open query qry-dbcmf 
                    FOR EACH dbcmf no-lock 
                        where dbcmf.StandNo >= wsSearch:SCREEN-VALUE USE-INDEX Stand.
                          
        END.
        when "StreetNo" then
        do:
           open query qry-dbcmf 
                    FOR EACH dbcmf no-lock 
                        where dbcmf.stno >= wsSearch:SCREEN-VALUE USE-INDEX Street.
                    END.
    END.
    RETURN.
END.

PROCEDURE blfdata-ip:
   wsReceipts = 0.
   FOR EACH dbrec WHERE dbRec.Account = dbcmf.dbAcc AND dbRec.RecStat = "" AND dbRec.contype = "C" NO-LOCK:
        FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
        ASSIGN wsReceipts = wsReceipts + ROUND((dbRec.Amount / ( wsRate / dbrec.decRate)),2) WHEN dbrec.txtCur <> simctr.dbcur
              wsReceipts = wsReceipts + dbRec.Amount WHEN dbrec.txtCur = simctr.dbcur .
    END.
   FOR EACH bfrdbblf WHERE bfrdbblf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-Enq ): 
        wsCurr = bfrdbblf.amt[1]  + bfrdbblf.amt[2].
        wsArr = bfrdbblf.Int.
        DO X = 3 TO 15:
            wsArr = wsArr + bfrdbblf.amt[X].
        END.
        wsTotal = wsCurr + wsArr.
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = bfrdbblf.sgrp NO-LOCK NO-ERROR.
        wsService = dbsgr.Descrip.
        ASSIGN wsT[1] = wsT[1] + wsCurr
               wsT[2] = wsT[2] + wsArr
               wsT[3] = wsT[3] + wsTotal
               wsdbAmt  = (wsTotal * wsrate)
               wsT[4] = wsT[4] + wsdbAmt.
    END.
    wsT[3] = wsT[3] - wsReceipts.
    wsT[4] = wsT[4] - ROUND((wsReceipts * wsRate),2).
END.
