/* Program.................ordenq.p
   Notes:................. Order Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE pgorientation "PORTRAIT"
&SCOPED-DEFINE  pagesize  66

&SCOPED-DEFINE wsMsg           "Order does not exist"
&SCOPED-DEFINE wsTitle         " ONLINE ORDER ENQUIRIES"
&SCOPED-DEFINE tmptable        ordmf
&SCOPED-DEFINE skey            ordmf.OrdNo
&SCOPED-DEFINE UpdFields    bfr{&tmptable}.descrip ~
                              bfr{&tmptable}.Accper~
 { pdf_inc.i "THIS-PROCEDURE"}                                                    
/**/                            
{varlibrary.i}
DEF VAR X AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsper    LIKE simctr.curper.
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsAcc    LIKE ordmf.acc.
DEF VAR wsAmt    LIKE ordmf.OrdAmt FORM "zzz,zzz,zz9.99-".
DEF VAR wsOrd    LIKE ordmf.OrdNo.
DEF VAR wsQty    LIKE ordtmf.qty FORM "zzz,zzz,zz9.99-".
DEF VAR wsTotal  LIKE wsAmt     FORM "zzz,zzz,zz9.99-".
DEF VAR wsNar    LIKE ordmf.descrip.
DEF VAR wsDate   LIKE ordmf.ordDate.
DEF VAR wsFund   LIKE glmf.fund.
DEF VAR wsLedger LIKE ordtmf.ledger.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsName   LIKE crdmf.NAME.
DEF VAR wsStat   AS CHAR  FORM "x(12)".
DEF VAR wsseq    LIKE Ordtmf.LineSeq.
DEF VAR wsopt      AS INT VIEW-AS RADIO-SET HORIZONTAL 
    RADIO-BUTTONS "Outstanding Orders", 1,
                  "Invoiced Orders", 2,
                  "Cancelled Orders", 3,
                  "UnAthorised", 0,
                  "Pending Authorisation",9.

DEF BUTTON btn-Prn  LABEL "PRINT".
DEF BUTTON btnAddl LABEL "ADD LINE".
DEF BUTTON btnSave LABEL "SAVE".
DEF BUTTON btnSav1 LABEL "SAVE".
DEF BUTTON btnDel  LABEL "DELETE".

DEF TEMP-TABLE tmpTrans LIKE ordtmf.

DEFINE RECTANGLE rect1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 190 by 2.3.
DEFINE RECTANGLE rect2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 190 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
     DISPLAY bfr{&tmptable}.OrdNo LABEL "ORDER" bfr{&tmptable}.OrdDate LABEL "DATE"
    bfr{&tmptable}.Descrip  LABEL "DESCRIPTION" bfr{&tmptable}.OrdAmt LABEL "AMOUNT" 
    wsName LABEL "SUPPLIER" wsStat LABEL "STATUS" WITH NO-LABELS 20 DOWN SEPARATORS.

DEF QUERY qry-ordl FOR tmpTrans SCROLLING.
DEF BROWSE brw-ordl QUERY qry-ordl
    DISPLAY tmpTrans.LineSeq LABEL "LINE" tmpTrans.Descrip LABEL "DESCRIPTION" 
            tmpTrans.Qty LABEL "QUANTITY" tmpTrans.Amt LABEL "AMOUNT"
            tmpTrans.Ledger LABEL "LEDGER" WITH NO-LABELS 10 DOWN SEPARATORS.

DEF NEW SHARED FRAME frmMain
    wsOpt AT ROW 2 COL 10 LABEL "Select Filter Option" AUTO-RETURN
    brw{&tmptable} AT ROW 3 COL 5
    btn-prn AT ROW 20.7 COL 30
    space(90) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    /*BGCOLOR 8 FGCOLOR 1 */ SIZE 195 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-input SKIP(1)
        {&skey}  AT ROW 2 COL 6           LABEL "ORDER NUMBER" 
        bfr{&tmptable}.OrdDate   COLON 80 LABEL "DATE" 
        btnAcc  AT ROW 4 COL 6 LABEL "SUPPLIER" bfr{&tmptable}.Acc NO-LABEL NO-TAB-STOP
        wsName   NO-LABEL VIEW-AS TEXT SKIP(0.5) 
        bfr{&tmptable}.OrdBy              LABEL "ORDERED BY" COLON 16 
        bfr{&tmptable}.OrdAmt    COLON 80 LABEL "AMOUNT" SKIP(0.2)
         bfr{&tmptable}.Descrip  COLON 16 LABEL "DESCRIPTION"
        brw-ordl AT ROW 8 COL 5
        SKIP(1.5)
     SPACE(50) btn-close SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "ONLINE ORDER ENQUIRIES".

DEFINE FRAME frm-input1 SKIP(1)
         tmptrans.Descrip COLON 20 LABEL "DESCRIPTION" SKIP(0.5)
         tmptrans.Qty     COLON 20 LABEL "QUANTITY"    SKIP(0.5)
         tmptrans.Amt     COLON 20 LABEL "AMOUNT"      SKIP(0.5)
         btnLedger COLON 8 NO-TAB-STOP tmpTrans.Ledger  NO-LABEL glmf.DESCRIPTION NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnDept   COLON 3 NO-TAB-STOP tmpTrans.Dept    NO-LABEL gldept.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnfund   COLON 8 NO-TAB-STOP tmpTrans.Fund    NO-LABEL glfund.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnProj   COLON 10 NO-TAB-STOP tmpTrans.Proj    NO-LABEL glproj.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
        SKIP(1.5)
        SPACE(30) btn-close
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "ORDER LINE DATA CAPTURE".


/* Triggers for the frmMain frame */
ON 'mouse-select-click' OF wsopt IN FRAME frmMain 
DO:
    ASSIGN wsOpt.
    OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat = wsopt NO-LOCK.
    RETURN.
END.
ON 'enter':u OF brw{&tmptable} IN FRAME frmMain
        OR 'mouse-select-dblclick' OF brw{&tmptable}
DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.OrdNo.
   RUN pro-View.
   RETURN.
END.


ON 'choose':U OF btn-prn IN FRAME frmMain 
DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.OrdNo.
    /*{PrintOpt.i &stream-name="stream a"
                    &print-prog= rep.ip
                    &paged} */
    RUN rep.ip.
    RETURN.
END.

ON row-display OF brw{&tmptable} DO:
    FIND FIRST crdmf WHERE crdmf.acc = bfr{&tmptable}.Acc NO-LOCK NO-ERROR.
    wsName = crdmf.NAME.
     ASSIGN wsStat = "UnAuthorised" WHEN bfr{&tmptable}.ordStat = 0
            wsStat = "Invoiced"    WHEN bfr{&tmptable}.ordStat = 2
            wsStat = "Cancelled"   WHEN bfr{&tmptable}.ordStat = 3
            wsStat = "Outstanding" WHEN bfr{&tmptable}.ordStat = 1
            wsStat = "Authorising" WHEN bfr{&tmptable}.ordStat = 9.
END.
/* Triggers for the frame frm-Input  */
ON 'enter':u OF brw-ordl IN FRAME frm-Input
        OR 'mouse-select-dblclick' OF brw-ordl
DO:
    GET CURRENT qry-ordl EXCLUSIVE-LOCK NO-WAIT.
    current-record = ROWID(tmpTrans).
    FIND FIRST glmf WHERE glmf.acct = tmptrans.ledger NO-ERROR.
    FIND FIRST glproj WHERE glproj.proj = tmptrans.proj NO-ERROR.
    FIND FIRST gldept WHERE gldept.dept = tmptrans.dept NO-ERROR.
    FIND FIRST glfund WHERE glfund.fund = tmptrans.fund NO-ERROR.
    VIEW FRAME frm-Input1.
    DISPLAY tmpTrans EXCEPT tmpTrans.OrdNo tmpTrans.LineSeq WITH FRAME frm-Input1.
    DISPLAY glmf.DESCRIPTION gldept.descrip glproj.descrip glfund.descrip WITH FRAME frm-Input1.
    ENABLE ALL WITH FRAME frm-Input1.
    WAIT-FOR CHOOSE OF btn-close OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input1.
    HIDE FRAME frm-Input1.
    RETURN.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frmMain.
ENABLE ALL WITH FRAME frmMain.
FIND FIRST simctr NO-LOCK NO-ERROR.
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat = 1 NO-LOCK.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE Pro-View:
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.ordNo  = wsid EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST crdmf WHERE crdmf.acc = bfr{&tmptable}.acc NO-LOCK NO-ERROR.
    IF AVAILABLE crdmf THEN
        wsName  = Name.
        ELSE wsName  = "Invalid Supplier".
    FOR EACH ordtmf WHERE ordtmf.ordNo =wsid:
        CREATE tmpTrans.
        BUFFER-COPY ordtmf TO tmpTrans.
    END.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.OrdBy 
        bfr{&tmptable}.Acc wsName bfr{&tmptable}.OrdDate bfr{&tmptable}.ordAmt WITH FRAME frm-input.
    OPEN QUERY qry-ordl FOR EACH tmpTrans.
    ENABLE  ALL EXCEPT {&skey}  bfr{&tmptable}.ordAmt WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btn-close 
         OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
     FOR EACH tmptrans:
         DELETE tmpTrans.
     END.
 END.

PROCEDURE REP.IP:
  {ordprn01.i}
 END.
