/* Program.................ordCap.p
   Notes:................. Order Data capture
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE wsMsg           "Order does not exist"
&SCOPED-DEFINE wsTitle         " Online Orders Data capture"
&SCOPED-DEFINE tmptable        ordmf
&SCOPED-DEFINE skey            ordmf.OrdNo
&SCOPED-DEFINE UpdFields    bfr{&tmptable}.descrip ~
                              bfr{&tmptable}.Accper~
                                                     
/**/ 
{ pdf_inc.i "THIS-PROCEDURE"}
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

DEF    QUERY qry-Pick FOR crdmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
     DISPLAY crdmf.Acc crdmf.NAME WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 3 COL 5
    btn-add AT ROW 20.7 COL 30
    Space(30) btn-prn
    space(30) btn-del
    space(30) btn-exit SKIP(1)
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
    btnAddl  colon 10
    SPACE(15) btnSave  SPACE(15) btnDel  SPACE(15) btn-close SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "ORDER DATA CAPTURE".

DEFINE FRAME frm-input1 SKIP(1)
         tmptrans.Descrip COLON 20 LABEL "DESCRIPTION" SKIP(0.5)
         tmptrans.Qty     COLON 20 LABEL "QUANTITY"    SKIP(0.5)
         tmptrans.Amt     COLON 20 LABEL "AMOUNT"   tmpTrans.vat LABEL "VAT" SKIP(0.5)
         btnLedger COLON 8 NO-TAB-STOP tmpTrans.Ledger  NO-LABEL glmf.DESCRIPTION NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnDept   COLON 3 NO-TAB-STOP tmpTrans.Dept    NO-LABEL gldept.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnfund   COLON 8 NO-TAB-STOP tmpTrans.Fund    NO-LABEL glfund.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
         btnProj   COLON 8 NO-TAB-STOP tmpTrans.Proj    NO-LABEL glproj.Descrip   NO-LABEL VIEW-AS TEXT SKIP(0.5)
        SKIP(1.5)
        SPACE(30) btnSav1 SPACE(20) btn-close
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "ORDER LINE DATA CAPTURE".

DEFINE FRAME frm-Supplier 
     brw-Pick AT ROW 1 COL 1.5 skip(0.2)
     btn-Ok COLON 8 SPACE (20) btn-close 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Supplier Selection".


/* Triggers for the frmMain frame */
ON CHOOSE OF btn-Add IN FRAME frmMain DO:
        RUN proc-input.
        RETURN.
END.

ON 'enter':u OF brw{&tmptable} IN FRAME frmMain
        OR 'mouse-select-dblclick' OF brw{&tmptable}
    DO:
       GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
       ASSIGN wsid = bfr{&tmptable}.ordNo. 
       IF  bfr{&tmptable}.OrdStat <> 0 THEN DO:
           MESSAGE "Order status does not allow editting, maybe invoiced or under authorisation process"
               VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
       END.
       ELSE DO:
           DISABLE {&skey} WITH FRAME frm-input.
       run proc-edit.
       END.
       RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frmMain DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Acc.
    FIND FIRST ordmf WHERE ordmf.OrdNo  = bfr{&tmptable}.OrdNo NO-ERROR.
    IF AVAILABLE ordmf AND ordmf.OrdStat = 9 THEN DO:
        MESSAGE "Order under Authorisation - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE ordmf AND ordmf.OrdStat = 2 THEN DO:
        MESSAGE "Order has been Invoiced - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE ordmf AND ordmf.OrdStat = 3 THEN DO:
        MESSAGE "Order has already been Cancelled - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO: /*Cancel Order */
     bfr{&tmptable}.OrdStat = 3.
    OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat <> 2 NO-LOCK.
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-prn IN FRAME frmMain 
DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.OrdNo.
    RUN rep.ip.
    RETURN.
END.

ON row-display OF brw{&tmptable} DO:
    FIND FIRST crdmf WHERE crdmf.acc = bfr{&tmptable}.Acc NO-LOCK NO-ERROR.
    wsName = crdmf.NAME.
     ASSIGN wsStat = "UnAuthorised" WHEN bfr{&tmptable}.ordStat = 0
            wsStat = "Outstanding"  WHEN bfr{&tmptable}.ordStat = 1
            wsStat = "Invoiced"     WHEN bfr{&tmptable}.ordStat = 2
            wsStat = "Cancelled"    WHEN bfr{&tmptable}.ordStat = 3
            wsStat = "Authorising"  WHEN bfr{&tmptable}.ordStat = 9.
END.

/* Triggers for the frame frm-Input  */
ON CHOOSE OF btnacc IN FRAME frm-Input
DO:
  VIEW FRAME frm-Supplier.
  OPEN QUERY qry-pick FOR EACH crdmf WHERE crdmf.acc <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Supplier.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-Supplier 
          OR close of THIS-PROCEDURE IN FRAME frm-Supplier
          OR CHOOSE OF btn-ok IN FRAME frm-Supplier 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-Supplier.
  APPLY 'tab' TO btnacc.
  APPLY 'tab' TO bfr{&tmptable}.Acc.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Supplier 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY crdmf.Acc @ bfr{&tmptable}.Acc crdmf.NAME @ wsName WITH FRAME frm-Input.
   APPLY 'TAB' TO bfr{&tmptable}.Acc IN FRAME frm-Input.
END.

ON 'tab' of bfr{&tmptable}.Acc IN FRAME frm-Input
    OR 'enter' OF bfr{&tmptable}.Acc IN FRAME frm-Input
DO:
    FIND FIRST crdmf WHERE crdmf.acc = int(bfr{&tmptable}.Acc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL crdmf THEN DO:
            MESSAGE "Invalid Supplier entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE
        DISPLAY crdmf.Acc @ bfr{&tmptable}.Acc crdmf.NAME @ wsName WITH FRAME frm-Input.
END.

ON LEAVE OF bfr{&tmptable}.descrip IN FRAME frm-input
DO:
   APPLY 'tab' TO brw-ordl.
   APPLY 'entry' TO btnAddl.
   RETURN NO-APPLY.
END.

ON 'choose':U OF btnAddl IN FRAME frm-Input 
DO:
   FIND LAST tmptrans WHERE tmptrans.ordNo = DEC({&skey}:SCREEN-VALUE) NO-ERROR.
   IF AVAILABLE tmptrans THEN
       wsSeq = tmptrans.LineSeq + 1.
   ELSE wsSeq = 1.
   VIEW FRAME frm-Input1.
   ENABLE ALL WITH FRAME frm-Input1.
   WAIT-FOR CHOOSE OF btn-close OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input1.
   HIDE FRAME frm-Input1.
   RETURN.
END.

ON 'choose':U OF btnSave IN FRAME frm-input 
DO:
    IF btnSave:LABEL = "SAVE" THEN DO:
        IF  DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE) <> 0 THEN DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.OrdNo      = DEC({&skey}:SCREEN-VALUE)
                 bfr{&tmptable}.Acc        = INT(bfr{&tmptable}.Acc:SCREEN-VALUE)
                 bfr{&tmptable}.Accper     = SIMCTR.CURPER
                 bfr{&tmptable}.AuthCnt    = 0
                 bfr{&tmptable}.AuthStat   = ""
                 bfr{&tmptable}.Descrip    =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                 bfr{&tmptable}.OrdAmt     = DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE)
                 bfr{&tmptable}.OrdBy      = bfr{&tmptable}.OrdBy:SCREEN-VALUE
                 bfr{&tmptable}.OrdDate    = DATE( bfr{&tmptable}.OrdDat:SCREEN-VALUE)
                 bfr{&tmptable}.OrdStat    = 1. /* assign 0 when authorisation is required */
          FOR EACH tmpTrans:
              CREATE ordtmf.
              BUFFER-COPY tmpTrans TO ordtmf.
              DELETE tmpTrans.
          END.
        END.
    END.
    ELSE IF btnSave:LABEL = "UPDATE" THEN DO:
            FIND FIRST ordmf WHERE ordmf.ordNo = wsid NO-ERROR.
            ASSIGN bfr{&tmptable}.Acc        = INT(bfr{&tmptable}.Acc:SCREEN-VALUE)
                     bfr{&tmptable}.AuthCnt    = 0
                     bfr{&tmptable}.AuthStat   = ""
                     bfr{&tmptable}.Descrip    =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                     bfr{&tmptable}.OrdAmt     = DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE)
                     bfr{&tmptable}.OrdBy      = bfr{&tmptable}.OrdBy:SCREEN-VALUE
                     bfr{&tmptable}.OrdDate    = DATE( bfr{&tmptable}.OrdDat:SCREEN-VALUE)
                     bfr{&tmptable}.OrdStat    = 0.
            FOR EACH ordtmf WHERE ordtmf.ordNo =wsid:
                DELETE ordtmf.
            END.
            FOR EACH tmpTrans:
                CREATE ordtmf.
                BUFFER-COPY tmpTrans TO ordtmf.
                DELETE tmpTrans.
            END.
            RETURN.
    END.
  CLEAR FRAME frm-input ALL.
  HIDE FRAME frm-Input.
  RETURN.      
END.

ON 'enter':u OF brw-ordl IN FRAME frm-Input
        OR 'mouse-select-dblclick' OF brw-ordl
DO:
    btnSav1:LABEL IN  FRAME frm-Input1 = "UPDATE".
    btn-Close:LABEL IN  FRAME frm-Input1 = "CANCEL".
    GET CURRENT qry-ordl EXCLUSIVE-LOCK NO-WAIT.
    current-record = ROWID(tmpTrans).
    VIEW FRAME frm-Input1.
    DISPLAY tmpTrans EXCEPT tmpTrans.OrdNo tmpTrans.LineSeq tmpTrans.InvAmt WITH FRAME frm-Input1.
    ENABLE ALL WITH FRAME frm-Input1.
    WAIT-FOR CHOOSE OF btnSav1 OR CHOOSE OF btn-close OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input1.
    HIDE FRAME frm-Input1.
    RETURN.
END.

ON CHOOSE OF btnDel
DO:
    GET CURRENT qry-ordl EXCLUSIVE-LOCK NO-WAIT.
    bfr{&tmptable}.OrdAmt:SCREEN-VALUE  
        = STRING(DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE) - tmpTrans.Amt).
    DELETE tmpTrans.
     Method-Status = brw-ordl:DELETE-SELECTED-ROWS().
    OPEN QUERY qry-ordl FOR EACH tmpTrans NO-LOCK.
END.

/* Triggers for the frame frm-Input1  */

ON CHOOSE OF btnLedger IN FRAME frm-Input1
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO tmptrans.Ledger.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  tmptrans.Ledger glmf.DESCRIPTION WITH FRAME frm-Input1.
   APPLY 'tab' TO  tmptrans.Ledger IN FRAME frm-Input1.
END.

ON 'tab' of  tmptrans.Ledger IN FRAME frm-Input1
    OR 'enter' OF  tmptrans.Ledger IN FRAME frm-Input1
DO:
    IF  DEC( tmptrans.Ledger:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid ledger entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.Acct = DEC(tmptrans.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  tmptrans.Ledger glmf.DESCRIPTION WITH FRAME frm-Input1.
            IF glmf.dept <> 0 THEN DO:
               DISPLAY glmf.Dept @  tmptrans.dept WITH FRAME frm-Input1.
               APPLY 'tab' TO tmptrans.dept IN FRAME frm-Input1.
               DISABLE tmptrans.dept WITH FRAME frm-Input1.
            END.
            ELSE ENABLE tmptrans.dept WITH FRAME frm-Input1.
            IF glmf.proj <> 0 THEN DO:
               DISPLAY glmf.proj @  tmptrans.proj WITH FRAME frm-Input1.
               APPLY 'tab' TO tmptrans.proj IN FRAME frm-Input1.
               DISABLE tmptrans.proj WITH FRAME frm-Input1.
            END.
            ELSE ENABLE tmptrans.proj WITH FRAME frm-Input1.
            IF glmf.fund <> 0 THEN DO:
               DISPLAY glmf.fund @  tmptrans.fund WITH FRAME frm-Input1.
               APPLY 'tab' TO tmptrans.fund IN FRAME frm-Input1.
               DISABLE tmptrans.fund WITH FRAME frm-Input1.
            END.
            ELSE ENABLE tmptrans.fund WITH FRAME frm-Input1.
        END.
    END.
END.

ON LEAVE OF tmpTrans.Descrip IN FRAME frm-input1
DO:
    IF tmpTrans.Descrip:SCREEN-VALUE = "" THEN DO:
       APPLY 'close' TO FRAME frm-Input1.
       HIDE FRAME frm-Input1.
    END.
    RETURN.   
END.

ON CHOOSE OF btnDept IN FRAME frm-Input1
DO:
  VIEW FRAME frm-pickdept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickdept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickdept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickdept
          OR CHOOSE OF btn-ok IN FRAME frm-pickdept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-pickdept.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btnFund IN FRAME frm-Input1
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO btnFund IN FRAME frm-Input1.
  APPLY 'tab' TO tmptrans.Fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund NO-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ tmpTrans.Fund glFund.DESCRIP  WITH FRAME frm-Input1.
   APPLY 'tab' TO btnFund IN FRAME frm-Input1.
   RETURN.
END.

ON 'enter':U OF tmptrans.Fund 
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(tmptrans.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Segment/Fund entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.Descrip WITH FRAME frm-Input1.
        APPLY 'tab' TO tmptrans.Fund IN FRAME frm-Input1.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickdept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ tmptrans.dept gldept.descrip WITH FRAME frm-Input1.
   RETURN.
END.

ON 'enter':U OF tmptrans.dept IN FRAME frm-Input1
   OR 'tab':U OF tmptrans.dept IN FRAME frm-Input1
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(tmptrans.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY gldept.descrip WITH FRAME frm-Input1.
    END.
    RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-Input1
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btn-ok IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO btnProj IN FRAME frm-Input1.
  APPLY 'entry' TO tmptrans.proj.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ tmptrans.Proj glproj.DESCRIPTION  WITH FRAME frm-Input1.
  APPLY 'tab' TO btnProj IN FRAME frm-Input1.
END.

ON 'enter':U OF tmptrans.proj IN FRAME frm-Input1
    OR 'tab':U OF tmptrans.proj IN FRAME frm-Input1
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(tmptrans.proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glProj.descrip WITH FRAME frm-Input1.
    END.
    RETURN.
END.

ON 'choose' OF btnSav1 IN FRAME frm-input1
DO:
    IF btnSav1:LABEL = "SAVE"  THEN DO:
        CREATE tmpTrans.
        ASSIGN tmpTrans.Amt = DEC(tmpTrans.Amt:SCREEN-VALUE)
               tmpTrans.vat = DEC(tmpTrans.vat:SCREEN-VALUE)
               tmpTrans.Dept = INT(tmpTrans.Dept:SCREEN-VALUE) 
               tmpTrans.Descrip = tmpTrans.Descrip:SCREEN-VALUE
               tmpTrans.Fund    = INT( tmpTrans.Fund:SCREEN-VALUE)
               tmpTrans.Ledger  = DEC(tmpTrans.Ledger:SCREEN-VALUE)
               tmpTrans.LineSeq = wsSeq
               tmpTrans.OrdNo   = DEC({&skey}:SCREEN-VALUE IN FRAME frm-Input)
               tmpTrans.Proj    = INT(tmpTrans.Proj:SCREEN-VALUE)
               tmpTrans.Qty     = DEC(tmpTrans.Qty:SCREEN-VALUE)
               wsSeq            = wsSeq + 1.
        bfr{&tmptable}.OrdAmt:SCREEN-VALUE IN FRAME frm-Input
             =  STRING(DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE) + tmpTrans.Amt).
        OPEN QUERY qry-ordl FOR EACH tmpTrans.
        CLEAR FRAME frm-Input1 ALL.
        APPLY 'entry' TO tmpTrans.Descrip IN FRAME frm-Input1.
    END.
    ELSE  IF btnSav1:LABEL = "UPDATE"  THEN DO:
        bfr{&tmptable}.OrdAmt:SCREEN-VALUE IN FRAME frm-Input
             =  STRING(DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE) - tmpTrans.Amt).
        
        bfr{&tmptable}.OrdAmt:SCREEN-VALUE IN FRAME frm-Input
             =  STRING( DEC(bfr{&tmptable}.OrdAmt:SCREEN-VALUE) 
                       + DEC(tmpTrans.Amt:SCREEN-VALUE)).
        ASSIGN tmpTrans.Amt     = DEC(tmpTrans.Amt:SCREEN-VALUE)
               tmpTrans.vat = DEC(tmpTrans.vat:SCREEN-VALUE)
               tmpTrans.Dept    = INT(tmpTrans.Dept:SCREEN-VALUE) 
               tmpTrans.Descrip = tmpTrans.Descrip:SCREEN-VALUE
               tmpTrans.Fund    = INT( tmpTrans.Fund:SCREEN-VALUE)
               tmpTrans.Ledger  = DEC(tmpTrans.Ledger:SCREEN-VALUE)
               tmpTrans.Proj    = INT(tmpTrans.Proj:SCREEN-VALUE)
               tmpTrans.Qty     = DEC(tmpTrans.Qty:SCREEN-VALUE).
        OPEN QUERY qry-ordl FOR EACH tmpTrans.
        btnSav1:LABEL = "SAVE".
        CLEAR FRAME frm-Input1 ALL.
        HIDE FRAME frm-Input1.
   END.
   RETURN.
END.


/********** MAIN LOGIC **********/
VIEW FRAME frmMain.
ENABLE ALL WITH FRAME frmMain.
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat <> 2 NO-LOCK.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.


PROCEDURE proc-input:
   FIND FIRST simctr NO-LOCK NO-ERROR. 
   CLEAR FRAME frm-input ALL.
   bfr{&tmptable}.ordDate:SCREEN-VALUE = STRING(TODAY).
   FIND FIRST bfr{&tmptable} NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bfr{&tmptable} THEN
       {&skey}:SCREEN-VALUE = STRING((INT(SUBSTR(STRING(SIMCTR.CURPER),1,4)) * 1000000) + 1).
      ELSE IF AVAILABLE bfr{&tmptable}  AND 
          SUBSTR(STRING(bfr{&tmptable}.ordno),1,4) <> SUBSTR(STRING(SIMCTR.CURPER),1,4) THEN
        {&skey}:SCREEN-VALUE = STRING((INT(SUBSTR(STRING(SIMCTR.CURPER),1,4)) * 1000000) + 1).
        ELSE IF AVAILABLE bfr{&tmptable} THEN 
        DO:
            {&skey}:SCREEN-VALUE = STRING(bfr{&tmptable}.ordno + 1).
        END.
           
   ENABLE ALL EXCEPT {&skey} bfr{&tmptable}.OrdAmt WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btnsave OR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat <> 2 NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
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
    /*DISABLE ALL WITH FRAME frm-input.*/
     btnSave:LABEL IN  FRAME frm-input = "UPDATE".
    ENABLE  ALL EXCEPT {&skey}  bfr{&tmptable}.ordAmt WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btnsave OR CHOOSE OF btn-close 
         OR close of THIS-PROCEDURE IN FRAME frm-input.
     btnSave:LABEL IN  FRAME frm-input = "SAVE".
     HIDE FRAME frm-input.
     FOR EACH tmptrans:
         DELETE tmpTrans.
     END.
    OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.OrdStat <> 2 NO-LOCK.
 END.

PROCEDURE REP.IP:
  {ordprn01.i}
 END.


  procedure Search.ip.
    hCol = browse  brw-ledger:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Account" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.acct >= DEC(wsSearch:SCREEN-VALUE)
                               BY glmf.acct.
            END.
            when "Description" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.descrip >= wsSearch:SCREEN-VALUE
                               BY glmf.descrip.
            END. 
            when "Department" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY glmf.dept BY glmf.acct.

            END.
        END.
        RETURN.
    END.
