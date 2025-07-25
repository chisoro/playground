/* Program.................bnkRec.p
   Notes:................. Auto-Bank Reconciliation
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "??"
&SCOPED-DEFINE wsTitle           "???"
&SCOPED-DEFINE tmpTable            CBTrans
&SCOPED-DEFINE skey                CBTrans.bank

DEF BUFFER bfrBnkStat FOR bnkStat.
DEF BUFFER bfCbTrans FOR cbTrans.
DEF BUFFER bfrCbkmf FOR cbkmf.
DEF TEMP-TABLE tmpTrans LIKE CBTrans.
{varlibrary.i}
DEF STREAM a.
DEF STREAM b.
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR tdate LIKE bnkstat.trDate.
DEF VAR wsDate LIKE tdate.
DEF VAR wsdate1 LIKE tdate.
DEF VAR vDate LIKE bnkstat.trDate.
DEF VAR vDate1 LIKE Vdate.
DEF VAR wsRef LIKE bnkstat.Ref.
DEF VAR wsRef1 LIKE wsref.
DEF VAR wsDr LIKE bnkStat.Debit.
DEF VAR wsCr LIKE bnkStat.Credit.
DEF VAR wsDr1 LIKE bnkStat.Debit.
DEF VAR wsCr1 LIKE bnkStat.Credit.
DEF VAR wsTDr    LIKE wsCr.
DEF VAR wsTCr    LIKE wsCr.
DEF VAR wsnar LIKE bnkStat.Narrat.
DEF VAR wsPer LIKE bnkstat.period.
DEF VAR wsAmt LIKE cbTrans.Amount.
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsFund   LIKE glmf.fund.
DEF VAR wsLedger LIKE cbkmf.ledger.
DEF VAR wsVat    LIKE wsAmt.
DEF VAR wsTotal  LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsObal LIKE wsAmt.
DEF VAR wsBal LIKE wsAmt.
DEF VAR wsUnrec LIKE wsAmt.
DEF VAR wsCbTotal LIKE wsAmt.
DEF VAR dummy LIKE wsnar.
DEF VAR wsBank LIKE bfrcbkmf.bank.
DEF VAR wsOpt AS INT FORM "9".
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsTitle AS CHAR FORM "x(65)".
DEF VAR wsTitle1 AS CHAR FORM "x(65)".
DEF VAR wsAdd AS LOGICAL.
DEF VAR X AS INT.
DEF VAR wsF AS INT.
DEF VAR wsRate LIKE tblForex.decRate.
DEF VAR wsSource LIKE gltdf.SOURCE INITIAL "CB".
/*DEF BUTTON btnTrans    LABEL "TRANSACTION TYPE". */

DEF RECT rect-1 SIZE 93 BY 8.5.
DEF RECT rect-2 SIZE 93 BY 2.5.
DEF RECT rect-3 SIZE 93 BY 7.0.

DEF    QUERY qry-cbPick FOR cbkmf SCROLLING.
DEF BROWSE brw-cbPick QUERY qry-cbPick
     DISPLAY cbkmf.Acb cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-trans FOR cbtype SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY cbtype.TranType cbtype.descrip cbtype.DrCr WITH 10 DOWN SEPARATORS.


DEFINE FRAME frm-trans 
     brw-trans AT ROW 1 COL 1.5 skip(0.5)
     btn-Ok COLON 8 SPACE (20) btn-close 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Type Selection".

FORM SPACE(5) bfCbTrans.Descrip 
     bfCbTrans.amount
     bfCbTrans.Accper 
     bfCbTrans.trDate 
     bfCbTrans.Ref
    HEADER skip(2) "BANK RECONCILIATION STATEMENT:" AT 5 SPACE(2) "Page: " AT 65 TRIM(STRING(PAGE-NUMBER(a)))
     SKIP(1)
     "BANK:- " AT 5 wsTitle SKIP
     wsTitle1   AT 5 SKIP(1) "UNRECONCILED ITEMS" AT 5 "PERIOD TR-DATE  REFERENCE" AT 47 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    /*WIDTH 132 */ FRAME frm-rpt.

DEFINE FRAME f1
     SKIP(1.0)
    bfrcbkmf.bank COLON 22 VIEW-AS FILL-IN LABEL "Bank"
    bfrcbkmf.descrip VIEW-AS TEXT NO-LABEL SKIP(1)
    bfrcbkmf.StatNo  COLON 22 VIEW-AS TEXT LABEL "Statement Number"
    bfrcbkmf.StatBal COLON 70 VIEW-AS TEXT LABEL "Opening Balance" 
    wsPer            COLON 22 FORM "999999" LABEL "Reconcile to Period"
    wsBal            COLON 70  LABEL "Closing Balance" SKIP(1)
    wsOpt COLON 22 LABEL "Select Option" VIEW-AS RADIO-SET 
                          RADIO-BUTTONS "Load Bank Statement", 1,
                                        "Run Bank Reconciliation", 2 SKIP(0.5)
    bfrBnkStat.recseq COLON 50 LABEL "Processing..." VIEW-AS TEXT SKIP(1.0)
    bfrBnkStat.Ref    COLON 20  FORM "x(20)" VIEW-AS text
    bfrBnkStat.Debit  VIEW-AS text
    bfrBnkStat.Credit  VIEW-AS TEXT SKIP(0.5)
    bfrBnkStat.trDate COLON 20 LABEL "Date" VIEW-AS TEXT SKIP(0.5)
    bfrBnkStat.Narrat COLON 20 LABEL "Narration" VIEW-AS TEXT SKIP(0.5)
    wsAdd             COLON 30 LABEL "Add Transaction? (Y/N)" SKIP(0.5)
    btnTrans          COLON 10 LABEL "TRANSACTION TYPE" NO-TAB-STOP cbtype.TranType NO-LABEL 
    cbtype.descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)           
    rect-1 AT ROW 1 COLUMN 4
    rect-3 AT ROW 9.8 COLUMN 4
    rect-2 AT ROW 17 COLUMN 4
    btn-ok AT ROW 18 COLUMN 20 SPACE(40) btn-exit
    WITH SIZE 100 BY 20.5 SIDE-LABEL
     TITLE "Auto-Bank Reconciliations" VIEW-AS DIALOG-BOX.

DEFINE FRAME f2
     SKIP(1.5)
    "***************** loading User Input Required *********" COLON 20 SKIP(1)
    bfrBnkStat.Ref    COLON 30 FORM "x(20)" VIEW-AS text
    bfrBnkStat.Debit  VIEW-AS text
    bfrBnkStat.Credit  VIEW-AS TEXT SKIP(0.5)
    bfrBnkStat.Narrat COLON 30 LABEL "Previous Narration" VIEW-AS TEXT SKIP(0.5)
    wsNar             COLON 30 LABEL "Another Narration" VIEW-AS TEXT SKIP(0.5)
    wsAdd             COLON 30 LABEL "Update Narration (Y/N)" SKIP(1)
    WITH SIZE 100 BY 14.5 SIDE-LABEL
     TITLE "Auto-Bank Reconciliations" VIEW-AS DIALOG-BOX.

DEFINE FRAME f3
     SKIP(1.5)
    "***************** loading User Input Required *********" COLON 20 SKIP(1)
    bfrBnkStat.Ref    COLON 10 VIEW-AS text
    bfrBnkStat.Debit  VIEW-AS text
    bfrBnkStat.Credit  VIEW-AS TEXT SKIP(0.5)
    bfrBnkStat.trDate COLON 20 LABEL "Date" VIEW-AS TEXT SKIP(0.5)
    bfrBnkStat.Narrat COLON 20 LABEL "Narration" VIEW-AS TEXT SKIP(0.5)
    btnTrans COLON 3 NO-LABEL NO-TAB-STOP cbtype.TranType NO-LABEL 
    cbtype.descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)           
    wsAdd             COLON 20 LABEL "Add Transaction? (Y/N)" SKIP(2)
    btn-Close COLON 40 LABEL "TERMINATE"
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED SIZE 100 BY 14.5
     TITLE "Create transactions from Bank Statement".

DEFINE FRAME frm-allocate 
     SKIP(1)
    wsTotal  COLON 80 LABEL "Amount to be allocated" VIEW-AS TEXT
    SKIP(1)
    bfCbTrans.Descrip COLON 15 LABEL "Narration" VIEW-AS TEXT SIZE 50 BY 1 SKIP(0.5)
    btnLedger COLON 5 NO-LABEL
    bfCbTrans.Ledger  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
    btnDept                 COLON 5  no-tab-stop
    bfCbTrans.Dept              NO-LABEL
    SPACE(1) glDept.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 5  no-tab-stop
    bfCbTrans.Proj              NO-LABEL
    SPACE(1) glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(0.5)
    /*btnAcb          COLON 1 NO-LABEL NO-TAB-STOP
    bfCbTrans.Acb     NO-LABEL
    cbkmf.descrip   NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5) */
    bfCbTrans.amount  COLON 20 LABEL "Amount" SKIP(2)
    btn-Close COLON 40
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ALLOCATIONS".

DEFINE FRAME frm-cbPick 
     brw-cbPick AT ROW 1 COL 1.5 skip(0.5)
     btn-Ok COLON 8 SPACE (20) btn-Close 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Cashbook Selection".

FORM 
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE JOURNAL CAPTURE AND UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.


/********* Triggers **************/
ON 'enter':U OF bfrcbkmf.bank IN FRAME f1
    OR 'TAB' OF bfrcbkmf.bank IN FRAME f1
DO:
    FIND FIRST bfrcbkmf WHERE bfrcbkmf.bank = INT(bfrcbkmf.bank:SCREEN-VALUE)
         SHARE-LOCK NO-ERROR.
    IF AVAILABLE bfrcbkmf THEN DO:
        IF LOCKED  bfrcbkmf THEN MESSAGE "Record LOCKED" VIEW-AS ALERT-BOX.
        DISPLAY bfrcbkmf.descrip bfrcbkmf.StatBal bfrcbkmf.StatNo + 1  @ bfrcbkmf.StatNo WITH FRAME f1.
        wsAmt = bfrcbkmf.StatBal.
        APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON 'enter':U OF wsPer IN FRAME f1
    OR 'TAB' OF wsPer IN FRAME f1
DO:
    OPEN QUERY q1 FOR EACH bfCbTrans WHERE bfCbTrans.bank = bfrcbkmf.bank
                               AND bfCbTrans.StatNo = 0 AND bfCbTrans.AccPer <= INT(wsPer:SCREEN-VALUE).

     APPLY 'tab' TO SELF.
    RETURN.
END.

ON 'enter':U OF wsOpt IN FRAME f1
    OR 'tab':U OF wsOpt IN FRAME f1
DO:
    ASSIGN wsOpt wsBal wsPer
            wsObal = bfrcbkmf.StatBal
            wsBank = INT(bfrcbkmf.bank:SCREEN-VALUE)
            st-per = wsper.
    IF wsOpt = 2 THEN DO:
        btn-Ok:LABEL = "RECONCILE".
        APPLY 'entry' TO btn-Ok.
    END.
    ELSE DO:
        btn-Ok:LABEL = "LOAD".
        APPLY 'entry' TO btn-Ok.
    END.
    RETURN. 
END.

ON 'choose':U OF btn-Ok IN FRAME f1
DO:
    IF btn-ok:LABEL = "RECONCILE" THEN DO:
        RUN RECONCILE.
    END.
        
    ELSE IF btn-ok:LABEL = "LOAD" THEN
    DO:
        RUN load-file.
    END.
    RETURN.
END.

ON 'enter':U OF wsAdd IN FRAME f1 
   OR 'tab':U OF wsAdd IN FRAME f1
DO:
    ASSIGN wsAdd.
    IF wsAdd = YES THEN
       APPLY "entry" TO cbtype.TranType IN FRAME f1.
    ELSE
       APPLY "tab" TO cbtype.TranType IN FRAME f1.
END.

ON CHOOSE OF btn-close IN FRAME f3 
DO:
   APPLY 'close' TO THIS-PROCEDURE.
   APPLY 'close' TO THIS-PROCEDURE IN FRAME f1.
END.
ON 'enter':U OF wsAdd IN FRAME f2 
   OR 'tab':U OF wsAdd IN FRAME f2
DO:
    RETURN.
END.
/******* Update Triggers *******/
/*ON CHOOSE OF btnAcb IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-cbPick.
  OPEN QUERY qry-cbPick FOR EACH cbkmf WHERE cbkmf.Acb <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-cbPick.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-cbPick 
          OR close of THIS-PROCEDURE IN FRAME frm-cbPick
          OR CHOOSE OF btn-ok IN FRAME frm-cbPick 
          OR 'enter':u OF brw-cbPick
          OR 'mouse-select-dblclick' OF brw-cbPick.
  CLOSE QUERY qry-cbPick.
  HIDE FRAME frm-cbPick.
  APPLY 'tab' TO btnAcb.
  APPLY 'tab' TO bfCbTrans.Acb.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-cbPick 
    OR 'enter':u OF brw-cbPick
    OR 'mouse-select-dblclick' OF brw-cbPick
DO: 
   GET CURRENT qry-cbPick NO-WAIT.
   DISPLAY cbkmf.Acb @ bfCbTrans.Acb cbkmf.DESCRIP WITH FRAME frm-allocate.
   APPLY 'TAB' TO bfCbTrans.Acb IN FRAME frm-allocate.
END.

ON 'tab' of bfCbTrans.Acb IN FRAME frm-allocate
    OR 'enter' OF bfCbTrans.Acb IN FRAME frm-Allocate
DO:
    IF  INT(bfCbTrans.Acb:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cbkmf WHERE cbkmf.Acb = int(bfCbTrans.Acb:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE
        DISPLAY cbkmf.Acb @ bfCBTrans.Acb cbkmf.DESCRIP WITH FRAME frm-Allocate.
    END.
END.
*/
ON CHOOSE OF btnLedger IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-Pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pick.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-Pick 
          OR close of THIS-PROCEDURE IN FRAME frm-Pick
          OR CHOOSE OF btn-ok IN FRAME frm-Pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-Pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO bfCBTrans.Ledger.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  bfCBTrans.Ledger glmf.DESCRIPTION WITH FRAME frm-allocate.
   APPLY 'TAB' TO  bfCBTrans.Ledger IN FRAME frm-allocate.
END.

ON 'tab' of  bfCBTrans.Ledger IN FRAME frm-allocate
    OR 'enter' OF  bfCBTrans.Ledger IN FRAME frm-Allocate
DO:
    IF  DEC( bfCBTrans.Ledger:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid ledger entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.Acct = DEC(bfCBTrans.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  bfCBTrans.Ledger glmf.DESCRIPTION WITH FRAME frm-Allocate.
            IF glmf.dept <> 0 THEN DO:
                DISPLAY glmf.dept @ bfCbTrans.dept WITH FRAME frm-Allocate.
                DISABLE btndept bfCbTrans.dept WITH FRAME frm-Allocate.
            END.
        END.
    END.
END.

ON CHOOSE OF btnTrans IN FRAME f1
DO:
  VIEW FRAME frm-Trans.
  OPEN QUERY qry-Trans FOR EACH cbtype NO-LOCK.
  ENABLE ALL WITH FRAME frm-Trans.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-Trans 
          OR close of THIS-PROCEDURE IN FRAME frm-Trans
          OR CHOOSE OF btn-ok IN FRAME frm-Trans 
          OR 'enter':u OF brw-Trans
          OR 'mouse-select-dblclick' OF brw-Trans.
  CLOSE QUERY qry-Trans.
  HIDE FRAME frm-Trans.
  APPLY 'tab' TO btnTrans.
  /*APPLY 'tab' TO cbtype.TranType. */
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Trans 
    OR 'enter':u OF brw-Trans
    OR 'mouse-select-dblclick' OF brw-Trans
DO: 
   GET CURRENT qry-Trans EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbtype.TranType cbtype.descrip WITH FRAME f1.
  /* APPLY 'TAB' TO cbtype.TranType IN FRAME f1. */
END.

ON 'tab' OF cbtype.TranType IN FRAME f1
    OR 'enter' OF cbtype.TranType IN FRAME f1
DO:
    IF wsAdd = NO THEN NEXT.
    FIND FIRST cbtype WHERE cbtype.TranType = int(cbtype.TranType:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAIL cbtype THEN DO:
       MESSAGE "Invalid Transaction Type entered" VIEW-AS ALERT-BOX.
       CLEAR FRAME frm-Pay ALL.
       RETURN NO-APPLY.
    END.
    ELSE DO:
       DISPLAY cbtype.TranType cbtype.descrip WITH FRAME f1.
        ASSIGN wsF = 1  WHEN cbtype.DrCr = "D"
               wsF = -1 WHEN cbtype.DrCr = "C".
       ASSIGN wsTotal = bfrBnkStat.Debit * -1 WHEN  bfrBnkStat.Debit <> 0
              wsTotal = bfrBnkStat.Credit     WHEN  bfrBnkStat.Credit <> 0.
       RUN allocate-ip.
    END.
END.

ON 'tab' OF bfCbTrans.Amount IN FRAME frm-Allocate
    OR 'enter' OF bfCbTrans.Amount IN FRAME frm-Allocate
DO:
  IF dec(bfCbTrans.Amount:SCREEN-VALUE) >= 0.00 
         AND dec(bfCbTrans.Amount:SCREEN-VALUE) <= wsTotal THEN DO:
      wsTotal = wsTotal - dec(bfCbTrans.Amount:SCREEN-VALUE).
      CREATE tmpTrans.
      ASSIGN tmpTrans.bank     = wsBank
             tmpTrans.seq      = x
             tmpTrans.txtcur   = bfrcbkmf.txtCur
             tmpTrans.decRate  = wsRate
             tmpTrans.Accper   = st-per
             tmpTrans.amount   = dec(bfCbTrans.Amount:SCREEN-VALUE)      WHEN bfrbnkstat.Credit <> 0
             tmpTrans.amount   = dec(bfCbTrans.Amount:SCREEN-VALUE) * -1 WHEN bfrbnkstat.Debit  <> 0
             tmpTrans.Descrip  = bfrbnkstat.Narrat
             tmpTrans.Ledger   = DEC(bfCbTrans.ledger:SCREEN-VALUE)
             tmpTrans.dept     = DEC(bfCbTrans.dept:SCREEN-VALUE)
             tmpTrans.Proj   = DEC(bfCbTrans.Proj:SCREEN-VALUE) 
             tmpTrans.OpCode   = varUser
             tmpTrans.Ref      = bfrBnkstat.Ref
             tmpTrans.TransID  = wsTransId
             tmpTrans.TranType = int(cbtype.TranType:SCREEN-VALUE IN FRAME f1)
             tmpTrans.trDate   = bfrBnkstat.trdate
             tmpTrans.UID      = varUser
             tmpTrans.PoDate   = TODAY
             tmpTrans.prog     = "bnkrec.p"
             tmpTrans.UID2     = varUser.
      IF wsTotal > 0.00  THEN DO:
         DISABLE btn-close WITH FRAME frm-allocate.
         /* CLEAR FRAME frm-Allocate. */
         DISPLAY wsTotal WITH FRAME frm-Allocate.
         APPLY 'entry' TO bfCbTrans.ledger IN FRAME frm-Allocate.
         RETURN NO-APPLY.
      END.
      ELSE
          RUN update-ip.
      HIDE FRAME frm-allocate.
      APPLY 'close' TO THIS-PROCEDURE.
  END.
  ELSE DO:
      MESSAGE "Amount enter is either less than 0.00 or Greater than " wsTotal
              SKIP "Please re-enter Amount up to" wsTotal VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
  END.   
END.
ON 'choose':U OF btn-exit IN FRAME f1 
DO:
    APPLY "ESC" TO FRAME f1.
    APPLY "ESC" TO FRAME f1. 
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
ASSIGN wsTime    = STRING(TIME,"HH:MM:SS")
       wsTransID = DEC (string(YEAR(TODAY),"9999")
                 + string(MONTH(TODAY),"99" )
                 + string(DAY(TODAY),"99")
                 + SUBSTR(STRING(wsTIME),1,2) 
                 + SUBSTR(STRING(wsTIME),4,2) 
                 + SUBSTR(STRING(wsTIME),7,2) )
      wsbal:SCREEN-VALUE = "0.00".
ENABLE ALL WITH FRAME f1.
WAIT-FOR CHOOSE OF btn-exit OR CLOSE OF THIS-PROCEDURE.
RELEASE bfrCBKmf.
RELEASE bfCbTrans.
RELEASE bfrBnkstat.
HIDE FRAME f1.

PROCEDURE Load-file:
    wsFile = "".
    SYSTEM-DIALOG GET-FILE wsFile SAVE-AS TITLE "Select File ...". 
    IF wsFile = "" THEN DO:
         RETURN NO-APPLY.
    END.
    INPUT STREAM b FROM VALUE(wsFile).
    IMPORT STREAM b DELIMITER "," tdate wsref wsNar Vdate wsDr wsCr dummy.
    IF wsPer <> INT(STRING(YEAR(tdate)) + STRING(MONTH(tDate),"99")) THEN DO:
       MESSAGE "Transaction dates in the file"
           SKIP
           " do not match reconcilitaion Period " VIEW-As ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        HIDE FRAME f1.
        wsPer = INT(STRING(YEAR(tdate)) + STRING(MONTH(tDate),"99")).
        FIND FIRST bfrBnkStat WHERE bfrBnkStat.trDate  = Vdate 
                                 AND bfrBnkStat.bank    = wsBank
                                 AND bfrBnkStat.Ref     = wsRef
                                 AND bfrBnkStat.Credit = wsCr
                                 AND bfrBnkStat.Debit  = wsDr
                       AND bfrBnkStat.period = wsPer NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfrbnkStat THEN DO: 
            CREATE bfrBnkStat.
            ASSIGN bfrBnkStat.bank    = wsBank
                   bfrBnkStat.period  = wsPer
                   bfrBnkStat.trDate  = Vdate
                   bfrBnkStat.Ref     = wsRef
                   bfrBnkStat.Narrat  = wsNar
                   bfrBnkStat.Debit   = wsDr
                   bfrBnkStat.Credit  = wsCr
                   bfrBnkStat.RecSeq  = X
                   X = X + 1.
       END.
        REPEAT :
            IMPORT STREAM b DELIMITER "," tdate wsref wsNar Vdate wsDr wsCr dummy.
            IF tdate = ? THEN DO:
                FIND LAST bfrBnkStat WHERE bfrBnkStat.trDate  = Vdate1 
                                          AND bfrBnkStat.bank    = wsBank
                                          AND bfrBnkStat.Ref     = wsRef1
                                          AND bfrBnkStat.Credit = wsCr1
                                          AND bfrBnkStat.Debit  = wsDr1 EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAILABLE bfrBnkStat THEN DO:
                         DISPLAY bfrBnkStat.Ref bfrBnkStat.Narrat wsNar 
                                 bfrBnkStat.Debit  bfrBnkStat.Credit wsAdd WITH FRAME f2.
                         UPDATE wsAdd WITH FRAME f2.
                         IF wsAdd = YES THEN
                             bfrBnkStat.Narrat = wsNar.
                   END.
            END.     
            ELSE DO:
                IF wsDr = wsCr THEN NEXT.
                wsPer = INT(STRING(YEAR(tdate)) + STRING(MONTH(tDate),"99")).
                CREATE bfrBnkStat.
                ASSIGN bfrBnkStat.bank    = wsBank
                       bfrBnkStat.period  = wsPer
                       bfrBnkStat.trDate  = Vdate
                       bfrBnkStat.Ref     = wsRef
                       bfrBnkStat.Narrat  = wsNar
                       bfrBnkStat.Debit   = wsDr
                       bfrBnkStat.Credit  = wsCr
                       bfrBnkStat.RecSeq  = X
                       X = X + 1.
            END.
            ASSIGN VDate1 = Vdate
                   wsRef1 =  wsRef
                   wsDr1 = wsDr
                   wsCr1 = wsCr.
        END.
    END.
    HIDE FRAME f2.
    VIEW FRAME f1.
   /* APPLY 'CLOSE' TO THIS-PROCEDURE. */
END.

PROCEDURE Reconcile:
DO X = 1 TO 3:
    IF X = 1 THEN DO: /* Pass 1 */
       FOR EACH bfrBnkStat WHERE bfrBnkStat.bank = wsBank AND bfrBnkStat.recon <> "Y" 
           AND bfrBnkStat.period <= wsPer BY bfrBnkStat.period BY bfrBnkStat.recseq:
           DISPLAY bfrBnkStat.recseq WITH FRAME f1.
            ASSIGN wsAmt =  bfrBnkStat.Debit WHEN  bfrBnkStat.Debit <> 0 
                   wsAmt =  bfrBnkStat.Credit WHEN  bfrBnkStat.Credit <> 0.
           ASSIGN  wsdate  = trdate
                   wsdate1 = trdate.
           IF WEEKDAY(wsdate + 1) = 1 OR WEEKDAY(wsdate + 1) = 7 THEN
                    wsdate = wsdate + 3.
           ELSE
                   wsdate = wsdate + 1.
           IF WEEKDAY(wsdate1 - 1) = 1 OR WEEKDAY(wsdate1 - 1) = 7 THEN
                    wsdate = wsdate - 3.
           ELSE
                   wsdate1 = wsdate1 - 1.
           FIND FIRST  bfCbTrans WHERE bfCbTrans.bank   = bfrBnkStat.bank
                                  AND bfCbTrans.Accper = bfrBnkStat.period
                                  AND (bfCbTrans.trDate = bfrBnkStat.trDate
                                       OR bfCbTrans.trDate = wsDate1
                                       OR bfCbTrans.trDate = wsDate )
                                  AND  bfCbTrans.ref   = bfrBnkStat.Ref
                                  AND bfCbTrans.Amount = wsAmt 
                                  AND bfCbTrans.Recon <> "Y"  NO-ERROR.
            IF AVAILABLE bfCbTrans THEN 
                ASSIGN bfCbTrans.Recon = "Y"
                       bfrBnkstat.Recon = "Y".
       END.
    END.
    IF X = 2 THEN DO: /* Pass 2 - less reference */
        FOR EACH bfrBnkStat WHERE bfrBnkStat.bank = wsBank AND bfrBnkStat.recon <> "Y" 
             AND bfrBnkStat.period <= wsPer BY bfrBnkStat.period BY bfrBnkStat.recseq:
            ASSIGN wsAmt =  bfrBnkStat.Debit WHEN  bfrBnkStat.Debit <> 0 
                   wsAmt =  bfrBnkStat.Credit WHEN  bfrBnkStat.Credit <> 0.
           FIND FIRST  bfCbTrans WHERE bfCbTrans.bank   = bfrBnkStat.bank
                                  AND bfCbTrans.Accper   = bfrBnkStat.period
                                  AND (bfCbTrans.trDate = bfrBnkStat.trDate
                                       OR bfCbTrans.trDate = wsDate1
                                       OR bfCbTrans.trDate = wsDate )
                                  /*AND  bfCbTrans.Descrip = bfrBnkStat.Narrat */
                                  AND bfCbTrans.Amount = wsAmt 
                                  AND bfCbTrans.Recon <> "Y"  NO-ERROR.
            IF AVAILABLE bfCbTrans THEN 
                ASSIGN bfCbTrans.Recon = "Y"
                       bfrBnkstat.Recon = "Y".
       END.
    END.
    IF X = 3 THEN DO: /* Pass 3 Create transactions from Bank statment */
        /*HIDE FRAME f1.
        VIEW FRAME f3. */
        /*ENABLE btnTrans cbtype.TranType wsAdd btn-Close WITH FRAME f3. */
        FOR EACH bfrBnkStat WHERE bfrBnkStat.bank = wsBank AND bfrBnkStat.recon = "" 
             AND bfrBnkStat.period <= wsPer BREAK BY bfrBnkStat.period BY bfrBnkStat.recseq:
            DISPLAY bfrBnkStat.Ref bfrBnkStat.Debit bfrBnkStat.Credit bfrBnkStat.trDate
                 bfrBnkStat.Narrat wsAdd  WITH FRAME f1.
            APPLY 'entry' TO wsAdd IN FRAME f1.
            WAIT-FOR CHOOSE OF btn-exit IN FRAME f1
                OR 'enter' OF cbtype.TranType IN FRAME f1
                OR 'tab' OF cbtype.TranType IN FRAME f1
                OR close of THIS-PROCEDURE IN FRAME f1.
            /*HIDE FRAME f3.
            VIEW FRAME f1. */
       END.
      MESSAGE "Reconciliation completed....,"
                   VIEW-AS ALERT-BOX.
      APPLY 'close' TO THIS-PROCEDURE IN FRAME f1.
    END. /* eof...x = 3 */
  /*  IF X = 4 THEN DO: /*display in browser and update  - this section not complete*/
        FOR EACH bfrBnkStat WHERE bfrBnkStat.bank = wsBank AND bfrBnkStat.recon <> "Y" 
             AND bfrBnkStat.period <= wsPer BY bfrBnkStat.period BY bfrBnkStat.recseq:
            ASSIGN wsAmt =  bfrBnkStat.Debit WHEN  bfrBnkStat.Debit <> 0 
                   wsAmt =  bfrBnkStat.Credit WHEN  bfrBnkStat.Credit <> 0.
           FIND FIRST  bfCbTrans WHERE bfCbTrans.bank   = bfrBnkStat.bank
                                  AND bfCbTrans.Accper = bfrBnkStat.period
                                  /*AND bfCbTrans.trDate = bfrBnkStat.trDate
                                  AND  bfCbTrans.ref   = bfrBnkStat.Ref */
                                  AND bfCbTrans.Amount = wsAmt 
                                  AND bfCbTrans.Recon <> "Y"  NO-ERROR.
            IF AVAILABLE bfCbTrans THEN
            DO:
               DISPLAY bfCbTrans.ref bfrBnkStat.Ref bfCbTrans.Amount wsAmt 
                bfCbTrans.Recon bfCbTrans.Accper bfCbTrans.trDate WITH WIDTH 200.
                UPDATE bfCbTrans.Recon.
                bfrBnkStat.Recon = bfCbTrans.Recon.
            END.
       END.   
       MESSAGE  "Pass 4 not yet ready......" VIEW-AS ALERT-BOX.
    END.*/
END.
END.
/* Check if all Bank statement transaction are marked */
FIND FIRST bfrBnkStat WHERE bfrBnkStat.bank = wsBank AND bfrBnkStat.recon <> "Y" 
     AND bfrBnkStat.period <= wsPer NO-ERROR.
IF NOT AVAILABLE bfrBnkStat THEN DO:
    FIND FIRST bfrcbkmf WHERE bfrcbkmf.bank = wsBank SHARE-LOCK NO-ERROR.
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        ASSIGN wsOBal = bfrcbkmf.StatBal.
        wsAmt = wsOBal.
        FOR EACH bfCbTrans WHERE bfCbTrans.bank = bfrcbkmf.bank AND bfCbTrans.Recon = "Y"
                           AND bfCbTrans.StatNo = 0: 
              wsAmt = wsAmt + bfCbTrans.amount.
        END.
       IF wsAmt = DEC(wsBal:SCREEN-VALUE) THEN DO:
               MESSAGE "Statement has reconcilled " wsAmt VIEW-AS ALERT-BOX.
               FOR EACH bfCbTrans WHERE bfCbTrans.bank = bfrcbkmf.bank AND bfCbTrans.Recon = "Y"
                                     AND bfCbTrans.StatNo = 0:
                    ASSIGN bfCbTrans.Recon = "U"
                           bfCbTrans.StatNo = bfrcbkmf.StatNo + 1.
               END.
               ASSIGN bfrcbkmf.StatBal = DEC(wsBal:SCREEN-VALUE)
                      bfrcbkmf.StatNo = bfrcbkmf.StatNo + 1.
               CREATE cbrec.
               ASSIGN cbrec.bank    = bfrcbkmf.bank
                      cbrec.Statno  = bfrcbkmf.StatNo
                      cbrec.closeBal = bfrcbkmf.StatBal
                      cbrec.OpenBal  = wsOBal
                      cbrec.per      = INT(wsPer:SCREEN-VALUE).
               /*{PrintOpt.i &stream-name="stream a"
                        &print-prog="Report.ip"
                        &paged} */
       END.
       ELSE DO:
             MESSAGE "Statement does NOT reconcile " wsAmt " :" DEC(wsBal:SCREEN-VALUE)
                  VIEW-AS ALERT-BOX. 
       END.  
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.
END.

PROCEDURE Report.ip: 
    wsTitle = bfrCBkmf.descrip.
    wsTitle1 = "STATEMENT NUMBER: " + STRING(cbrec.StatNo) 
             + "                       ENDING PERIOD: " + STRING(cbrec.per).
    FOR EACH bfCbTrans WHERE bfCbTrans.Recon <> "U" AND bfCbTrans.StatNo = 0
                           AND bfCbTrans.Accper <= cbrec.per AND bfCbTrans.bank = cbrec.bank:
        DISPLAY STREAM a bfCbTrans.Accper bfCbTrans.trDate bfCbTrans.Ref bfCbTrans.Descrip 
        bfCbTrans.amount WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        wsUnrec = wsUnrec + bfCbTrans.amount.
    END.
    UNDERLINE STREAM a bfCbTrans.amount WITH FRAME frm-rpt.
    DISPLAY STREAM a wsUnRec @ bfCbTrans.amount WITH FRAME frm-rpt.
    DOWN STREAM a 5  WITH FRAME frm-rpt.
    DISPLAY STREAM a "BALANCE RECONCILITAION" @ bfCbTrans.Descrip WITH FRAME frm-rpt.
    UNDERLINE STREAM a bfCbTrans.Descrip WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    wsCBTotal = cbrec.closebal + wsUnrec.
    DISPLAY STREAM a "BALANCE AS PER BANK STATEMENT" @ bfCbTrans.Descrip cbrec.closebal @ bfCbTrans.amount WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    DISPLAY STREAM a "UNRECONCILED ITEMS" @ bfCbTrans.Descrip wsUnrec @ bfCbTrans.amoun WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    DISPLAY STREAM a "BALANCE AS PER OUR CASHBOOK" @ bfCbTrans.Descrip wsCBTotal @ bfCbTrans.amoun WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

PROCEDURE allocate-ip:
  CLEAR FRAME frm-Allocate.
  VIEW FRAME frm-Allocate.
  ENABLE ALL EXCEPT btn-close WITH FRAME frm-Allocate.
  DISPLAY wsTotal bfrbnkstat.Narrat @ bfCbTrans.Descrip WITH FRAME frm-Allocate.
  APPLY 'entry' TO bfCbTrans.ledger IN FRAME frm-Allocate.
  /*IF wspay = NO THEN
    APPLY 'entry' TO bfCbTrans.ledger IN FRAME frm-Allocate.
  ELSE IF wsPay = YES THEN DO:
      DISABLE btnLedger bfCbTrans.ledger WITH FRAME frm-Allocate.
      APPLY 'entry' TO bfCbTrans.acb IN FRAME frm-Allocate.
  END.*/
  WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-Allocate.
  RETURN.
END.

PROCEDURE Update-ip:
    ASSIGN wsTotal = 0
           wsMonth = INT(SUBSTR(STRING(bfrbnkstat.period),5,2))
           wsYear  = INT(SUBSTR(STRING(bfrbnkstat.period),1,4)).
   FIND FIRST cbkmf WHERE cbkmf.bank = bfrbnkstat.bank EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN cbkmf.bal = cbkmf.bal + bfrbnkstat.Debit + bfrbnkstat.Credit.
   FIND FIRST tblForex WHERE tblForex.txtCur = bfrcbkmf.txtCur AND  bfrBnkstat.trDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
   IF AVAILABLE tblForex THEN DO:
      ASSIGN wsRate = tblForex.decRate. 
   END.
   ELSE IF NOT AVAILABLE tblForex  THEN DO:
        FIND FIRST tblForexH WHERE tblForexH.txtCur = bfrcbkmf.txtCur AND tblForexH.dtRate <=  bfrBnkstat.trDate NO-LOCK NO-ERROR.
            IF AVAILABLE tblForexH THEN DO:
               ASSIGN wsRate = tblForexH.decRate.
             END.
     END.
   CREATE bfcbTrans. /* Create Bank transaction */
   X = 0.
   ASSIGN bfCBTrans.Accper = st-per
          bfCBTrans.amount = bfCBTrans.amount + bfrbnkstat.Debit + bfrbnkstat.Credit
          bfCBTrans.bank   = bfrBnkstat.bank
          bfCBTrans.Descrip = bfrBnkstat.Narrat
          bfCBTrans.OpCode  = varUser
          bfCBTrans.Ref     = bfrBnkstat.Ref
          bfCBTrans.TransID = wsTransId
          bfCBTrans.TranType = INT(cbtype.TranType:SCREEN-VALUE IN FRAME f1)
          bfCBTrans.trDate   = bfrBnkstat.trDate
          bfCBTrans.ledger   = 99
          bfCBTrans.Recon    = "Y"
          bfCBTrans.seq     = X
          bfCBTrans.txtCur  = bfrcbkmf.txtCur
          bfCBTrans.decRate = wsRate
          bfrBnkstat.Recon   = "Y".
          
   FOR EACH tmpTrans WHERE tmpTrans.TransID = wsTransId BREAK BY tmpTrans.bank: /*  Create Acb transaction */
       IF FIRST-OF(tmpTrans.bank) THEN DO:
           FIND FIRST cbkmf WHERE cbkmf.bank = tmpTrans.bank EXCLUSIVE-LOCK NO-ERROR.
           FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-ERROR.
           wsTotal = 0.
           wsFund = glmf.fund.
       END.
       CREATE bfCbTrans.
       X = X + 1. 
       BUFFER-COPY tmpTrans TO bfCbTrans.
       bfCBTrans.seq     = X.
       wsTotal = wsTotal + tmpTrans.Amount.
       FIND FIRST dbgl WHERE dbgl.acct = tmpTrans.Ledger AND dbgl.proj = tmpTrans.Proj
                  AND dbgl.dept     = tmpTrans.dept no-error. /* create record for ledger */
       IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl.
          ASSIGN  dbgl.CREDATE = TODAY
                  dbgl.DESCRIP  = bfrbnkstat.Narrat
                  dbgl.period   = bfrbnkstat.period
                  dbgl.REF      = bfrbnkstat.Ref
                  dbgl.TransID  = wsTransId
                  dbgl.trDATE   = bfrbnkstat.trDate
                  dbgl.UID      = varUser
                  dbgl.acct     = tmpTrans.Ledger
                  dbgl.proj     = tmpTrans.Proj
                  dbgl.dept     = tmpTrans.dept
                  dbgl.SOURCE   = "CB".
       END.
       ASSIGN dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * -1 * wsRate).
       /*FIND FIRST cbkmf WHERE cbkmf.acb = tmpTrans.acb NO-ERROR.*/ 
       FIND FIRST dbgl WHERE dbgl.acct = cbkmf.Ledger NO-ERROR.
       IF NOT AVAILABLE dbgl THEN DO:
            /* MESSAGE tmpTrans.acb  cbkmf.Ledger VIEW-AS ALERT-BOX. */
          CREATE dbgl.
          ASSIGN  dbgl.CREDATE = TODAY
                  dbgl.DESCRIP  = bfrbnkstat.Narrat
                  dbgl.period   = bfrbnkstat.period
                  dbgl.REF      = bfrbnkstat.Ref
                  dbgl.TransID  = wsTransId
                  dbgl.trDATE   = bfrbnkstat.trDate
                  dbgl.UID      = varUser
                  dbgl.acct     = cbkmf.Ledger
                  dbgl.proj     = 0
                  dbgl.dept     = 2
                  dbgl.SOURCE   = "CB".
       END.
       ASSIGN dbgl.AMT  = dbgl.AMT   + (tmpTrans.Amount * wsRate).
             /* cbkmf.bal =  cbkmf.bal + tmpTrans.Amount.*/
       FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Ledger NO-ERROR.
       IF glmf.fund <> wsFund AND SIMCTR.FUNDACC = YES THEN DO: /*Interfund Transaction */
           DO X = 1 TO 2:
               ASSIGN wsLedger = DEC(string(wsFund) + "00" + string(SIMCTR.FUNDDb)) WHEN X = 1
                      wsLedger = DEC(string(glmf.fund) + "00" + string(SIMCTR.FUNDCr)) WHEN X = 2.
               FIND FIRST dbgl WHERE dbgl.acct = wsLedger no-error.
               IF NOT AVAILABLE dbgl THEN DO:
                  CREATE dbgl.
                  ASSIGN dbgl.CREDATE = TODAY
                         dbgl.DESCRIP  = "Resource advance: EX#" + STRING(wsFund)
                                       + "/TO" + string(glmf.fund) 
                         dbgl.period   = bfrbnkstat.period
                         dbgl.REF      = bfrbnkstat.Ref
                         dbgl.TransID  = wsTransId
                         dbgl.trDATE   = bfrbnkstat.trDate
                         dbgl.UID      = varUser
                         dbgl.acct     = wsLedger
                         dbgl.acct     = 0
                         dbgl.proj     = 0
                         dbgl.SOURCE   = "CB".
               END.
               ASSIGN dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsRate) WHEN X = 2
                      dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * -1 * wsRate) WHEN X = 1.
           END.
       END.
       DELETE tmpTrans.
   END. /* eof for each tmptrans */
   /* Consolidate to GL */
   FOR EACH dbgl WHERE dbgl.TransID = wsTransID AND  dbgl.SOURCE = "CB":
        CREATE gltdf.
        BUFFER-COPY dbgl TO gltdf.
        /*ASSIGN gltdf.SOURCE = "CB". */
    /* Ledger Balances */
        FIND FIRST glbal WHERE GLBAL.acct = dbgl.acct 
                           AND GLBAL.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glbal THEN DO:
            CREATE glbal.
            ASSIGN glbal.acct = dbgl.acct
                   glbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
        END.
        ASSIGN glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
              glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
    /* Fund Balances */
        FIND FIRST glfbal WHERE GLfBal.fund = dbgl.fund AND glfbal.acct = dbgl.acct 
                           AND glfbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glfbal THEN DO:
            CREATE glfbal.
            ASSIGN glfbal.acct = dbgl.acct
                   glfbal.fund = dbgl.fund
                   glfbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
        END.
        ASSIGN glfbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
               glfbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
    /* Project Balances */
        FIND FIRST glPbal WHERE GLPBal.proj = dbgl.proj AND GLPBal.acct = dbgl.acct 
                           AND GLPBal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glPbal THEN DO:
            CREATE glPbal.
            ASSIGN glPbal.acct = dbgl.acct
                   GLPBal.proj = dbgl.proj
                   glPbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
        END.
        ASSIGN glPbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
              glPbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
    /* Department Balances */
        FIND FIRST glDbal WHERE GLDBal.dept = dbgl.dept AND glDbal.acct = dbgl.acct 
                           AND glDbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glDbal THEN DO:
            CREATE glDbal.
            ASSIGN glDbal.acct = dbgl.acct
                   glDbal.dept = dbgl.dept
                   glDbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
        END.
        ASSIGN glDbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
               glDbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
        DELETE dbgl.
   END.
   RETURN.
END.

/*PROCEDURE crd-update:
        wsVat = 0.
    FIND FIRST crdmf WHERE crdmf.acc = wsAcc NO-ERROR.
    ASSIGN wsVat = ROUND(((crdmf.vatAmt / crdmf.bal) * wsAmt),2) WHEN crdmf.vatAmt <> 0
           crdmf.bal = crdmf.bal - wsAmt
           crdmf.vatAmt = crdmf.vatAmt - wsVat.
    CREATE bfrCrdtmf. /* Update and create Creditor  transaction */
    ASSIGN bfrCrdtmf.period  = INT(st-per:SCREEN-VALUE IN FRAME frm-pay)
          bfrCrdtmf.amt     = dec(wsAmt:SCREEN-VALUE) * -1
          bfrCrdtmf.Acc     = wsAcc
          bfrCrdtmf.Descrip = wsNar:SCREEN-VALUE IN FRAME frm-pay
          bfrCrdtmf.uid     = varUser
          bfrCrdtmf.Ref     = wsRef:SCREEN-VALUE IN FRAME frm-pay
          bfrCrdtmf.TransID = wsTransId
          bfrCrdtmf.TrType  = 1
          bfrCrdtmf.trDate   = DATE(vDate:SCREEN-VALUE)
          bfrCrdtmf.ledger   = 0
          bfrCrdtmf.CreDate  = TODAY 
          bfrCrdtmf.OrdNo    = "0"
          bfrCrdtmf.Quantity = 1
          bfrCrdtmf.VAT      = wsVat * -1.
    IF wsVat <> 0 THEN
       DO  X = 1 TO 2:
           CREATE dbgl.
           ASSIGN dbgl.CREDATE = TODAY
                 dbgl.DESCRIP  = "Creditor Payment: Acc#" + string(crdmf.acc) 
                 dbgl.period   = INT(st-per:SCREEN-VALUE)
                 dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                 dbgl.TransID  = wsTransId
                 dbgl.trDATE   = DATE(vDate:SCREEN-VALUE)
                 dbgl.UID      = varUser
                 dbgl.SOURCE   = "CB".
           IF X = 1 THEN
               ASSIGN dbgl.AMT = dbgl.AMT + wsVat * -1 
                      dbgl.acct     = DEC("100" + STRING(simctr.vat[3])).
           ELSE IF X = 2 THEN
               ASSIGN dbgl.AMT = dbgl.AMT + wsVat 
                      dbgl.acct     = DEC("100" + STRING(simctr.vat[4])).
       END.
END.*/
