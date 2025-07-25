/* Program.................hse02.p
   Notes:...... Land sale Master file maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
/*DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO. */

DEF SHARED VAR varUser    AS CHAR FORM "xxx" .
DEF VAR wsid LIKE hsecmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR wsSearch   LIKE hsecmf.NAME.
DEF VAR hCol      AS WIDGET-HANDLE.
DEF VAR wsSource  AS CHAR INITIAL "HS".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsdate    AS DATE.
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR st-per     AS INT FORM "999999".
DEF VAR wsAmt     LIKE hsecmf.bal.
DEF VAR wsVat     LIKE hsecmf.bal.
DEF VAR wsTotal   LIKE wsAmt.
DEF VAR wsTVat    LIKE wsAmt.
DEF VAR wsDr      LIKE wsAmt.
DEF VAR wsCr      LIKE wsAmt.
DEF VAR wsTDr     LIKE wsAmt.
DEF VAR wsTCr     LIKE wsAmt.
DEF VAR wsP       LIKE wsAmt.
DEF VAR wsn       AS INT.
DEF VAR wsr       AS   DEC FORM "zzz9.999999999-".
DEF VAR wsInst    LIKE wsAmt.
DEF VAR wsRate    AS DEC EXTENT 2.
DEF VAR wsdes     AS CHAR FORM "x(12)".
DEF  VAR wsLedger LIKE hsesch.CLedger.
DEF VAR X AS INT.


DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".
DEF BUTTON btn-Ward   LABEL "Ward".
DEF BUTTON btn-Sub    LABEL "Suburb".
DEF BUTTON btn-acc    LABEL "Account".
DEF BUTTON btn-Scheme LABEL "Scheme".

DEFINE QUERY qry-hsecmf FOR hsecmf scrolling.
DEF BROWSE brw-hsecmf QUERY qry-hsecmf
    DISPLAY hsecmf.dbacc  hsecmf.NAME hsecmf.Regid LABEL "ID/Co. Registration"
    hsecmf.StandNo hsecmf.scheme hsecmf.Suburb hsecmf.PPrice hsecmf.bal hsecmf.AmtDue[1] WIDTH 15
    WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-PickWard FOR dbwrd scrolling.
DEF BROWSE brw-PickWard QUERY qry-PickWard
    DISPLAY  dbwrd.Ward dbwrd.Descrip
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-PickSuburb FOR dbsmf scrolling.
DEF BROWSE brw-PickSuburb QUERY qry-PickSuburb
    DISPLAY  dbsmf.suburb dbsmf.Descrip
    WITH 8 DOWN SEPARATORS.

DEF    QUERY qry-pickScheme FOR hsesch SCROLLING.
DEF BROWSE brw-pickScheme QUERY qry-pickScheme
        DISPLAY hsesch.scheme hsesch.Descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickScheme 
        brw-pickScheme AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Scheme Selection".

define RECTANGLE rec-1
     edge-pixels 2 graphic-edge  no-fill
     size 135 by 2.3.

define RECTANGLE rec-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 135 by 18.5.

define RECTANGLE rec-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 99 by 19.5.
define RECTANGLE rec-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 75 by 19.5.

define RECTANGLE rec-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define RECTANGLE rec-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 174 by 2.3.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE JOURNAL CAPTURE AND UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.


define frame frm-input
    SKIP (0.5)
     "------------------ACCOUNT DETAILS ------------------" COLON 10 SKIP
    hsecmf.dbacc        colon 20 LABEL "Account" 
    hsecmf.Ref          COLON 77.5  LABEL "Reference" SKIP(.2)
    hsecmf.NAME         COLON 20 LABEL "Account Name"
    hsecmf.Sortname     COLON 77.5 LABEL "Sort Name"SKIP(.2)
    hsecmf.RegID        COLON 20 LABEL "ID/Co. Reg no"
    hsecmf.RegID1       COLON 40 NO-LABEL
    hsecmf.VatNo        COLON 77.5 LABEL "VAT Registration" SKIP(.2)
    hsecmf.Add1         colon 20 label "Address"
    hsecmf.Add2         colon 20 NO-LABEL
    hsecmf.Add3         colon 20 NO-LABEL SKIP(.2)
    hsecmf.cell[1]      COLON 20    LABEL "Cell Number" SPACE(2)  hsecmf.cell[2] NO-LABEL SKIP(.2)
    hsecmf.emailAdd     COLON 20    LABEL "Email Address" SKIP(1)

    "-------------PROPERTY DETAILS ---------------" COLON 20 SKIP(0.5)
    btn-Scheme          COLON 10 hsecmf.Scheme NO-LABEL hsesch.Descrip NO-LABEL VIEW-AS TEXT SKIP(.2)
    hsecmf.standNo      COLON 20 LABEL "Stand Number" 
    hsecmf.DeedNo       COLON 77.5    LABEL "Deed Number" SKIP(.2)
    hsecmf.SIZE         COLON 20 LABEL "Stand Size"
    hsecmf.sitevalue     LABEL "    Value Site" 
    hsecmf.bldvalue      LABEL "Building" SKIP(.2)
    btn-Sub            COLON 11 NO-TAB-STOP 
    hsecmf.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)
    btn-Ward           COLON 12.5 NO-TAB-STOP 
    hsecmf.Ward         NO-LABEL 
     dbwrd.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)

    "-------------   PURCHASE DETAILS   ---------------" AT ROW 1.5 COL 110 SKIP(0.5)
    hsesch.txtCur COLON 120 LABEL "Currency"  FORM "!!!!!!!!" BGCOLOR 12 VIEW-AS TEXT
    Hsecmf.PRate            LABEL "Rate" VIEW-AS TEXT  SKIP(0.5)
    Hsecmf.PDate  COLON 120 LABEL "Purchase Date" SKIP(.2)
    Hsecmf.PPrice COLON 120 LABEL "Purchase Price" SKIP(.2)
    Hsecmf.Vat    COLON 120 LABEL "VAT %"  skip(.2)
    Hsecmf.DAmt   COLON 120 LABEL "Required Deposit" skip(.2)
    Hsecmf.Period COLON 120 LABEL "Period  "
    wsdes                   NO-LABEL VIEW-AS TEXT
    hsesch.IntRate          LABEL "Interest Rate" VIEW-AS TEXT skip(.2)
    Hsecmf.Bal    COLON 120 LABEL "Capital Balance"  VIEW-AS TEXT SKIP(.2)
    Hsecmf.InsAmt COLON 120 LABEL "Instalment" VIEW-AS TEXT SKIP(.5)
    Hsecmf.AmtDue[1] COLON 130  LABEL "Trading Amount Due" VIEW-AS TEXT hsesch.txtCur NO-LABEL VIEW-AS TEXT SKIP(.2)
    Hsecmf.AmtDue[2] COLON 130  LABEL "Accounting  Amount Due" VIEW-AS TEXT  FGCOLOR 12 SKIP(1)
    Hsecmf.LTDate  COLON 130 LABEL "Last Transaction Date" VIEW-AS TEXT SKIP(.2)
    Hsecmf.LTAmt   COLON 130 LABEL "Last Transaction Amount" VIEW-AS TEXT SKIP(.2)
    Hsecmf.LTRate  COLON 130 LABEL "Last Transaction Rate" VIEW-AS TEXT SKIP(.5)

    "-------------   ACCOUNTING DATES   ---------------" COLON 110
    st-per            COLON 130  LABEL "Accounting Period" SKIP(.2)
    wsDate           COLON 130  LABEL "Transaction Date" SKIP(0.5)
    hsecmf.comnt   AT ROW 21 COL 5  LABEL "COMMENTS" VIEW-AS EDITOR SIZE 132 BY 2 
    btn-ok  AT ROW 23.6  COL 30
    btn-close AT ROW 23.6  COL 120
    rec-3 AT ROW 1.4 COL 3
    rec-4 AT ROW 1.4 COL 102
    rec-6 AT ROW 23  COL 3
    with view-as dialog-box keep-tab-order no-validate
      BGCOLOR 3  side-labels no-underline three-d SCROLLABLE SIZE 180 BY 26
     TITLE "LAND SALES ACCOUNT MAINTENANCE".
    
DEF FRAME frm-hsecmf
    brw-hsecmf AT ROW 2 COL 6
    btn-add AT ROW 20.7 COL 20
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rec-2 AT ROW 1.4 COL 3
    rec-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
     NO-LABEL no-underline three-d SCROLLABLE SIZE 140 BY 24 CENTERED
    TITLE "LAND SALES ACCOUNT MAINTENANCE".

DEF    QUERY qry-Ward FOR dbwrd SCROLLING.
DEF BROWSE brw-Ward QUERY qry-Ward
    DISPLAY dbwrd.Ward dbwrd.Descrip WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-Ward 
    brw-Ward AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ward Selection".

DEF    QUERY qry-Sub FOR dbsmf SCROLLING.
DEF BROWSE brw-Sub QUERY qry-Sub
    DISPLAY dbsmf.suburb dbsmf.Descrip WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-Sub 
    brw-Sub AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Suburb Selection".

ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO hsecmf.dbacc IN FRAME frm-input.
    RETURN NO-APPLY.
END.

ON CHOOSE OF btn-Scheme IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickScheme.
  OPEN QUERY qry-pickScheme FOR EACH hsesch NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickScheme.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickScheme 
          OR close of THIS-PROCEDURE IN FRAME frm-pickScheme 
          OR CHOOSE OF btn-ok IN FRAME frm-pickScheme 
          OR 'enter':u OF brw-pickScheme
          OR 'mouse-select-dblclick' OF brw-pickScheme.
  CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO  hsecmf.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickScheme 
    OR 'enter':u OF brw-PickScheme
    OR 'mouse-select-dblclick' OF brw-PickScheme
DO: 
   GET CURRENT qry-PickScheme NO-LOCK NO-WAIT.
   DISPLAY hsesch.Scheme @ hsecmf.Scheme WITH FRAME frm-input.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO hsecmf.Scheme.
  RETURN. 
END.

ON  'enter':u OF hsecmf.Scheme IN FRAME  frm-Input
     OR 'tab':u OF hsecmf.Scheme IN FRAME  frm-Input
DO:
   FIND FIRST hsesch WHERE hsesch.Scheme = INT(hsecmf.Scheme:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsesch THEN DO:
      MESSAGE "Project does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE  DO:
       FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur NO-LOCK NO-ERROR.
       wsRate[1] = tblForex.DecRate.
       IF hsesch.Freq = "M"  THEN
          wsdes = "Monthly".
       IF hsesch.Freq = "Q"  THEN
          wsdes = "Quarterly".
       IF hsesch.Freq = "H"  THEN
          wsdes = "Half-Yearly".
       IF hsesch.Freq = "Y"  THEN
          wsdes = "Yearly".
       DISPLAY hsesch.Scheme @ hsecmf.Scheme hsesch.Descrip hsesch.txtcur tblForex.DecRate @ hsecmf.PRate 
               wsdes hsesch.IntRate WITH FRAME frm-input.
   END.   
   RETURN.    
END.

ON 'enter':U OF hsecmf.StandNo  
    OR 'tab':U OF hsecmf.StandNo 
    OR 'leave':U OF hsecmf.StandNo
DO:
     FIND FIRST hsecmf WHERE hsecmf.Scheme = INT(hsecmf.Scheme:SCREEN-VALUE)
         AND hsecmf.StandNo = hsecmf.StandNo:SCREEN-VALUE
         AND hsecmf.Accstat   = 0 NO-LOCK  NO-ERROR. 
       IF AVAILABLE hsecmf THEN
       DO:
          MESSAGE "Stand Number " hsecmf.StandNo:SCREEN-VALUE " OF Project  "
               hsesch.Descrip " is already allocated"
              VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          APPLY "entry" TO SELF IN FRAME  frm-Input.
          RETURN NO-APPLY.
       END.
    RETURN.
END.

ON 'enter':U OF hsecmf.PDate 
DO:
    FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND DATE(hsecmf.PDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
        wsRate[1] = tblForex.decRate.
    END.    
    ELSE IF NOT AVAILABLE tblForex THEN DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= DATE(hsecmf.PDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
             wsRate[1] = tblForexH.decRate.
    END.
    hsecmf.PRate:SCREEN-VALUE = STRING(wsRate[1]).
    wsDate = DATE(hsecmf.PDate:SCREEN-VALUE).
    st-per = DEC(STRING(YEAR(wsDate)) + STRING(MONTH(wsDate),"99")).
    DISPLAY st-per wsDate WITH FRAME frm-Input.
    RETURN.
END.

ON 'enter':U OF st-per
    OR 'tab':U OF st-per
DO:
   IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
    RETURN.
END.

ON 'TAB':U OF wsDate IN FRAME frm-Input
   OR 'enter':U OF wsDate IN FRAME frm-Input
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:  
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Update"  THEN DO:
        /*DO TRANSACTION: */
           /* FIND CURRENT hsecmf EXCLUSIVE-LOCK. */
            IF CURRENT-CHANGED hsecmf THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
              FIND FIRST hsecmf WHERE hsecmf.dbacc =  DEC(hsecmf.dbacc:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN  hsecmf.NAME      = hsecmf.NAME:SCREEN-VALUE
                    hsecmf.scheme    = INT(hsecmf.scheme:SCREEN-VALUE)
                    hsecmf.Sortname  = hsecmf.Sortname:SCREEN-VALUE
                    hsecmf.RegId     = hsecmf.RegId:SCREEN-VALUE
                    hsecmf.RegId1     = hsecmf.RegId1:SCREEN-VALUE
                    hsecmf.VatNo     = DEC(hsecmf.VatNo:SCREEN-VALUE)
                    hsecmf.StandNo   = hsecmf.StandNo:SCREEN-VALUE
                    hsecmf.Add1      = hsecmf.Add1:SCREEN-VALUE
                    hsecmf.Add2      = hsecmf.Add2:SCREEN-VALUE
                    hsecmf.Add3      = hsecmf.Add3:SCREEN-VALUE
                    hsecmf.suburb    = int(hsecmf.suburb:SCREEN-VALUE)
                    hsecmf.ward      = int(hsecmf.ward:SCREEN-VALUE)
                    hsecmf.SIZE      = DEC(hsecmf.SIZE:SCREEN-VALUE)
                    hsecmf.SiteValue = DEC(hsecmf.SiteValue:SCREEN-VALUE)
                    hsecmf.BldValue  = DEC(hsecmf.BldValue:SCREEN-VALUE)
                    hsecmf.DeedNo    = hsecmf.DeedNo:SCREEN-VALUE
                    hsecmf.emailAdd  = hsecmf.emailAdd:SCREEN-VALUE
                    hsecmf.cell[1]      = DEC(hsecmf.cell[1]:SCREEN-VALUE)
                    hsecmf.cell[2]      = DEC(hsecmf.cell[2]:SCREEN-VALUE)
                    hsecmf.comnt      = hsecmf.comnt:SCREEN-VALUE
                    hsecmf.credate   = TODAY.
               ASSIGN hsecmf.InsAmt hsecmf.Period hsecmf.Ref.
            END.
            FIND CURRENT hsecmf NO-LOCK.                    
       /* END. *//* Do Transaction */
        HIDE FRAME frm-input.
        APPLY 'close' TO THIS-PROCEDURE.
        brw-hsecmf:REFRESH() IN FRAME frm-hsecmf.
        RETURN.
    END.
    ELSE DO:
        IF hsecmf.dbacc:SCREEN-VALUE = "" THEN DO:
            MESSAGE "BLANK not a valid Account...." VIEW-AS ALERT-BOX.
            APPLY 'entry' TO hsecmf.dbacc IN FRAME frm-input.
            RETURN NO-APPLY.
        END.
        ELSE IF  DEC(hsecmf.dbacc:SCREEN-VALUE) <> 0 THEN DO:
            FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(hsecmf.dbacc:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE hsecmf THEN DO:
                MESSAGE "Account alread exist.... no record saved" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                CREATE hsecmf.
                 ASSIGN hsecmf.dbacc     = DEC(hsecmf.dbacc:SCREEN-VALUE)
                        hsecmf.scheme    = INT(hsecmf.scheme:SCREEN-VALUE)
                        hsecmf.NAME      = hsecmf.NAME:SCREEN-VALUE
                        hsecmf.Sortname  = hsecmf.Sortname:SCREEN-VALUE
                        hsecmf.RegId     = hsecmf.RegId:SCREEN-VALUE
                        hsecmf.RegId1     = hsecmf.RegId1:SCREEN-VALUE
                        hsecmf.VatNo     = int(hsecmf.VatNo:SCREEN-VALUE)
                        hsecmf.StandNo   = hsecmf.StandNo:SCREEN-VALUE
                        hsecmf.Add1      = hsecmf.Add1:SCREEN-VALUE
                        hsecmf.Add2      = hsecmf.Add2:SCREEN-VALUE
                        hsecmf.Add3      = hsecmf.Add3:SCREEN-VALUE
                        hsecmf.suburb    = INT(hsecmf.suburb:SCREEN-VALUE)
                        hsecmf.ward      = INT(hsecmf.ward:SCREEN-VALUE)
                        hsecmf.SIZE      = DEC(hsecmf.SIZE:SCREEN-VALUE)
                        hsecmf.SiteValue = DEC(hsecmf.SiteValue:SCREEN-VALUE)
                        hsecmf.BldValue  = DEC(hsecmf.BldValue:SCREEN-VALUE)
                        hsecmf.DeedNo    = hsecmf.DeedNo:SCREEN-VALUE
                        hsecmf.emailAdd  = hsecmf.emailAdd:SCREEN-VALUE
                        hsecmf.cell[1]      = DEC(hsecmf.cell[1]:SCREEN-VALUE)
                        hsecmf.cell[2]      = DEC(hsecmf.cell[2]:SCREEN-VALUE)
                        hsecmf.comnt      = hsecmf.comnt:SCREEN-VALUE
                        hsecmf.credate   = TODAY
                        hsecmf.AccStat   = 0
                        hsecmf.LTDate    = wsDate
                        hsecmf.scheme    = INT(hsecmf.scheme:SCREEN-VALUE).
                           
                 ASSIGN  hsecmf.AmtDue[1] hsecmf.AmtDue[2] hsecmf.Bal hsecmf.InsAmt
                         hsecmf.LTAmt hsecmf.PPrice   hsecmf.Ref hsecmf.PRate 
                         hsecmf.LTRate  hsecmf.Period hsecmf.PDate Hsecmf.DAmt Hsecmf.Vat .
                 ASSIGN hsecmf.LBal    = (hsecmf.bal * hsecmf.PRate)
                        Hsecmf.LAmtDue = hsecmf.AmtDue[2].
                 IF DEC(hsecmf.DAmt:SCREEN-VALUE) > 0 THEN
                     RUN CreTrans.ip.
            END.
            RELEASE hsecmf.
           CLEAR FRAME frm-input ALL.
           VIEW FRAME frm-input.
           APPLY 'entry' TO hsecmf.dbacc.
        END. /* eof if dbacc <> 0 */
     END. /* eof ELSE */
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
    RETURN.
END.

ON LEAVE OF hsecmf.dbacc IN FRAME frm-input
DO:
   IF  DEC(hsecmf.dbacc:SCREEN-VALUE) <> 0 THEN DO:
       FIND FIRST hsecmf WHERE hsecmf.dbacc = DEC(hsecmf.dbacc:SCREEN-VALUE) NO-ERROR.
       IF AVAILABLE hsecmf AND btn-ok:LABEL = "SAVE" THEN DO:
          MESSAGE "Record already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
   END.
   RETURN.    
END.

/*...... Triggers for button on-form buttons ....*/

/*------------------ Ward Button -------------*/
ON CHOOSE OF btn-Ward IN FRAME frm-input
DO:
  VIEW FRAME frm-Ward.
  OPEN QUERY qry-Ward FOR EACH dbwrd NO-LOCK.
  ENABLE ALL EXCEPT WITH FRAME frm-Ward.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Ward 
          OR close of THIS-PROCEDURE IN FRAME frm-Ward
          OR CHOOSE OF btn-ok IN FRAME frm-Ward 
          OR 'enter':u OF brw-Ward
          OR 'mouse-select-dblclick' OF brw-Ward.
  CLOSE QUERY qry-Ward.
  HIDE FRAME frm-Ward.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO hsecmf.ward.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ward
    OR 'enter':u OF brw-Ward
    OR 'mouse-select-dblclick' OF brw-Ward
DO: 
   GET CURRENT qry-Ward EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbwrd.Descrip dbwrd.Ward @ hsecmf.ward  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF hsecmf.ward IN FRAME frm-input
    OR 'tab':U OF hsecmf.ward IN FRAME frm-input
DO:
    FIND FIRST dbwrd WHERE dbwrd.Ward = INT(hsecmf.ward:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbwrd THEN
        DISPLAY dbwrd.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Ward...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

/*------------------ Suburb Button -------------*/
ON CHOOSE OF btn-Sub IN FRAME frm-input
DO:
  VIEW FRAME frm-Sub.
  OPEN QUERY qry-Sub FOR EACH dbsmf NO-LOCK.
  ENABLE ALL EXCEPT WITH FRAME frm-Sub.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Sub 
          OR close of THIS-PROCEDURE IN FRAME frm-Sub
          OR CHOOSE OF btn-ok IN FRAME frm-Sub 
          OR 'enter':u OF brw-Sub
          OR 'mouse-select-dblclick' OF brw-Sub.
  CLOSE QUERY qry-Sub.
  HIDE FRAME frm-Sub.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO hsecmf.suburb.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Sub
    OR 'enter':u OF brw-Sub
    OR 'mouse-select-dblclick' OF brw-Sub
DO: 
   GET CURRENT qry-Sub EXCLUSIVE-LOCK NO-WAIT.
        DISPLAY dbsmf.Descrip dbsmf.suburb @ hsecmf.suburb  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF hsecmf.suburb IN FRAME frm-input
    OR 'tab':U OF hsecmf.suburb IN FRAME frm-input
DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = INT(hsecmf.suburb:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbsmf THEN
        DISPLAY dbsmf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Suburb...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF hsecmf.DAmt IN FRAME frm-Input 
DO:
    IF  DEC(hsecmf.DAmt:SCREEN-VALUE) > DEC(hsecmf.PPrice:SCREEN-VALUE) THEN DO:
        MESSAGE "Deposit may not be Greater than the Price......." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN  hsecmf.Bal:SCREEN-VALUE = STRING(DEC(hsecmf.PPrice:SCREEN-VALUE) - DEC(hsecmf.DAmt:SCREEN-VALUE)).
       IF  DEC(hsecmf.DAmt:SCREEN-VALUE) <> 0.00 THEN DO:
           ASSIGN hsecmf.AmtDue[1]:SCREEN-VALUE =  STRING(DEC(hsecmf.DAmt:SCREEN-VALUE))
                  hsecmf.AmtDue[2]:SCREEN-VALUE = STRING(ROUND(DEC(hsecmf.AmtDue[1]:SCREEN-VALUE) * DEC(hsecmf.PRate:SCREEN-VALUE),2))
                  Hsecmf.LTDate:SCREEN-VALUE    = STRING(TODAY)
                  Hsecmf.LTRate:SCREEN-VALUE    = hsecmf.PRate:SCREEN-VALUE
                  Hsecmf.LTAmt:SCREEN-VALUE     = hsecmf.AmtDue[1]:SCREEN-VALUE.
                  
       END.
    END.   
    RETURN.
END.

ON 'enter':U OF hsecmf.period
DO:
    ASSIGN wsP = DEC(hsecmf.PPrice:SCREEN-VALUE) - DEC(hsecmf.DAmt:SCREEN-VALUE)
           wsn = DEC(hsecmf.period:SCREEN-VALUE).
    IF hsesch.IntRate <> 0 THEN
           ASSIGN wsr = (hsesch.IntRate / 1200)
                  wsInst = ROUND(wsP / ((EXP(( 1 + wsr),wsn) - 1) / (wsr * (EXP((1 + wsr),wsn)))),2).
    ELSE wsInst = ROUND((wsP / wsn),2).
           MESSAGE "Installment = " wsInst VIEW-AS ALERT-BOX.
    hsecmf.InsAmt:SCREEN-VALUE = STRING(wsInst).
    RETURN.
END.
/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-Add IN FRAME frm-hsecmf DO:
    btn-ok:LABEL IN  FRAME frm-input = "SAVE".
    CLEAR FRAME frm-input ALL.
    VIEW FRAME frm-input.
    st-per:SCREEN-VALUE = STRING(simctr.curper).
    ENABLE ALL  WITH FRAME frm-input.
    APPLY 'entry' TO hsecmf.dbacc.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
    OPEN QUERY qry-hsecmf FOR EACH hsecmf NO-LOCK.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-hsecmf 
    OR 'mouse-select-dblclick' OF brw-hsecmf 
DO: 
    btn-ok:LABEL IN FRAME frm-input = "UPDATE".
    GET CURRENT qry-hsecmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = hsecmf.dbacc.
    DISABLE hsecmf.dbacc WITH FRAME frm-input.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-hsecmf 
DO:
    GET CURRENT qry-hsecmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = hsecmf.dbacc.
    FIND FIRST hsehtf WHERE hsehtf.dbacc  = wsid NO-LOCK NO-ERROR.
    IF AVAILABLE hsehtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE hsehtf THEN DO:
       DELETE hsecmf.
       Method-Status = brw-hsecmf:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

 on 'start-search':u of browse brw-hsecmf
    run SEARCH.ip.

ON 'choose':U OF btnSearch IN FRAME search-opt 
    OR 'enter':U OF btnSearch IN FRAME search-opt
    OR 'tab':U OF btnSearch IN FRAME search-opt
DO:
    open query qry-hsecmf preselect 
                    each hsecmf no-lock 
                        where hsecmf.SortName >= wsSearch:SCREEN-VALUE
                           BY hsecmf.Sortname.
    /*RETURN.*/
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-hsecmf:allow-column-searching = true.
ASSIGN wsTime    = STRING(TIME,"HH:MM:SS").
       wsTransID = DEC (string(YEAR(TODAY),"9999")
                 + string(MONTH(TODAY),"99" )
                 + string(DAY(TODAY),"99")
                 + SUBSTR(STRING(wsTIME),1,2) 
                 + SUBSTR(STRING(wsTIME),4,2) 
                 + SUBSTR(STRING(wsTIME),7,2) ). 
OPEN QUERY qry-hsecmf FOR EACH hsecmf NO-LOCK.
ENABLE ALL EXCEPT btn-Edit WITH FRAME Frm-hsecmf.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-hsecmf.
IF CAN-FIND (FIRST dbgl WHERE dbgl.TransID = wsTransID AND dbgl.SOURCE = wsSource NO-LOCK) THEN
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Consol.ip"
                    &paged}
END. 
CLOSE QUERY qry-hsecmf.
HIDE FRAME frm-hsecmf.

/******* INTERNAL PROCEDURES *********/
PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO hsecmf.dbacc.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-hsecmf FOR EACH hsecmf NO-LOCK.
END PROCEDURE.

PROCEDURE proc-Edit:
   CLEAR FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
   FIND FIRST dbwrd WHERE dbwrd.ward   = hsecmf.ward NO-LOCK NO-ERROR.
   FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
   IF hsesch.Freq = "M"  THEN
      wsdes = "Monthly".
   IF hsesch.Freq = "Q"  THEN
      wsdes = "Quarterly".
   IF hsesch.Freq = "H"  THEN
      wsdes = "Half-Yearly".
   IF hsesch.Freq = "Y"  THEN
      wsdes = "Yearly".
   DISPLAY hsecmf EXCEPT Hsecmf.bfBal hsecmf.AccStat hsecmf.Credate hsecmf.Usr 
           Hsecmf.LAmtDue hsecmf.lbal WITH FRAME frm-input.
   DISPLAY hsesch.Descrip wsdes hsesch.txtCur hsesch.IntRate 
       dbwrd.Descrip dbsmf.Descrip WITH FRAME frm-input.  
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
END PROCEDURE.

PROCEDURE SEARCH.ip.
    hCol = BROWSE brw-hsecmf:CURRENT-COLUMN.
        VIEW FRAME Search-opt.
        ENABLE ALL WITH FRAME Search-opt.
        WAIT-FOR 'choose' OF btnSearch OR 'tab' of btnSearch OR 'enter' OF btnSearch 
            OR 'window-close' OF FRAME Search-opt OR CLOSE OF THIS-PROCEDURE OR
                'esc','f4' OF FRAME Search-opt.
        HIDE FRAME Search-opt.
        CASE trim(hCol:label):
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
END PROCEDURE.


PROCEDURE CreTrans.ip:
    /*Create Monthly Transaction  for deposit*/
    CREATE hsemtf.
    ASSIGN hsemtf.Accper  = st-per
           hsemtf.Amt     = hsecmf.DAmt /* hsecmf.PRate */
           hsemtf.CRate   = hsecmf.PRate
           hsemtf.Vat     = ROUND((hsecmf.DAmt * (hsecmf.Vat / (100  + hsecmf.Vat))) ,2)
           hsemtf.dbacc   = hsecmf.dbacc
           hsemtf.Scheme  = hsesch.scheme
           hsemtf.trDate  = wsDate
           hsemtf.ref     = hsecmf.ref
           hsemtf.descrip = "Land Sales Deposit".
     /*Create Historical Transaction for deposit*/
       FIND FIRST glmf WHERE glmf.acct = hsesch.CLedger NO-LOCK NO-ERROR.
       CREATE hsehtf.
       ASSIGN hsehtf.Accper  = st-per
              hsehtf.dbacc   = hsecmf.dbacc
              hsehtf.Amt     = hsecmf.DAmt
              hsehtf.CRate   = hsecmf.PRate
              hsehtf.tRate   = hsecmf.PRate
              hsehtf.txtCur  = hsesch.txtCur
              hsehtf.Vat     = ROUND((hsecmf.DAmt * (hsecmf.Vat / (100 + hsecmf.Vat))) ,2)
              hsehtf.Iledger = hsesch.Cledger
              hsehtf.proj    = glmf.Proj
              hsehtf.dept    = glmf.dept
              hsehtf.fund    = glmf.fund
              hsehtf.ref     = hsecmf.ref
              hsehtf.Scheme  = hsesch.scheme
              hsehtf.trDate  = wsDate
              hsehtf.PoDate  = TODAY
              hsehtf.prog    = "hse02.p"
              hsehtf.TransID = wsTransId  
              hsehtf.uid     = varUser
              hsehtf.descrip = "Land Sales Deposit".
    FIND FIRST hseblf WHERE hseblf.scheme = hsecmf.scheme AND hseblf.dbacc = hsecmf.dbacc NO-ERROR. /* Age analysis data */
    IF NOT AVAILABLE hseblf THEN DO:
        CREATE hseblf.
        ASSIGN hseblf.scheme = hsecmf.scheme
               hseblf.dbacc  = hsecmf.dbacc.
    END.
    hseblf.amt[1] = hseblf.amt[1] + hsecmf.DAmt.

    RUN glcons-ip.

END PROCEDURE.      

PROCEDURE Consol.ip:
     {glcon.i}
END.

PROCEDURE glcons-ip:
    /* Create GL Consolidation Transactions */ 
     wsVat  = ROUND((hsecmf.PPrice * hsecmf.PRate) * (hsecmf.Vat / 100) ,2).
    DO X = 1 TO 2: /*Capital Amount */     
         ASSIGN wsledger = hsesch.CLedger WHEN X = 1
                wsledger = hsesch.ILedger WHEN X = 2
                wsAmt  = hsecmf.PPrice * hsecmf.PRate        WHEN X = 1
                wsAmt  = ((hsecmf.PPrice * hsecmf.PRate) - wsVat) * -1 WHEN X = 2.
         FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
         FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = st-per
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
         IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = wsLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = st-per
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "hse01.p"
                      dbgl.descrip  = "Online Land Sales "
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
         END.
         dbgl.AMT = dbgl.AMT + wsAmt.
    END.
    IF wsVat <> 0 THEN DO: /* Credit Vat leg */
         FIND FIRST glmf WHERE glmf.acct = simctr.vat[2] NO-LOCK NO-ERROR.
         FIND FIRST dbgl WHERE dbgl.acct = simctr.vat[2] AND dbgl.period = st-per
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
         IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = simctr.vat[2]
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = st-per
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "hse01.p"
                      dbgl.descrip  = "Online Land Sales VAT"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
         END.
         dbgl.AMT = dbgl.AMT + wsVat * -1.
    END.
    DO X = 1 TO 2: /* Deposit Amount */       
         ASSIGN wsledger = hsesch.CtrLedger WHEN X = 1
                wsledger = hsesch.CLedger WHEN X = 2
                wsAmt  = hsecmf.DAmt * hsecmf.PRate        WHEN X = 1
                wsAmt  = (hsecmf.DAmt * hsecmf.PRate) * -1 WHEN X = 2.
         FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
         FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = st-per
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
         IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = wsLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = st-per
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "hse01.p"
                      dbgl.descrip  = "Online Land Sales Deposit"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
         END.
         dbgl.AMT = dbgl.AMT + wsAmt.
    END.
END PROCEDURE.
