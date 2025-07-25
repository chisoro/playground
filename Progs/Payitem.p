session:DATA-ENTRY-RETURN = TRUE.
/* Program.................payitem.p
   Notes:...... Payroll item codes capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Pay item Code already exist"
&SCOPED-DEFINE wsTitle           "Pay item File Maintenance"
&SCOPED-DEFINE tmptable             payitem
&SCOPED-DEFINE skey                   payitem.Itemcode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.cat~
                              bfr{&tmptable}.Type~
                              bfr{&tmptable}.Suffix~
                              bfr{&tmptable}.StdAmt~
                              bfr{&tmptable}.Regular~
                              bfr{&tmptable}.Tax~
                              bfr{&tmptable}.LVar~
                              bfr{&tmptable}.ULAmt~
                              bfr{&tmptable}.LLAmt~
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Itemcode ~
                                        COLUMN-LABEL 'Item ':C ~
                              bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' Description ' WIDTH 40~
                              bfr{&tmptable}.Suffix ~
                                            COLUMN-LABEL 'GL Suffix ':C ~
                              wsDes  ~
                                            COLUMN-LABEL 'Type ':C ~
                              wsDes1  ~
                                            COLUMN-LABEL 'Tax Type ':C
                              

DEF VAR wsfrm AS CHAR.
DEF VAR wsDesc LIKE PayItem.Descrip.
DEF VAR wsdes AS CHAR FORM "x(15)".
DEF VAR wsdes1 AS CHAR FORM "x(15)".
DEF BUTTON btnLink LABEL "LINKED".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.
{varlib.i}


define frame frm-input
    {&skey}                 COLON 20 LABEL "Item" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 20 LABEL "Description"FORM "x(40)" SKIP(0.5)
    bfr{&tmptable}.cat      COLON 20 LABEL "Item Category"
                VIEW-AS COMBO-BOX SIZE 15 BY 2 
                LIST-ITEM-PAIRS "N/A",0, "INCOME",1,"DEDUCTION",2,"ACRUAL",3,"Benefit",4 AUTO-RETURN SKIP(0.5)
     bfr{&tmptable}.Type    COLON 20 LABEL "Item Type" AUTO-RETURN 
     bfr{&tmptable}.Tax     COLON 50 LABEL "Tax" AUTO-RETURN SKIP(0.5)
     bfr{&tmptable}.LVar    COLON 20 LABEL "Formula" FORM "x(40)" SKIP(0.5)
     btnLink COLON 10 NO-TAB-STOP 
     bfr{&tmptable}.Linked NO-LABEL
     Payitem.Descrip NO-LABEL VIEW-AS TEXT  SKIP(0.5)
     bfr{&tmptable}.StdAmt  COLON 20 LABEL "Amount" SKIP(0.5)
     bfr{&tmptable}.LLAmt  COLON 20 LABEL "Lower Limit"
     bfr{&tmptable}.ULAmt            LABEL "Upper Limit" FORM ">>>,>>>,>>>9.99-" SKIP(0.5)
     bfr{&tmptable}.Regular COLON 20 LABEL "Regular Item/Not?" AUTO-RETURN SKIP(0.5)
     btnLedger COLON 8 bfr{&tmptable}.Suffix  NO-LABEL glmf.DESCRIPTION NO-LABEL VIEW-AS TEXT 
    SKIP(1.5)
    btn-ok colon 5
    btn-close colon 50 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL ITEM CODE MAINTENANCE".

DEF    QUERY qryLink FOR Payitem SCROLLING.
DEF BROWSE brwLink QUERY qryLink
    DISPLAY Payitem.Itemcode Payitem.Descrip COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frmLink 
    brwLink AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Item Selection".

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-Ok colon 5
    btn-Close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "LEDGER SELECTION".

    
/* ***** Triggers for the main frame **** */

ON CHOOSE OF btnLink IN FRAME frm-input
DO:
  VIEW FRAME frmLink.
  OPEN QUERY qryLink FOR EACH Payitem 
      WHERE PayItem.Itemcode < INT({&skey}:SCREEN-VALUE IN FRAME frm-input) NO-LOCK.
  ENABLE ALL WITH FRAME frmLink.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frmLink 
          OR close of THIS-PROCEDURE IN FRAME frmLink
          OR CHOOSE OF btn-ok IN FRAME frmLink 
          OR 'enter':u OF brwLink
          OR 'mouse-select-dblclick' OF brwLink.
  CLOSE QUERY qryLink.
  HIDE FRAME frmLink.
  IF bfr{&tmptable}.Link:SCREEN-VALUE = "" THEN
       RETURN NO-APPLY.
   ELSE 
  APPLY 'tab' TO bfr{&tmptable}.Link.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frmLink 
    OR 'enter':u OF brwLink
    OR 'mouse-select-dblclick' OF brwLink
DO: 
   GET CURRENT qryLink EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY PayItem.Itemcode @ bfr{&tmptable}.Link PayItem.DESCRIP  WITH FRAME frm-input.
   
END.

ON VALUE-CHANGED OF bfr{&tmptable}.TYPE
DO:
    IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 2 OR INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 3 THEN
        ENABLE btnLink bfr{&tmptable}.Link WITH FRAME frm-input.
    ELSE DO:
       /* bfr{&tmptable}.Link:SCREEN-VALUE = "0". */
        DISABLE btnLink bfr{&tmptable}.Link WITH FRAME frm-input.
    END.
    APPLY 'tab' TO bfr{&tmptable}.TYPE.
END.

ON  'enter':u OF bfr{&tmptable}.cat
DO:
    APPLY 'tab' TO bfr{&tmptable}.cat.
END.
  
/* ***** Triggers for the main frame **** */
ON CHOOSE OF btn-Add IN FRAME frm-Main DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw-{&tmptable}
        OR 'mouse-select-dblclick' OF brw-{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        CLEAR FRAME frm-input ALL.
        run proc-edit.
        RETURN.
END.

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
    END.
END.

ON CHOOSE OF btnLedger IN FRAME frm-Input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-Ok IN FRAME frm-pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO bfr{&tmptable}.Suffix.
  RETURN. 
END. 

ON CHOOSE OF btn-Ok IN FRAME frm-pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  bfr{&tmptable}.Suffix glmf.DESCRIPTION WITH FRAME frm-Input.
   APPLY 'tab' TO  bfr{&tmptable}.Suffix IN FRAME frm-Input.
END.

ON 'tab' of  bfr{&tmptable}.Suffix IN FRAME frm-Input
    OR 'enter' OF  bfr{&tmptable}.Suffix IN FRAME frm-Input
DO:
    IF  DEC( bfr{&tmptable}.Suffix:SCREEN-VALUE) <> 0 THEN DO:
        FIND FIRST glmf WHERE glmf.Acct = DEC(bfr{&tmptable}.Suffix:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  bfr{&tmptable}.Suffix glmf.DESCRIPTION WITH FRAME frm-Input.
            ASSIGN bfr{&tmptable}.Suffix = DEC(bfr{&tmptable}.Suffix:SCREEN-VALUE).
        END.
    END.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST PayItem WHERE Payitem.Itemcode  = wsid NO-ERROR.
        IF AVAILABLE PayItem THEN DO:
           MESSAGE  "Pay Item already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.Itemcode = wsid
                   bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.cat     = INT(bfr{&tmptable}.cat:SCREEN-VALUE)
                   bfr{&tmptable}.TYPE    = INT(bfr{&tmptable}.Type:SCREEN-VALUE)
                   bfr{&tmptable}.LVar    = bfr{&tmptable}.LVar:SCREEN-VALUE
                   bfr{&tmptable}.Linked  = INT(bfr{&tmptable}.Linked:SCREEN-VALUE)
                   bfr{&tmptable}.Regular = LOGICAL(bfr{&tmptable}.Regular:SCREEN-VALUE)
                   bfr{&tmptable}.Suffix  = INT(bfr{&tmptable}.Suffix:SCREEN-VALUE)
                   bfr{&tmptable}.Tax     = INT(bfr{&tmptable}.Tax:SCREEN-VALUE)
                   bfr{&tmptable}.StdAmt  = DEC(bfr{&tmptable}.StdAmt:SCREEN-VALUE)
                   bfr{&tmptable}.ULAmt   = DEC(bfr{&tmptable}.ULAmt:SCREEN-VALUE)
                   bfr{&tmptable}.LLAmt   = DEC(bfr{&tmptable}.LLAmt:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.cat     = INT(bfr{&tmptable}.cat:SCREEN-VALUE)
                bfr{&tmptable}.TYPE    = INT(bfr{&tmptable}.Type:SCREEN-VALUE)
                bfr{&tmptable}.LVar    = bfr{&tmptable}.LVar:SCREEN-VALUE
                bfr{&tmptable}.Linked  = INT(bfr{&tmptable}.Linked:SCREEN-VALUE)
                bfr{&tmptable}.Regular = LOGICAL(bfr{&tmptable}.Regular:SCREEN-VALUE)
                bfr{&tmptable}.Suffix  = INT(bfr{&tmptable}.Suffix:SCREEN-VALUE)
                bfr{&tmptable}.Tax     = INT(bfr{&tmptable}.Tax:SCREEN-VALUE)
                bfr{&tmptable}.StdAmt  = DEC(bfr{&tmptable}.StdAmt:SCREEN-VALUE)
                bfr{&tmptable}.ULAmt   = DEC(bfr{&tmptable}.ULAmt:SCREEN-VALUE)
                bfr{&tmptable}.LLAmt   = DEC(bfr{&tmptable}.LLAmt:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Itemcode.
    FIND FIRST paymtf WHERE Paymtf.Itemcode  = bfr{&tmptable}.Itemcode NO-ERROR.
    IF AVAILABLE paymtf THEN DO:
        MESSAGE "Pay item has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE paymtf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON row-display OF brw-{&tmptable} DO:
    ASSIGN wsDes = "AMOUNT" WHEN bfr{&tmptable}.TYPE = 1
           wsDes = "PERCENTAGE" WHEN bfr{&tmptable}.TYPE = 2
           wsDes = "HOURS" WHEN bfr{&tmptable}.TYPE = 3
           wsDes = "FORMULA" WHEN bfr{&tmptable}.TYPE = 4
           wsdes1 = "N/A"    WHEN  bfr{&tmptable}.tax = 0
           wsdes1 = "TAXABLE"    WHEN  bfr{&tmptable}.tax = 1
           wsdes1 = "ALLOWABLE"    WHEN  bfr{&tmptable}.tax = 2
           wsdes1 = "CREDIT"    WHEN  bfr{&tmptable}.tax = 3.
END.

ON 'start-search':u OF BROWSE brw-{&tmptable}
  OR 'start-search':u OF BROWSE brw-Ledger
    run SEARCH.ip.

/********** MAIN LOGIC **********/
browse brw-Ledger:allow-column-searching = true.
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-Main.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT btnLink WITH FRAME frm-input.
   DISABLE btnLink bfr{&tmptable}.link WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    CLEAR FRAME frm-input ALL.
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.Itemcode).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Itemcode  = wsid EXCLUSIVE-LOCK NO-ERROR.
    IF bfr{&tmptable}.Linked <> 0 THEN DO:
        FIND FIRST PayItem WHERE Payitem.ItemCode = bfr{&tmptable}.Linked NO-LOCK NO-ERROR.
        ASSIGN wsDesc = Payitem.Descrip.
    END.
    ELSE wsDesc = "".
    DISPLAY wsid @ {&skey} {&updFields}  bfr{&tmptable}.Linked wsDesc @ Payitem.Descrip  WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close btnLink btnLedger WITH FRAME frm-input.
    IF  bfr{&tmptable}.TYPE <> 1 THEN
        ENABLE bfr{&tmptable}.LINKED WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

procedure Search.ip.
   IF FRAME-NAME = "frm-Main" THEN
       hCol = browse brw-{&tmptable}:current-column.
   ELSE IF FRAME-NAME = "frm-Pick" THEN
       hCol = browse brw-ledger:current-column.
       assign  frame Search-opt:title = hCol:label + " Column"
               wsfrm = FRAME-NAME.
       IF trim(hCol:label)<> "Description" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
    IF wsfrm = "frm-Main" THEN
    case trim(hCol:label):
      when "Description" then
         do:
          OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.Descrip.
            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END. /*end CASE */
    IF wsfrm = "frm-Pick" THEN DO:
    case trim(hCol:label):
      when "Description" then
         do:
          OPEN query qry-ledger FOR EACH glmf no-lock 
                            where glmf.DESCRIPTION >= wsSearch:SCREEN-VALUE
                               BY glmf.DESCRIPTION.
            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END. /*end CASE */
    END.
    RETURN.
END.
