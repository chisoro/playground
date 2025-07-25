session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varDescrip LIKE dbtmf.Descrip.
DEF VAR varFactor LIKE  dbtat.Units.

DEFINE QUERY qry-dbcmf FOR dbcmf scrolling.
DEF BROWSE brw-dbcmf QUERY qry-dbcmf
    DISPLAY dbcmf.dbacc  dbcmf.NAME dbcmf.Regid LABEL "ID/Co. Registration"
    dbcmf.StandNo dbcmf.Suburb dbcmf.POwner dbcmf.AccBal WIDTH 15
    WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-PickCons FOR dbctf scrolling.
DEF BROWSE brw-PickCons QUERY qry-PickCons
    DISPLAY dbctf.Descrip dbctf.cons 
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-PickWard FOR dbwrd scrolling.
DEF BROWSE brw-PickWard QUERY qry-PickWard
    DISPLAY  dbwrd.Ward dbwrd.Descrip
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-PickSuburb FOR dbsmf scrolling.
DEF BROWSE brw-PickSuburb QUERY qry-PickSuburb
    DISPLAY  dbsmf.suburb dbsmf.Descrip
    WITH 8 DOWN SEPARATORS.

DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".
DEF BUTTON btn-Cons   LABEL "Consumer Type".
DEF BUTTON btn-Ward   LABEL "Ward".
DEF BUTTON btn-Sub LABEL "Suburb".

define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 116 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 116 by 18.5.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 95 by 17.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 11.

define rectangle rect-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define rectangle rect-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 155 by 2.3.

define frame frm-input
    SKIP (1)
     "------------------ACCOUNT DETAILS ------------------" COLON 5 SKIP
    dbcmf.dbacc        colon 20 label "Account" SKIP(.5)
    dbcmf.NAME         COLON 20 LABEL "Account Name"
    dbcmf.Sortname     COLON 70 LABEL "Sort Name"SKIP(.5)
    dbcmf.RegID        COLON 20 LABEL "ID/Co. Reg no" SKIP(.5)
    dbcmf.VatNo        COLON 20 LABEL "VAT Registration" SKIP(.5)
    dbcmf.Add1         colon 20 label "Address"
    dbcmf.Add2         colon 20
    dbcmf.Add3         colon 20 SKIP(.5)
    dbcmf.emailAdd     COLON 20 LABEL "Email Address" SPACE(2)
    dbcmf.email                 LABEL "Send Email"  SKIP(.5)
    dbcmf.cell         COLON 20 LABEL "Cell Number" SPACE(2)
    dbcmf.sms                   LABEL "Send SMS"  SKIP(.5)
    dbcmf.POwner       COLON 20 LABEL "Property Onwer" SKIP(.5)
    btn-Cons           COLON 5  
    dbcmf.cons         NO-LABEL
    dbctf.Descrip      NO-LABEL VIEW-AS TEXT SKIP(1)
    "-------------PROPERTY DETAILS ---------------" AT ROW 2 COL 100
    dbcmf.standNo      AT ROW 3 COL 100 LABEL "Stand Number" SPACE(2)
    dbcmf.DeedNo          LABEL "Deed Number"
    btn-Sub            AT ROW 4.5 COL 100
    dbcmf.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT
    btn-Ward           AT ROW 6   COL 100 
    dbcmf.Ward         NO-LABEL 
    dbwrd.Descrip      NO-LABEL VIEW-AS TEXT
    dbcmf.SIZE         AT ROW 7.5 COL 100 LABEL "     Stand Size"
    dbcmf.sitevalue    AT ROW 9   COL 100  LABEL "    Site Value"
    dbcmf.bldvalue     AT ROW 10.5 COL 100 LABEL "Building Value"
    skip(0.5)
    /*brw-tarif AT ROW 13 COL 99 */
    btn-ok  AT ROW 21  COL 10 SPACE(30)
     SPACE(15) btn-close 
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 1.4 COL 98
    /*rect-5 AT ROW 12.5  COL 98 */
    rect-6 AT ROW 20.5  COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE SIZE 160 BY 24
     TITLE "RATEPAYER ACCOUNT MAINTENANCE".
    
DEF FRAME frm-dbcmf
    brw-dbcmf AT ROW 2 COL 15
    btn-add AT ROW 20.7 COL 15
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 120 BY 24 CENTERED
    TITLE "RATEPAYER ACCOUNT MAINTENANCE".

DEF    QUERY qry-Cons FOR dbctf SCROLLING.
DEF BROWSE brw-Cons QUERY qry-Cons
    DISPLAY dbctf.cons dbctf.Descrip WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-Cons 
    brw-Cons AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Consumer Type Selection".

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


ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Update" THEN DO:
        DO TRANSACTION:
            FIND CURRENT dbcmf EXCLUSIVE-LOCK.
            IF CURRENT-CHANGED dbcmf THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE
            ASSIGN  dbcmf.NAME      = dbcmf.NAME:SCREEN-VALUE
                    dbcmf.Sortname  = dbcmf.Sortname:SCREEN-VALUE
                    dbcmf.RegId     = dbcmf.RegId:SCREEN-VALUE
                    dbcmf.VatNo     = int(dbcmf.VatNo:SCREEN-VALUE)
                    dbcmf.StandNo   = int(dbcmf.StandNo:SCREEN-VALUE)
                    dbcmf.Add1      = dbcmf.Add1:SCREEN-VALUE
                    dbcmf.Add2      = dbcmf.Add2:SCREEN-VALUE
                    dbcmf.Add3      = dbcmf.Add3:SCREEN-VALUE
                    dbcmf.suburb    = int(dbcmf.suburb:SCREEN-VALUE)
                    dbcmf.ward      = int(dbcmf.ward:SCREEN-VALUE)
                    dbcmf.cons      = int(dbcmf.cons:SCREEN-VALUE)
                    dbcmf.SIZE      = DEC(dbcmf.SIZE:SCREEN-VALUE)
                    dbcmf.SiteValue = DEC(dbcmf.SiteValue:SCREEN-VALUE)
                    dbcmf.BldValue  = DEC(dbcmf.BldValue:SCREEN-VALUE)
                    dbcmf.DeedNo    = dbcmf.DeedNo:SCREEN-VALUE
                    dbcmf.emailAdd  = dbcmf.emailAdd:SCREEN-VALUE
                    dbcmf.cell      = int(dbcmf.cell:SCREEN-VALUE)
                    dbcmf.credate   = TODAY
                    dbcmf.AccStat = 0
                    dbcmf.email dbcmf.sms dbcmf.POwner.
            FIND CURRENT dbcmf NO-LOCK.
        END. /* Do Transaction */
        HIDE FRAME frm-input.
        APPLY 'close' TO THIS-PROCEDURE.
        brw-dbcmf:REFRESH() IN FRAME frm-dbcmf.
        RETURN.
    END.
    ELSE DO:
        FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE)NO-ERROR.
        IF AVAILABLE dbcmf THEN DO:
            MESSAGE "Account alread exist.... no record saved" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE dbcmf.
             ASSIGN dbcmf.dbacc     = DEC(dbcmf.dbacc:SCREEN-VALUE)
                    dbcmf.NAME      = dbcmf.NAME:SCREEN-VALUE
                    dbcmf.Sortname  = dbcmf.Sortname:SCREEN-VALUE
                    dbcmf.RegId     = dbcmf.RegId:SCREEN-VALUE
                    dbcmf.VatNo     = int(dbcmf.VatNo:SCREEN-VALUE)
                    dbcmf.StandNo   = int(dbcmf.StandNo:SCREEN-VALUE)
                    dbcmf.Add1      = dbcmf.Add1:SCREEN-VALUE
                    dbcmf.Add2      = dbcmf.Add2:SCREEN-VALUE
                    dbcmf.Add3      = dbcmf.Add3:SCREEN-VALUE
                    dbcmf.suburb    = int(dbcmf.suburb:SCREEN-VALUE)
                    dbcmf.ward      = int(dbcmf.ward:SCREEN-VALUE)
                    dbcmf.cons      = int(dbcmf.cons:SCREEN-VALUE)
                    dbcmf.SIZE      = DEC(dbcmf.SIZE:SCREEN-VALUE)
                    dbcmf.SiteValue = DEC(dbcmf.SiteValue:SCREEN-VALUE)
                    dbcmf.BldValue  = DEC(dbcmf.BldValue:SCREEN-VALUE)
                    dbcmf.DeedNo    = dbcmf.DeedNo:SCREEN-VALUE
                    dbcmf.emailAdd  = dbcmf.emailAdd:SCREEN-VALUE
                    dbcmf.cell      = int(dbcmf.cell:SCREEN-VALUE)
                    dbcmf.credate   = TODAY
                    dbcmf.AccStat = 0.
            ASSIGN dbcmf.email dbcmf.sms dbcmf.POwner.
        END.
         APPLY 'close' TO THIS-PROCEDURE.
         brw-dbcmf:REFRESH() IN FRAME frm-dbcmf.
        RUN dbsgr10a.p.
        CLEAR FRAME frm-input ALL.
        APPLY 'entry' TO dbcmf.dbacc.
        RETURN.
     END.
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    RETURN.
END.

ON LEAVE OF dbcmf.dbacc IN FRAME frm-input
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE) NO-ERROR.
   IF AVAILABLE dbcmf THEN DO:
      MESSAGE "Record already exist" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RETURN.    
END.

/*...... Triggers for button on-form buttons ....*/
/*------------------ Consumer Button -------------*/
ON CHOOSE OF btn-Cons IN FRAME frm-input
   /* OR CHOOSE OF btn-Cons IN FRAME frm-edit */
DO:
  VIEW FRAME frm-Cons.
  OPEN QUERY qry-Cons FOR EACH dbctf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Cons.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Cons 
          OR close of THIS-PROCEDURE IN FRAME frm-Cons
          OR CHOOSE OF btn-ok IN FRAME frm-Cons 
          OR 'enter':u OF brw-Cons
          OR 'mouse-select-dblclick' OF brw-Cons.
  CLOSE QUERY qry-Cons.
  HIDE FRAME frm-Cons.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Cons 
    OR 'enter':u OF brw-Cons
    OR 'mouse-select-dblclick' OF brw-Cons
DO: 
   GET CURRENT qry-Cons EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbctf.Descrip dbctf.cons @ dbcmf.cons  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF dbcmf.cons IN FRAME frm-input
    OR 'tab':U OF dbcmf.cons IN FRAME frm-input
DO:
    FIND FIRST dbctf WHERE dbctf.cons = INT(dbcmf.cons:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbctf THEN
        DISPLAY dbctf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Consumer...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

/*------------------ Ward Button -------------*/
ON CHOOSE OF btn-Ward IN FRAME frm-input
DO:
  VIEW FRAME frm-Ward.
  OPEN QUERY qry-Ward FOR EACH dbwrd NO-LOCK.
  ENABLE ALL WITH FRAME frm-Ward.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Ward 
          OR close of THIS-PROCEDURE IN FRAME frm-Ward
          OR CHOOSE OF btn-ok IN FRAME frm-Ward 
          OR 'enter':u OF brw-Ward
          OR 'mouse-select-dblclick' OF brw-Ward.
  CLOSE QUERY qry-Ward.
  HIDE FRAME frm-Ward.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ward
    OR 'enter':u OF brw-Ward
    OR 'mouse-select-dblclick' OF brw-Ward
DO: 
   GET CURRENT qry-Ward EXCLUSIVE-LOCK NO-WAIT.
        DISPLAY dbwrd.Descrip dbwrd.Ward @ dbcmf.ward  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF dbcmf.ward IN FRAME frm-input
    OR 'tab':U OF dbcmf.ward IN FRAME frm-input
DO:
    FIND FIRST dbwrd WHERE dbwrd.Ward = INT(dbcmf.ward:SCREEN-VALUE) NO-ERROR.
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
  ENABLE ALL WITH FRAME frm-Sub.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Sub 
          OR close of THIS-PROCEDURE IN FRAME frm-Sub
          OR CHOOSE OF btn-ok IN FRAME frm-Sub 
          OR 'enter':u OF brw-Sub
          OR 'mouse-select-dblclick' OF brw-Sub.
  CLOSE QUERY qry-Sub.
  HIDE FRAME frm-Sub.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Sub
    OR 'enter':u OF brw-Sub
    OR 'mouse-select-dblclick' OF brw-Sub
DO: 
   GET CURRENT qry-Sub EXCLUSIVE-LOCK NO-WAIT.
        DISPLAY dbsmf.Descrip dbsmf.suburb @ dbcmf.suburb  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF dbcmf.ward IN FRAME frm-input
    OR 'tab':U OF dbcmf.ward IN FRAME frm-input
DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb= INT(dbcmf.suburb:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbsmf THEN
        DISPLAY dbsmf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Suburb...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-Add IN FRAME frm-dbcmf DO:
    btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-dbcmf 
    OR 'mouse-select-dblclick' OF brw-dbcmf 
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-dbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = dbcmf.dbacc.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = wsid EXCLUSIVE-LOCK NO-ERROR.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-dbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = dbcmf.dbacc.
    FIND FIRST dbtat WHERE dbtat.dbacc  = dbcmf.dbacc NO-ERROR.
    IF AVAILABLE dbtat THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbtat THEN DO:
     DELETE dbcmf.
     Method-Status = brw-dbcmf:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-dbcmf FOR EACH dbcmf NO-LOCK.
ENABLE ALL WITH FRAME Frm-dbcmf.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-dbcmf.
CLOSE QUERY qry-dbcmf.
HIDE FRAME frm-dbcmf.

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO dbcmf.dbacc.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
 OPEN QUERY qry-dbcmf FOR EACH dbcmf NO-LOCK.
END.

PROCEDURE proc-Edit:
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT dbcmf.dbacc WITH FRAME frm-input.
   DISPLAY dbcmf WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
END.
