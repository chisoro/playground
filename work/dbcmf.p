/* Program.................dbcmf.p
   Notes:...... Debtors file maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varDescrip LIKE dbtmf.Descrip.
DEF VAR varFactor LIKE  dbtat.Units.
DEF VAR wsType    LIKE dbtmf.TYPE.
DEF VAR wsR       AS INT.
DEF VAR wsRd       AS DATE.
DEF VAR wsChoice AS CHAR.
DEF VAR wsDescrip LIKE dbtmf.descrip.
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".
DEF BUTTON btn-Cons   LABEL "Consumer Type".
DEF BUTTON btn-Ward   LABEL "Ward".
DEF BUTTON btn-Sub LABEL "Suburb".
DEF BUTTON btn-Tarif  LABEL "Meter and Tariffs".
DEF BUTTON btn-acc    LABEL "Account".
DEF BUTTON btn-Water  LABEL "Add Meter/Water Tariff".
DEF BUTTON btn-Tar    LABEL "Yeeessss".
DEF BUTTON btn-Rate   LABEL "Add Rates/Services".

DEFINE QUERY qry-tarif FOR dbtat scrolling.
DEF BROWSE brw-tarif QUERY qry-tarif
    DISPLAY dbtat.tarif varDescrip LABEL "Tariff" dbtat.Units 
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-picktarif FOR dbtmf scrolling.
DEF BROWSE brw-picktarif QUERY qry-picktarif
    DISPLAY  dbtmf.type dbtmf.Tarif dbtmf.Descrip dbtmf.Charge
    WITH 8 DOWN SEPARATORS.

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
    btn-ok  AT ROW 21  COL 30
    btn-close AT ROW 21  COL 120
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 1.4 COL 98
    rect-6 AT ROW 20.5  COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE SIZE 160 BY 24
     TITLE "RATEPAYER ACCOUNT MAINTENANCE".
    
DEF FRAME frm-dbcmf
    brw-dbcmf AT ROW 2 COL 15
    btn-add AT ROW 20.7 COL 10
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-Tarif
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 120 BY 24 CENTERED
    TITLE "RATEPAYER ACCOUNT MAINTENANCE".

define frame frm-main
    SKIP (1)
     "------------------ACCOUNT DETAILS ------------------" COLON 5 SKIP
    dbcmf.dbacc        colon 20 LABEL "Account" SPACE(1)
    dbcmf.NAME         NO-LABEL  SKIP(.5)
    dbcmf.RegID        COLON 20 LABEL "ID/Co. Reg no" SKIP(.5)
    dbcmf.VatNo        COLON 20 LABEL "VAT Registration" SKIP(.5)
    dbcmf.Add1         colon 20 label "Address"
    dbcmf.Add2         colon 20
    dbcmf.Add3         colon 20 SKIP(.5)
    dbcmf.emailAdd     COLON 20 LABEL "Email Address"  SKIP(.5)
    dbcmf.cell         COLON 20 LABEL "Cell Number"  SKIP(.5)
    dbcmf.POwner       COLON 20 LABEL "Property Onwer" SKIP(.5)
    dbcmf.cons         COLON 20  LABEL "Consumer Type"
    dbctf.Descrip        NO-LABEL SKIP(1)
    "-------------PROPERTY DETAILS ---------------" AT ROW 2 COL 100
    dbcmf.standNo      COLON 110 LABEL "Stand Number" SPACE(2)
    dbcmf.DeedNo          LABEL "Deed Number" 
    dbcmf.Suburb          COLON 105 LABEL "Suburb" SPACE(1)
    dbsmf.Descrip      NO-LABEL 
    dbcmf.Ward          COLON 105 LABEL "Ward" SPACE(1)
    dbwrd.Descrip      NO-LABEL 
    dbcmf.SIZE         COLON 110 LABEL "     Stand Size" SPACE(1) "SQM"
    dbcmf.sitevalue    COLON 110 LABEL "    Site Value"
    dbcmf.bldvalue     COLON 110 LABEL "Building Value" SKIP(0.1)
    wsDescrip          COLON 110 LABEL "Water Tarif" SKIP(0.1)
    dbmmf.RDate[13]    COLON 115 LABEL "Last Reading Date" SKIP(0.1)
    dbmmf.READ[13]     COLON 115 LABEL "Last Reading" 
    skip(0.5)
    brw-tarif AT ROW 13 COL 99
    btn-Water AT ROW 21  COL 10 SPACE(30)
    btn-Rate SPACE(15) btn-Del SPACE(15) btn-close 
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 1.4 COL 98
    rect-5 AT ROW 12.5  COL 98
    rect-6 AT ROW 20.5  COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE SIZE 160 BY 24
     TITLE "RATEPAYER TARIFF ALLOCATION".
    

DEFINE FRAME frm-WTarifinput
    dbmmf.Meter      COLON 20 LABEL "Meter Number" SKIP(.5)
    dbmmf.Serial     COLON 20 LABEL "Meter Serial Number" skip(.5)
    dbmmf.Route      COLON 20 LABEL "Route" SKIP(.5)
    btn-Tar          COLON 20 LABEL "Water Tariff" 
    dbmmf.tarif               NO-LABEL VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.5)
    dbmmf.READ[13]   COLON 20 LABEL "Meter Reading"
    skip(0.5)
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter and Tariff Assignment".

DEFINE FRAME frm-Tarifinput 
    btn-Tar           COLON 15 LABEL "Click to Select Tariff" 
    dbtat.tarif         NO-LABEL VIEW-AS TEXT
    dbtmf.Descrip       NO-LABEL VIEW-AS TEXT SKIP(.5)
    dbtat.units         COLON 22 LABEL "Points/Units"
    skip(0.5)
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Assignment".

DEFINE FRAME frm-picktarif 
    brw-picktarif AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "Ok"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Selection".

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
        RELEASE dbcmf.
        APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
        /*brw-dbcmf:REFRESH() IN FRAME frm-dbcmf. */
        VIEW FRAME frm-main.
        ENABLE brw-tarif btn-Del btn-Water btn-Rate  btn-close WITH FRAME frm-main.
        FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-input).
        /*FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-input).
        MESSAGE dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-input VIEW-AS ALERT-BOX.*/
        FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-ERROR.
        FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-ERROR.
        DISPLAY dbcmf EXCEPT dbcmf.AccStat dbcmf.Credate dbcmf.Usr dbcmf.email dbcmf.Sms 
            dbcmf.AccBal dbcmf.BalBf dbcmf.SortName WITH FRAME frm-main.
        DISPLAY dbsmf.Descrip dbwrd.Descrip WITH FRAME frm-main.
        WAIT-FOR CHOOSE OF btn-CLOSE OR close of THIS-PROCEDURE IN FRAME frm-main.
        CLOSE QUERY qry-tarif.
        HIDE FRAME frm-main.
        CLEAR FRAME frm-input ALL.
        ENABLE ALL WITH FRAME frm-input.
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
    DISABLE dbcmf.dbacc WITH FRAME frm-input.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Tarif IN FRAME frm-dbcmf 
DO:
    GET CURRENT qry-dbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = dbcmf.dbacc.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = wsid EXCLUSIVE-LOCK NO-ERROR.
    VIEW FRAME frm-main.
        ENABLE brw-tarif btn-Del btn-Water btn-Rate btn-close WITH FRAME frm-main.
        FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-ERROR.
        FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-ERROR.
        FIND FIRST dbctf WHERE dbctf.cons = dbcmf.cons NO-ERROR.
        DISPLAY dbcmf EXCEPT dbcmf.AccStat dbcmf.Credate dbcmf.Usr dbcmf.email dbcmf.Sms 
            dbcmf.AccBal dbcmf.BalBf dbcmf.SortName WITH FRAME frm-main.
        DISPLAY dbctf.Descrip dbsmf.Descrip dbwrd.Descrip WITH FRAME frm-main.
        FIND FIRST dbmmf WHERE dbmmf.dbacc = wsid NO-ERROR.
        IF AVAILABLE dbmmf THEN DO:
            ASSIGN wsRd = dbmmf.Rdate[13] WHEN dbmmf.Rdate[13] <> ?
                   wsRd = dbmmf.Rdate[12] WHEN dbmmf.Rdate[13] = ?
                   wsR = dbmmf.READ[13] WHEN dbmmf.Rdate[13] <> ?
                   wsR = dbmmf.READ[12] WHEN dbmmf.Rdate[13] = ?.    
            FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-ERROR.
            IF AVAILABLE dbtmf THEN
                DISPLAY dbtmf.descrip @ wsDescrip wsRd @ dbmmf.Rdate[13] wsR @ dbmmf.READ[13] WITH FRAME frm-main.
        END.
        OPEN QUERY qry-tarif FOR EACH dbtat 
              WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main).
        WAIT-FOR CHOOSE OF btn-CLOSE OR close of THIS-PROCEDURE IN FRAME frm-main.
        CLOSE QUERY qry-tarif.
        HIDE FRAME frm-main.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-dbcmf 
DO:
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

/* *** Triggers to Tarif allocations frames *** */
ON row-display OF brw-Tarif DO:
    FIND FIRST dbtmf WHERE dbtmf.tarif = dbtat.tarif AND dbtmf.TYPE = dbtat.TYPE NO-ERROR.
    varDescrip = dbtmf.Descrip.
END.

ON CHOOSE OF btn-Tar IN FRAME frm-Tarifinput
DO:
  VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 2 OR dbtmf.TYPE = 3 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picktarif 
          OR close of THIS-PROCEDURE IN FRAME frm-picktarif
          OR CHOOSE OF btn-ok IN FRAME frm-picktarif 
          OR 'enter':u OF brw-picktarif
          OR 'mouse-select-dblclick' OF brw-picktarif.
  CLOSE QUERY qry-picktarif.
  HIDE FRAME frm-picktarif.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-Tar IN FRAME frm-WTarifinput  
DO:
  VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 1 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picktarif 
          OR close of THIS-PROCEDURE IN FRAME frm-picktarif
          OR CHOOSE OF btn-ok IN FRAME frm-picktarif 
          OR 'enter':u OF brw-picktarif
          OR 'mouse-select-dblclick' OF brw-picktarif.
  CLOSE QUERY qry-picktarif.
  HIDE FRAME frm-picktarif.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-picktarif 
    OR 'enter':u OF brw-picktarif
    OR 'mouse-select-dblclick' OF brw-picktarif
DO: 
   GET CURRENT qry-picktarif EXCLUSIVE-LOCK NO-WAIT.
   ASSIGN wsType = dbtmf.TYPE.
   IF wsChoice = "Water"  THEN
        DISPLAY dbtmf.Tarif @ dbmmf.tarif dbtmf.Descrip @ wsDescrip WITH FRAME frm-WTarifinput.
   ELSE  
       DISPLAY dbtmf.Tarif @ dbtat.tarif dbtmf.Descrip WITH FRAME frm-Tarifinput.
   APPLY 'tab' TO SELF.
   RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-Tarifinput
DO:
    FIND FIRST dbtmf WHERE dbtmf.tarif = int(dbtat.tarif:SCREEN-VALUE)NO-ERROR.
    IF AVAILABLE dbtmf THEN DO:
        FIND FIRST dbtat WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main)
                         AND dbtat.tarif = int(dbtat.tarif:SCREEN-VALUE)
                         AND dbtat.TYPE  = wsType NO-ERROR.
        IF AVAILABLE dbtat THEN DO:
            MESSAGE "Tariff already allocated to this Account" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE
        DO:
            CREATE dbtat.
             ASSIGN dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main)
                    dbtat.tarif = int(dbtat.tarif:SCREEN-VALUE)
                    dbtat.Units = dec(dbtat.units:SCREEN-VALUE)
                    dbtat.TYPE  = wsType.
             OPEN QUERY qry-tarif FOR EACH dbtat 
               WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main).
             CLEAR FRAME frm-TarifInput ALL.
             APPLY 'tab' TO SELF.
             APPLY 'tab' TO btn-close.
        END.
     END.
     ELSE
         APPLY "close" TO THIS-PROCEDURE IN FRAME frm-TarifInput.
     RETURN.
END.


ON CHOOSE OF btn-Rate IN FRAME frm-main
 DO:
     RUN proc-Tarifinput.
     RETURN.
 END.

 ON CHOOSE OF btn-Water IN FRAME frm-main
 DO:
     RUN proc-WTarifinput.
     RETURN.
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-WTarifinput
 DO:
     IF btn-ok:LABEL = "Update" THEN DO:
         ASSIGN dbmmf.Serial   = dbmmf.Serial:SCREEN-VALUE IN FRAME frm-WTarifinput
                dbmmf.Route    = INT(dbmmf.Route:SCREEN-VALUE)
                dbmmf.Tarif    = int(dbmmf.Tarif:SCREEN-VALUE)
                dbmmf.READ[13] = int(dbmmf.READ[13]:SCREEN-VALUE).
     END.
     ELSE DO:
         CREATE dbmmf.
         ASSIGN dbmmf.Serial   = dbmmf.Serial:SCREEN-VALUE IN FRAME frm-WTarifinput
                dbmmf.Route    = INT(dbmmf.Route:SCREEN-VALUE)
                dbmmf.Tarif    = INT(dbmmf.Tarif:SCREEN-VALUE)
                dbmmf.READ[13] = DEC(dbmmf.READ[13]:SCREEN-VALUE)
                dbmmf.rDate[13] = ?
                dbmmf.dbacc     = dbcmf.dbacc
                dbmmf.Meter     = INT(dbmmf.Meter:SCREEN-VALUE).
     END.
     DISABLE dbmmf.Meter WITH FRAME frm-WTarifInput.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-WTarifInput.
 END.

 ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-tarif EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = dbtat.tarif.
    FIND FIRST dbhtf WHERE dbhtf.tarif  = dbtat.tarif NO-ERROR.
    IF AVAILABLE dbhtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbhtf THEN DO:
     DELETE dbtat.
     Method-Status = brw-tarif:DELETE-SELECTED-ROWS().
    END.
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
   FIND FIRST dbwrd WHERE dbwrd.ward = dbcmf.ward NO-ERROR.
   FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-ERROR.
   FIND FIRST dbctf WHERE dbctf.cons = dbcmf.cons NO-ERROR.
   DISPLAY dbcmf EXCEPT dbcmf.AccStat dbcmf.Credate dbcmf.Usr 
            dbcmf.AccBal dbcmf.BalBf WITH FRAME frm-input.
   DISPLAY dbctf.Descrip dbsmf.Descrip dbwrd.Descrip WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
END.

PROCEDURE proc-Tarifinput:
    wsChoice = "Service".
   CLEAR FRAME frm-tarifInput ALL.
   VIEW FRAME frm-TarifInput.
   ENABLE ALL WITH FRAME frm-Tarifinput.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Tarifinput.
   HIDE FRAME frm-Tarifinput.
   OPEN QUERY qry-tarif FOR EACH dbtat 
       WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main).
   
END.

PROCEDURE proc-WTarifinput:
    wsChoice = "Water".
   CLEAR FRAME frm-WtarifInput ALL.
   VIEW FRAME frm-WTarifInput.
   FIND FIRST dbmmf WHERE dbmmf.dbacc = dbcmf.dbacc NO-ERROR.
   FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-ERROR.
   IF AVAILABLE dbtmf THEN
       wsDescrip = dbtmf.Descrip.
   ELSE wsDescrip = "".
   ENABLE ALL EXCEPT WITH FRAME frm-WTarifinput.
   IF AVAILABLE dbmmf THEN DO:
        MESSAGE "Account already has a meter update Meter Details" VIEW-AS ALERT-BOX.
        DISPLAY dbmmf.Meter wsDescrip dbmmf.Serial dbmmf.Route dbmmf.Tarif dbmmf.READ[13] WITH FRAME frm-WTarifinput.
        btn-ok:LABEL = "Update".
        DISABLE dbmmf.Meter WITH FRAME frm-WTarifinput.
   END.
   ELSE  btn-ok:LABEL = "SAVE".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-WTarifinput.
   HIDE FRAME frm-WTarifinput.
END.


