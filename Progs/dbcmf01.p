/* Program.................dbcmf01.p
   Notes:...... Debtors file maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED VAR varUser    AS CHAR FORM "xxx" INIT "1".
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
DEF VAR wsDes     LIKE dbtmf.descrip EXTENT 15 FORM "x(30)".
DEF VAR wsTar    AS LOGICAL INITIAL NO.
DEF VAR wst AS INT.
DEF VAR X AS INT.
DEF VAR wsSearch   LIKE dbcmf.NAME.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
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
DEF BUTTON btn-Water  LABEL "Add/Update Meter".
DEF BUTTON btn-Tar    LABEL "Rates Tarif".
DEF BUTTON btn-Rate   LABEL "Add Rates/Services".
DEF BUTTON btn-t1     LABEL "Tarif-1 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t2     LABEL "Tarif-2 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t3     LABEL "Tarif-3 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t4     LABEL "Tarif-4 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t5     LABEL "Tarif-5 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t6     LABEL "Tarif-6 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t7     LABEL "Tarif-7 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t8     LABEL "Tarif-8 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t9     LABEL "Tarif-9 " SIZE 8.5 BY 1.1.
DEF BUTTON btn-t10     LABEL "Tarif-10" SIZE 8.5 BY 1.1.
DEF BUTTON btn-t11     LABEL "Tarif-11" SIZE 8.5 BY 1.1.
DEF BUTTON btn-t12     LABEL "Tarif-12" SIZE 8.5 BY 1.1.
DEF BUTTON btn-t13     LABEL "Tarif-13" SIZE 8.5 BY 1.1.
DEF BUTTON btn-t14     LABEL "Tarif-14" SIZE 8.5 BY 1.1.

DEF BUFFER bfrdbcmf FOR dbcmf.

DEFINE QUERY qry-tarif FOR dbtat scrolling.
DEF BROWSE brw-tarif QUERY qry-tarif
    DISPLAY dbtat.tarif varDescrip LABEL "Tariff" dbtat.Units 
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-picktarif FOR dbtmf scrolling.
DEF BROWSE brw-picktarif QUERY qry-picktarif
    DISPLAY  dbtmf.type dbtmf.Tarif dbtmf.Descrip dbtmf.Charge
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-bfrdbcmf FOR bfrdbcmf scrolling.
DEF BROWSE brw-bfrdbcmf QUERY qry-bfrdbcmf
    DISPLAY bfrdbcmf.dbacc  bfrdbcmf.NAME bfrdbcmf.Regid LABEL "ID/Co. Registration"
    bfrdbcmf.StandNo bfrdbcmf.Suburb bfrdbcmf.POwner bfrdbcmf.AccBal WIDTH 15
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
     size 102.5 by 19.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 71 by 19.5.

define rectangle rect-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define rectangle rect-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 174 by 2.3.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.


define frame frm-input
    SKIP (0.5)
     "------------------ACCOUNT DETAILS ------------------" COLON 10 SKIP
    bfrdbcmf.dbacc        colon 20 label "Account" SKIP(.2)
    bfrdbcmf.NAME         COLON 20 LABEL "Account Name"
    bfrdbcmf.Sortname     COLON 77.5 LABEL "Sort Name"SKIP(.2)
    bfrdbcmf.RegID        COLON 20 LABEL "ID/Co. Reg no" 
    bfrdbcmf.VatNo        COLON 77.5 LABEL "VAT Registration" SKIP(.2)
    bfrdbcmf.Add1         colon 20 label "Address"
    bfrdbcmf.Add2         colon 20
    bfrdbcmf.Add3         colon 20 bfrdbcmf.cell    LABEL "Cell Number" 
    bfrdbcmf.sms          LABEL "Send SMS"  SKIP(.2)
    bfrdbcmf.emailAdd     COLON 20    LABEL "Email Address" 
    bfrdbcmf.email        COLON 93.2  LABEL "Send Email"  SKIP(.2)
    btn-Cons           COLON 3 NO-TAB-STOP  
    bfrdbcmf.cons         NO-LABEL
    dbctf.Descrip      NO-LABEL VIEW-AS TEXT SKIP(0.2)
    "-------------PROPERTY DETAILS ---------------" COLON 20 SKIP(0.2)
    bfrdbcmf.standNo      COLON 20 LABEL "Stand Number" 
     bfrdbcmf.POwner      COLON 50 LABEL "Property Onwer" 
    bfrdbcmf.DeedNo       COLON 77.5    LABEL "Deed Number" SKIP(0.2)
    bfrdbcmf.stno         COLON 20  LABEL "Street Number"
    bfrdbcmf.street       COLON 50 LABEL "Street Name" SKIP(0.2)
    btn-Sub            COLON 11 NO-TAB-STOP 
    bfrdbcmf.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT  SKIP(.2)
    btn-Ward           COLON 12.5 NO-TAB-STOP 
    bfrdbcmf.Ward         NO-LABEL 
    dbwrd.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfrdbcmf.SIZE         COLON 20 LABEL "Stand Size"
    bfrdbcmf.sitevalue     LABEL "    Site Value"
    bfrdbcmf.bldvalue     LABEL "Building Value" SKIP(.2)
    btn-Tar            COLON 8 NO-TAB-STOP 
    bfrdbcmf.rate-tarif   NO-LABEL
    dbtmf.descrip       NO-LABEL VIEW-AS TEXT 
    bfrdbcmf.rNextP         COLON 90.5 LABEL " Next Billing Period" skip(0.2)
    
    "-------------SERVICE AND AVAILABILITY TARIFS---------------" AT ROW 1.5 COL 110 SKIP(0.5)
    "           TARIF   UNITS     NEXT PERIOD" COLON 110 SKIP(0.2)
    btn-t1 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[1] NO-LABEL bfrdbcmf.units[1] NO-LABEL bfrdbcmf.NextP[1] NO-LABEL wsdes[1] NO-LABEL VIEW-AS TEXT
    btn-t2 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[2] NO-LABEL bfrdbcmf.units[2] NO-LABEL bfrdbcmf.NextP[2] NO-LABEL wsdes[2] NO-LABEL VIEW-AS TEXT
    btn-t3 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[3] NO-LABEL bfrdbcmf.units[3] NO-LABEL bfrdbcmf.NextP[3] NO-LABEL wsdes[3] NO-LABEL VIEW-AS TEXT
    btn-t4 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[4] NO-LABEL bfrdbcmf.units[4] NO-LABEL bfrdbcmf.NextP[4] NO-LABEL wsdes[4] NO-LABEL VIEW-AS TEXT
    btn-t5 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[5] NO-LABEL bfrdbcmf.units[5] NO-LABEL bfrdbcmf.NextP[5] NO-LABEL wsdes[5] NO-LABEL VIEW-AS TEXT
    btn-t6 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[6] NO-LABEL bfrdbcmf.units[6] NO-LABEL bfrdbcmf.NextP[6] NO-LABEL wsdes[6] NO-LABEL VIEW-AS TEXT
    btn-t7 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[7] NO-LABEL bfrdbcmf.units[7] NO-LABEL bfrdbcmf.NextP[7] NO-LABEL wsdes[7] NO-LABEL VIEW-AS TEXT
    btn-t8 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[8] NO-LABEL bfrdbcmf.units[8] NO-LABEL bfrdbcmf.NextP[8] NO-LABEL wsdes[8] NO-LABEL VIEW-AS TEXT
    btn-t9 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[9] NO-LABEL bfrdbcmf.units[9] NO-LABEL bfrdbcmf.NextP[9] NO-LABEL wsdes[9] NO-LABEL VIEW-AS TEXT
    btn-t10 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[10] NO-LABEL bfrdbcmf.units[10] NO-LABEL bfrdbcmf.NextP[10] NO-LABEL wsdes[10] NO-LABEL VIEW-AS TEXT
    btn-t11 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[11] NO-LABEL bfrdbcmf.units[11] NO-LABEL bfrdbcmf.NextP[11] NO-LABEL wsdes[11] NO-LABEL VIEW-AS TEXT
    btn-t12 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[12] NO-LABEL bfrdbcmf.units[12] NO-LABEL bfrdbcmf.NextP[12] NO-LABEL wsdes[12] NO-LABEL VIEW-AS TEXT
    btn-t13 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[13] NO-LABEL bfrdbcmf.units[13] NO-LABEL bfrdbcmf.NextP[13] NO-LABEL wsdes[13] NO-LABEL VIEW-AS TEXT
    btn-t14 COLON 105 NO-TAB-STOP bfrdbcmf.tarif[14] NO-LABEL bfrdbcmf.units[14] NO-LABEL bfrdbcmf.NextP[14] NO-LABEL wsdes[14] NO-LABEL VIEW-AS TEXT
    btn-water COLON 105 NO-TAB-STOP dbmmf.Tarif   NO-LABEL dbmmf.wNextP NO-LABEL VIEW-AS TEXT wsdes[15] FORM "x(30)" NO-LABEL VIEW-AS TEXT
    btn-ok  AT ROW 21.6  COL 30
    btn-close AT ROW 21.6  COL 120
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 1.4 COL 106
    rect-6 AT ROW 21  COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE SIZE 180 BY 24
     TITLE "RATEPAYER ACCOUNT MAINTENANCE".
    
DEF FRAME frm-bfrdbcmf
    brw-bfrdbcmf AT ROW 2 COL 15
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

DEFINE FRAME frm-WTarifinput
    dbmmf.Meter      COLON 20 LABEL "Meter Number" SKIP(.2)
    dbmmf.Serial     COLON 20 LABEL "Meter Serial Number" SKIP(.2)
    dbmmf.Route      COLON 20 LABEL "Route" SKIP(.2)
    btn-Tar          COLON 20 LABEL "Water Tariff" 
    dbmmf.tarif               NO-LABEL VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    dbmmf.READ[13]   COLON 20 LABEL "Meter Reading"
    dbmmf.rDate[12]  LABEL "Reading Date" SKIP(.2)
    dbmmf.wNextP     COLON 20 LABEL "Next Billing Period"
    skip(1.5)  
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter and Tariff Assignment".

DEFINE FRAME frm-Tarifinput 
    btn-Tar           COLON 15 LABEL "Click to Select Tariff" 
    dbtat.tarif         NO-LABEL VIEW-AS TEXT
    dbtmf.Descrip       NO-LABEL VIEW-AS TEXT SKIP(.2)
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

ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO bfrdbcmf.dbacc IN FRAME frm-input.
    RETURN NO-APPLY.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Update" 
        OR (btn-ok:LABEL IN  FRAME frm-input) = "TARIF UPDATE" THEN DO:
        /*DO TRANSACTION: */
            FIND CURRENT bfrdbcmf EXCLUSIVE-LOCK.
            IF CURRENT-CHANGED bfrdbcmf THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE
            ASSIGN  bfrdbcmf.NAME      = bfrdbcmf.NAME:SCREEN-VALUE
                    bfrdbcmf.Sortname  = bfrdbcmf.Sortname:SCREEN-VALUE
                    bfrdbcmf.RegId     = bfrdbcmf.RegId:SCREEN-VALUE
                    bfrdbcmf.VatNo     = int(bfrdbcmf.VatNo:SCREEN-VALUE)
                    bfrdbcmf.StandNo   = bfrdbcmf.StandNo:SCREEN-VALUE
                    bfrdbcmf.StNo      = bfrdbcmf.StNo:SCREEN-VALUE
                    bfrdbcmf.Street    = bfrdbcmf.Street:SCREEN-VALUE
                    bfrdbcmf.Add1      = bfrdbcmf.Add1:SCREEN-VALUE
                    bfrdbcmf.Add2      = bfrdbcmf.Add2:SCREEN-VALUE
                    bfrdbcmf.Add3      = bfrdbcmf.Add3:SCREEN-VALUE
                    bfrdbcmf.suburb    = int(bfrdbcmf.suburb:SCREEN-VALUE)
                    bfrdbcmf.ward      = int(bfrdbcmf.ward:SCREEN-VALUE)
                    bfrdbcmf.cons      = int(bfrdbcmf.cons:SCREEN-VALUE)
                    bfrdbcmf.SIZE      = DEC(bfrdbcmf.SIZE:SCREEN-VALUE)
                    bfrdbcmf.SiteValue = DEC(bfrdbcmf.SiteValue:SCREEN-VALUE)
                    bfrdbcmf.BldValue  = DEC(bfrdbcmf.BldValue:SCREEN-VALUE)
                    bfrdbcmf.DeedNo    = bfrdbcmf.DeedNo:SCREEN-VALUE
                    bfrdbcmf.emailAdd  = bfrdbcmf.emailAdd:SCREEN-VALUE
                    bfrdbcmf.cell      = DEC(bfrdbcmf.cell:SCREEN-VALUE)
                    bfrdbcmf.credate   = TODAY
                    bfrdbcmf.AccStat   = 0
                    bfrdbcmf.Usr       = varUser
                    bfrdbcmf.email bfrdbcmf.sms bfrdbcmf.POwner
                    bfrdbcmf.rate-tarif = INT(bfrdbcmf.rate-tarif:SCREEN-VALUE)
                    bfrdbcmf.rNextP     = DEC(bfrdbcmf.rNextP:SCREEN-VALUE) WHEN INT(bfrdbcmf.rate-tarif:SCREEN-VALUE) <> 0 .
            RUN tarif-upd.
            FIND CURRENT bfrdbcmf NO-LOCK.
       /* END. *//* Do Transaction */
        HIDE FRAME frm-input.
        APPLY 'close' TO THIS-PROCEDURE.
        brw-bfrdbcmf:REFRESH() IN FRAME frm-bfrdbcmf.
        RETURN.
    END.
    ELSE DO:
        IF bfrdbcmf.dbacc:SCREEN-VALUE = "" THEN DO:
            MESSAGE "BLANK not a valid Account...." VIEW-AS ALERT-BOX.
            APPLY 'entry' TO bfrdbcmf.dbacc IN FRAME frm-input.
            RETURN NO-APPLY.
        END.
        ELSE IF  DEC(bfrdbcmf.dbacc:SCREEN-VALUE) <> 0 THEN DO:
            FIND FIRST bfrdbcmf WHERE bfrdbcmf.dbacc = DEC(bfrdbcmf.dbacc:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE bfrdbcmf THEN DO:
                MESSAGE "Account alread exist.... no record saved" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                CREATE bfrdbcmf.
                 ASSIGN bfrdbcmf.dbacc     = DEC(bfrdbcmf.dbacc:SCREEN-VALUE)
                        bfrdbcmf.NAME      = bfrdbcmf.NAME:SCREEN-VALUE
                        bfrdbcmf.Sortname  = bfrdbcmf.Sortname:SCREEN-VALUE
                        bfrdbcmf.RegId     = bfrdbcmf.RegId:SCREEN-VALUE
                        bfrdbcmf.VatNo     = int(bfrdbcmf.VatNo:SCREEN-VALUE)
                        bfrdbcmf.StandNo   = bfrdbcmf.StandNo:SCREEN-VALUE
                        bfrdbcmf.StNo      = bfrdbcmf.StNo:SCREEN-VALUE
                        bfrdbcmf.Street    = bfrdbcmf.Street:SCREEN-VALUE
                        bfrdbcmf.Add1      = bfrdbcmf.Add1:SCREEN-VALUE
                        bfrdbcmf.Add2      = bfrdbcmf.Add2:SCREEN-VALUE
                        bfrdbcmf.Add3      = bfrdbcmf.Add3:SCREEN-VALUE
                        bfrdbcmf.suburb    = int(bfrdbcmf.suburb:SCREEN-VALUE)
                        bfrdbcmf.ward      = int(bfrdbcmf.ward:SCREEN-VALUE)
                        bfrdbcmf.cons      = int(bfrdbcmf.cons:SCREEN-VALUE)
                        bfrdbcmf.SIZE      = DEC(bfrdbcmf.SIZE:SCREEN-VALUE)
                        bfrdbcmf.SiteValue = DEC(bfrdbcmf.SiteValue:SCREEN-VALUE)
                        bfrdbcmf.BldValue  = DEC(bfrdbcmf.BldValue:SCREEN-VALUE)
                        bfrdbcmf.DeedNo    = bfrdbcmf.DeedNo:SCREEN-VALUE
                        bfrdbcmf.emailAdd  = bfrdbcmf.emailAdd:SCREEN-VALUE
                        bfrdbcmf.cell      = DEC(bfrdbcmf.cell:SCREEN-VALUE)
                        bfrdbcmf.credate   = TODAY
                        bfrdbcmf.AccStat = 0
                        bfrdbcmf.rate-tarif = INT(bfrdbcmf.rate-tarif:SCREEN-VALUE)
                        bfrdbcmf.rNextP     = DEC(bfrdbcmf.rNextP:SCREEN-VALUE) WHEN INT(bfrdbcmf.rate-tarif:SCREEN-VALUE) <> 0.
                RUN Tarif-upd.
                ASSIGN bfrdbcmf.email bfrdbcmf.sms bfrdbcmf.POwner.
            END.
            RELEASE bfrdbcmf.
           CLEAR FRAME frm-input ALL.
           ASSIGN bfrdbcmf.email:SCREEN-VALUE = "NO"
                   bfrdbcmf.Sms:SCREEN-VALUE = "NO"
                   bfrdbcmf.POwner:SCREEN-VALUE = "NO".
           VIEW FRAME frm-input.
           APPLY 'entry' TO bfrdbcmf.dbacc.
        END. /* eof if dbacc <> 0 */
     END. /* eof ELSE */
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
    RETURN.
END.

ON LEAVE OF bfrdbcmf.dbacc IN FRAME frm-input
DO:
   IF  DEC(bfrdbcmf.dbacc:SCREEN-VALUE) <> 0 THEN DO:
       FIND FIRST bfrdbcmf WHERE bfrdbcmf.dbacc = DEC(bfrdbcmf.dbacc:SCREEN-VALUE) NO-ERROR.
       IF AVAILABLE bfrdbcmf AND btn-ok:LABEL = "SAVE" THEN DO:
          MESSAGE "Record already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
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
   DISPLAY dbctf.Descrip dbctf.cons @ bfrdbcmf.cons  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfrdbcmf.cons IN FRAME frm-input
    OR 'tab':U OF bfrdbcmf.cons IN FRAME frm-input
DO:
    FIND FIRST dbctf WHERE dbctf.cons = INT(bfrdbcmf.cons:SCREEN-VALUE) NO-ERROR.
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
   DISPLAY dbwrd.Descrip dbwrd.Ward @ bfrdbcmf.ward  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfrdbcmf.ward IN FRAME frm-input
    OR 'tab':U OF bfrdbcmf.ward IN FRAME frm-input
DO:
    FIND FIRST dbwrd WHERE dbwrd.Ward = INT(bfrdbcmf.ward:SCREEN-VALUE) NO-ERROR.
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
        DISPLAY dbsmf.Descrip dbsmf.suburb @ bfrdbcmf.suburb  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF bfrdbcmf.suburb IN FRAME frm-input
    OR 'tab':U OF bfrdbcmf.suburb IN FRAME frm-input
DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = INT(bfrdbcmf.suburb:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbsmf THEN
        DISPLAY dbsmf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Suburb...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-Add IN FRAME frm-bfrdbcmf DO:
    btn-ok:LABEL IN  FRAME frm-input = "SAVE".
    /*RUN proc-input.
    RETURN. */
   CLEAR FRAME frm-input ALL.
   ASSIGN bfrdbcmf.email:SCREEN-VALUE = "NO"
           bfrdbcmf.Sms:SCREEN-VALUE = "NO"
           bfrdbcmf.POwner:SCREEN-VALUE = "NO".
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT WITH FRAME frm-input.
   APPLY 'entry' TO bfrdbcmf.dbacc.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-bfrdbcmf FOR EACH bfrdbcmf NO-LOCK.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-bfrdbcmf 
    OR 'mouse-select-dblclick' OF brw-bfrdbcmf 
DO: 
    btn-ok:LABEL IN FRAME frm-input = "UPDATE".
    wstar = NO.
    GET CURRENT qry-bfrdbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfrdbcmf.dbacc.
    DISABLE bfrdbcmf.dbacc WITH FRAME frm-input.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Tarif IN FRAME frm-bfrdbcmf 
DO:
    btn-ok:LABEL IN  FRAME frm-input = "TARIF UPDATE".
    wsTar = YES.
    GET CURRENT qry-bfrdbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfrdbcmf.dbacc.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-bfrdbcmf 
DO:
    GET CURRENT qry-bfrdbcmf EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfrdbcmf.dbacc.
    FIND FIRST dbhtf WHERE dbhtf.dbacc  = wsid NO-LOCK NO-ERROR.
    IF AVAILABLE dbhtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbhtf THEN DO:
       FIND FIRST dbmmf WHERE dbmmf.dbacc = wsid NO-ERROR.
       IF AVAILABLE dbmmf THEN
           DELETE dbmmf.
       DELETE bfrdbcmf.
       Method-Status = brw-bfrdbcmf:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/* *** Triggers to Tarif allocations frames *** */

ON CHOOSE OF btn-Tar IN FRAME frm-WTarifinput /* Water Tarif */
DO:
    VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 1 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  wsChoice = "Water".
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

ON CHOOSE OF btn-Tar IN FRAME frm-input /* Rates Tarif */
DO:
    VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 2 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  wsChoice = "Rates".
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

ON CHOOSE OF btn-T1 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 1.
  RUN btn-t.
  APPLY 'tab' TO bfrdbcmf.tarif[1].
END.
ON CHOOSE OF btn-T2 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 2.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[2].
END.
ON CHOOSE OF btn-T3 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 3.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[3].
END.
ON CHOOSE OF btn-T4 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 4.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[4].
END.
ON CHOOSE OF btn-T5 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 5.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[5].
END.
ON CHOOSE OF btn-T6 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 6.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[6].
END.
ON CHOOSE OF btn-T7 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 7.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[7].
END.
ON CHOOSE OF btn-T8 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 8.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[8].
END.
ON CHOOSE OF btn-T9 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 9.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[9].
END.
ON CHOOSE OF btn-T10 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 10.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[10].
END.
ON CHOOSE OF btn-T11 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 11.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[11].
END.
ON CHOOSE OF btn-T12 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 12.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[12].
END.
ON CHOOSE OF btn-T13 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 13.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[13].
END.
ON CHOOSE OF btn-T14 IN FRAME frm-input /* Service Tarif */
DO:
   ASSIGN wsChoice = "Services"
          X = 14.
  RUN btn-t.
   APPLY 'tab' TO bfrdbcmf.tarif[14].
END.

ON 'leave':U OF bfrdbcmf.tarif[1] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[1]:SCREEN-VALUE).
    X = 1.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[2] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[2]:SCREEN-VALUE).
    X = 2.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[3] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[3]:SCREEN-VALUE).
    X = 3.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[4] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[4]:SCREEN-VALUE).
    X = 4.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[5] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[5]:SCREEN-VALUE).
    X = 5.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[6] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[6]:SCREEN-VALUE).
    X = 6.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[7] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[7]:SCREEN-VALUE).
    X = 7.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[8] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[8]:SCREEN-VALUE).
    X = 8.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[9] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[9]:SCREEN-VALUE).
    X = 9.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[10] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[10]:SCREEN-VALUE).
    X = 10.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[11] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[11]:SCREEN-VALUE).
    X = 11.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[12] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[12]:SCREEN-VALUE).
    X = 12.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[13] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[13]:SCREEN-VALUE).
    X = 13.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
    RETURN.
END.
ON 'leave':U OF bfrdbcmf.tarif[14] IN FRAME frm-input 
DO:
    wst = int(bfrdbcmf.tarif[14]:SCREEN-VALUE).
    X = 14.
    RUN field-tarif.
    IF wst = 9999 THEN
       RETURN NO-APPLY.
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
   ELSE  IF wsChoice = "Rates"  THEN
        DISPLAY dbtmf.Tarif @ bfrdbcmf.rate-tarif dbtmf.Descrip  WITH FRAME frm-Input.
       ELSE IF wsChoice = "Services" THEN DO:
           DISPLAY dbtmf.Tarif @ bfrdbcmf.tarif[X] dbtmf.Descrip @ wsDes[X] WITH FRAME frm-input.
           APPLY 'tab' TO SELF.
       END.
           
   APPLY 'tab' TO SELF.
   RETURN.
END.

ON 'leave':U OF bfrdbcmf.rate-tarif IN FRAME frm-input
DO:
    IF INT(bfrdbcmf.rate-tarif:SCREEN-VALUE) <> 0 AND dbtmf.SOURCE = 4 THEN DO:
        FIND FIRST dbqty WHERE dbqty.dbacc = bfrdbcmf.dbacc NO-ERROR.
        IF NOT AVAILABLE dbqty THEN DO:
           CREATE dbqty.
           ASSIGN dbqty.dbAcc = bfrdbcmf.dbacc.
        END.
    END.
    RETURN.
END.

ON CHOOSE OF btn-Water IN FRAME frm-Input
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
                dbmmf.wNextP    = DEC(dbmmf.wNextP:SCREEN-VALUE)
                dbmmf.READ[13] = int(dbmmf.READ[13]:SCREEN-VALUE).
     END.
     ELSE DO:
         CREATE dbmmf.
         ASSIGN dbmmf.Serial   = dbmmf.Serial:SCREEN-VALUE IN FRAME frm-WTarifinput
                dbmmf.Route    = INT(dbmmf.Route:SCREEN-VALUE)
                dbmmf.Tarif    = INT(dbmmf.Tarif:SCREEN-VALUE)
                dbmmf.READ[12] = DEC(dbmmf.READ[13]:SCREEN-VALUE)
                dbmmf.rDate[12] = DATE(dbmmf.rDate[12]:SCREEN-VALUE)
                dbmmf.wNextP    = DEC(dbmmf.wNextP:SCREEN-VALUE)
                dbmmf.dbacc     = bfrdbcmf.dbacc
                dbmmf.Meter     = INT(dbmmf.Meter:SCREEN-VALUE).
                
     END.
     IF dbmmf.tarif <> 0 AND mStat <> 2 THEN
         mStats          = 1.
     ELSE mStats          = 3.
     DISABLE dbmmf.Meter WITH FRAME frm-WTarifInput.
     DISPLAY dbmmf.Tarif dbtmf.descrip @ wsdes[15] dbmmf.wNextP WITH FRAME frm-input.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-WTarifInput.
 END.

 on 'start-search':u of browse brw-bfrdbcmf
    run SEARCH.ip.

/*ON 'choose':U OF btnSearch IN FRAME search-opt 
    OR 'enter':U OF btnSearch IN FRAME search-opt
    OR 'tab':U OF btnSearch IN FRAME search-opt
DO:
    open query qry-bfrdbcmf preselect 
                    each bfrdbcmf no-lock 
                        where bfrdbcmf.SortName >= wsSearch:SCREEN-VALUE USE-INDEX name.
    /*RETURN.*/
END. */
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-bfrdbcmf:allow-column-searching = true.
OPEN QUERY qry-bfrdbcmf FOR EACH bfrdbcmf NO-LOCK.
ENABLE ALL WITH FRAME Frm-bfrdbcmf.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-bfrdbcmf.
CLOSE QUERY qry-bfrdbcmf.
HIDE FRAME frm-bfrdbcmf.

/******* INTERNAL PROCEDURES *********/
PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   ASSIGN bfrdbcmf.email:SCREEN-VALUE = "NO"
          bfrdbcmf.Sms:SCREEN-VALUE = "NO"
          bfrdbcmf.POwner:SCREEN-VALUE = "NO".
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO bfrdbcmf.dbacc.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-bfrdbcmf FOR EACH bfrdbcmf NO-LOCK.
END.

PROCEDURE proc-Edit:
    wsdescrip = "".
   CLEAR FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT bfrdbcmf.dbacc WITH FRAME frm-input.
   IF wstar = YES THEN 
      DISABLE ALL EXCEPT btn-Tar bfrdbcmf.rate-tarif bfrdbcmf.RNextP btn-water dbmmf.wNextP btn-ok btn-close
                         btn-t1  bfrdbcmf.tarif[1]  bfrdbcmf.units[1]   
                         btn-t2  bfrdbcmf.tarif[2]  bfrdbcmf.units[2]  
                         btn-t3  bfrdbcmf.tarif[3]  bfrdbcmf.units[3] 
                         btn-t4  bfrdbcmf.tarif[4]  bfrdbcmf.units[4]  
                         btn-t5  bfrdbcmf.tarif[5]  bfrdbcmf.units[5]  
                         btn-t6  bfrdbcmf.tarif[6]  bfrdbcmf.units[6]  
                         btn-t7  bfrdbcmf.tarif[7]  bfrdbcmf.units[7]  
                         btn-t8  bfrdbcmf.tarif[8]  bfrdbcmf.units[8]  
                         btn-t9  bfrdbcmf.tarif[9]  bfrdbcmf.units[9]  
                         btn-t10  bfrdbcmf.tarif[10]  bfrdbcmf.units[10]  
                         btn-t11  bfrdbcmf.tarif[11]  bfrdbcmf.units[11]  
                         btn-t12  bfrdbcmf.tarif[12]  bfrdbcmf.units[12]  
                         btn-t13  bfrdbcmf.tarif[13]  bfrdbcmf.units[13]  
                         btn-t14  bfrdbcmf.tarif[14]  bfrdbcmf.units[14]  WITH FRAME frm-input.
   FIND FIRST dbwrd WHERE dbwrd.ward = bfrdbcmf.ward NO-LOCK NO-ERROR.
   FIND FIRST dbsmf WHERE dbsmf.suburb = bfrdbcmf.suburb NO-LOCK NO-ERROR.
   FIND FIRST dbctf WHERE dbctf.cons = bfrdbcmf.cons NO-LOCK NO-ERROR.
   FIND FIRST dbtmf WHERE dbtmf.tarif = bfrdbcmf.rate-tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
   IF AVAILABLE dbtmf THEN
       wsdescrip = dbtmf.descrip.
   DISPLAY bfrdbcmf EXCEPT bfrdbcmf.AccStat bfrdbcmf.Credate bfrdbcmf.Usr 
            bfrdbcmf.AccBal bfrdbcmf.BalBf bfrdbcmf.lpdate  WITH FRAME frm-input.
   DISPLAY dbctf.Descrip dbsmf.Descrip dbwrd.Descrip wsdescrip @ dbtmf.descrip WITH FRAME frm-input.  
   DO X = 1 TO 14:
       IF bfrdbcmf.tarif[X] <> 0 THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.tarif = bfrdbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
           wsdes[X] = dbtmf.Descrip.
           DISPLAY wsdes[x] WITH FRAME frm-input.
       END.
  FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-LOCK NO-ERROR.
  IF AVAILABLE dbmmf THEN
      DISPLAY dbmmf.tarif dbmmf.wNextP WITH FRAME frm-input.
  FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
  IF AVAILABLE dbtmf THEN
      DISPLAY dbtmf.descrip @ wsdes[15] WITH FRAME frm-input.
END.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
END.


PROCEDURE proc-WTarifinput:
    wsChoice = "Water".
   CLEAR FRAME frm-WtarifInput ALL.
   VIEW FRAME frm-WTarifInput.
   FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-ERROR.
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

PROCEDURE tarif-upd:
    /*IF INT(bfrdbcmf.tarif[1]:SCREEN-VALUE IN FRAME frm-input) <> bfrdbcmf.tarif[1] 
        OR bfrdbcmf.units[1] <> DEC(bfrdbcmf.units[1]:SCREEN-VALUE) THEN */
          ASSIGN bfrdbcmf.tarif[1] = INT(bfrdbcmf.tarif[1]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[1]:MODIFIED IN FRAME frm-input
                 bfrdbcmf.units[1] = DEC(bfrdbcmf.units[1]:SCREEN-VALUE) WHEN bfrdbcmf.Units[1]:MODIFIED
                 bfrdbcmf.NextP[1] = DEC(bfrdbcmf.NextP[1]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[1]:MODIFIED
                 bfrdbcmf.tarif[2] = INT(bfrdbcmf.tarif[2]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[2]:MODIFIED
                 bfrdbcmf.units[2] = DEC(bfrdbcmf.units[2]:SCREEN-VALUE) WHEN bfrdbcmf.Units[2]:MODIFIED
                 bfrdbcmf.NextP[2] = DEC(bfrdbcmf.NextP[2]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[2]:MODIFIED           
                 bfrdbcmf.tarif[3] = INT(bfrdbcmf.tarif[3]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[3]:MODIFIED
                 bfrdbcmf.units[3] = DEC(bfrdbcmf.units[3]:SCREEN-VALUE) WHEN bfrdbcmf.Units[3]:MODIFIED
                 bfrdbcmf.NextP[3] = DEC(bfrdbcmf.NextP[3]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[3]:MODIFIED
                 bfrdbcmf.tarif[4] = INT(bfrdbcmf.tarif[4]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[4]:MODIFIED
                 bfrdbcmf.units[4] = DEC(bfrdbcmf.units[4]:SCREEN-VALUE) WHEN bfrdbcmf.Units[4]:MODIFIED
                 bfrdbcmf.NextP[4] = DEC(bfrdbcmf.NextP[4]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[4]:MODIFIED
                 bfrdbcmf.tarif[5] = INT(bfrdbcmf.tarif[5]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[5]:MODIFIED
                 bfrdbcmf.units[5] = DEC(bfrdbcmf.units[5]:SCREEN-VALUE) WHEN bfrdbcmf.Units[5]:MODIFIED
                 bfrdbcmf.NextP[5] = DEC(bfrdbcmf.NextP[5]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[5]:MODIFIED
                 bfrdbcmf.tarif[6] = INT(bfrdbcmf.tarif[6]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[6]:MODIFIED
                 bfrdbcmf.units[6] = DEC(bfrdbcmf.units[6]:SCREEN-VALUE) WHEN bfrdbcmf.Units[6]:MODIFIED
                 bfrdbcmf.NextP[6] = DEC(bfrdbcmf.NextP[6]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[6]:MODIFIED
                 bfrdbcmf.tarif[7] = INT(bfrdbcmf.tarif[7]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[7]:MODIFIED
                 bfrdbcmf.units[7] = DEC(bfrdbcmf.units[7]:SCREEN-VALUE) WHEN bfrdbcmf.Units[7]:MODIFIED
                 bfrdbcmf.NextP[7] = DEC(bfrdbcmf.NextP[7]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[7]:MODIFIED
                 bfrdbcmf.tarif[8] = INT(bfrdbcmf.tarif[8]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[8]:MODIFIED
                 bfrdbcmf.units[8] = DEC(bfrdbcmf.units[8]:SCREEN-VALUE) WHEN bfrdbcmf.Units[8]:MODIFIED
                 bfrdbcmf.NextP[8] = DEC(bfrdbcmf.NextP[8]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[8]:MODIFIED
                 bfrdbcmf.tarif[9] = INT(bfrdbcmf.tarif[9]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[9]:MODIFIED
                 bfrdbcmf.units[9] = DEC(bfrdbcmf.units[9]:SCREEN-VALUE) WHEN bfrdbcmf.Units[9]:MODIFIED
                 bfrdbcmf.NextP[9] = DEC(bfrdbcmf.NextP[9]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[9]:MODIFIED
                 bfrdbcmf.tarif[10] = INT(bfrdbcmf.tarif[10]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[10]:MODIFIED
                 bfrdbcmf.units[10] = DEC(bfrdbcmf.units[10]:SCREEN-VALUE) WHEN bfrdbcmf.Units[10]:MODIFIED
                 bfrdbcmf.NextP[10] = DEC(bfrdbcmf.NextP[10]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[10]:MODIFIED
                 bfrdbcmf.tarif[11] = INT(bfrdbcmf.tarif[11]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[11]:MODIFIED
                 bfrdbcmf.units[11] = DEC(bfrdbcmf.units[11]:SCREEN-VALUE) WHEN bfrdbcmf.Units[11]:MODIFIED
                 bfrdbcmf.NextP[11] = DEC(bfrdbcmf.NextP[11]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[10]:MODIFIED
                 bfrdbcmf.tarif[12] = INT(bfrdbcmf.tarif[12]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[12]:MODIFIED
                 bfrdbcmf.units[12] = DEC(bfrdbcmf.units[12]:SCREEN-VALUE) WHEN bfrdbcmf.Units[12]:MODIFIED
                 bfrdbcmf.NextP[12] = DEC(bfrdbcmf.NextP[12]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[12]:MODIFIED           
                 bfrdbcmf.tarif[13] = INT(bfrdbcmf.tarif[13]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[13]:MODIFIED
                 bfrdbcmf.units[13] = DEC(bfrdbcmf.units[13]:SCREEN-VALUE) WHEN bfrdbcmf.Units[13]:MODIFIED
                 bfrdbcmf.NextP[13] = DEC(bfrdbcmf.NextP[13]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[13]:MODIFIED
                 bfrdbcmf.tarif[14] = INT(bfrdbcmf.tarif[14]:SCREEN-VALUE) WHEN bfrdbcmf.tarif[14]:MODIFIED
                 bfrdbcmf.units[14] = DEC(bfrdbcmf.units[14]:SCREEN-VALUE) WHEN bfrdbcmf.Units[14]:MODIFIED
                 bfrdbcmf.NextP[14] = DEC(bfrdbcmf.NextP[14]:SCREEN-VALUE) WHEN bfrdbcmf.NextP[14]:MODIFIED.
  DO X = 1 TO 14: /*Check for query quantity file */
      IF bfrdbcmf.tarif[X] <> 0 THEN DO:
          FIND FIRST dbtmf WHERE dbtmf.tarif = bfrdbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
          IF dbtmf.SOURCE = 4 THEN DO:
              FIND FIRST dbqty WHERE dbqty.dbacc = bfrdbcmf.dbacc NO-LOCK NO-ERROR.
              IF NOT AVAILABLE dbqty THEN DO:
                  CREATE dbqty.
                  ASSIGN dbqty.dbacc = bfrdbcmf.dbacc.
              END.
          END.
      END.
  END.
END.

PROCEDURE btn-t:
  VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 3 NO-LOCK.
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

PROCEDURE field-tarif:
    IF wst <> 0 THEN DO:
        FIND FIRST dbtmf WHERE dbtmf.TYPE = 3 AND dbtmf.tarif = wst NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbtmf AND wst <> 0 THEN DO:
            MESSAGE "Invalid tarif" VIEW-AS ALERT-BOX.
            wst = 9999.
        END. 
        ELSE DO:
            ASSIGN wsdes[X] = dbtmf.Descrip.
            IF dbtmf.SOURCE = 4 THEN DO:
                FIND FIRST dbqty WHERE dbqty.dbacc = bfrdbcmf.dbacc NO-ERROR.
                IF NOT AVAILABLE dbqty THEN DO:
                    CREATE dbqty.
                    ASSIGN dbqty.dbAcc = bfrdbcmf.dbacc.
                END.
            END.
            DISPLAY wsdes[X] WITH FRAME frm-input.
        END.
    END.
    RETURN.
END.

procedure Search.ip.
    hCol = browse brw-bfrdbcmf:current-column.
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
               open query qry-bfrdbcmf
                        FOR EACH bfrdbcmf no-lock 
                            where bfrdbcmf.sortname >= wsSearch:SCREEN-VALUE USE-INDEX NAME.

            END.
            when "Account" then
            do:
               open query qry-bfrdbcmf 
                        FOR EACH bfrdbcmf no-lock 
                            where bfrdbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbAcc.

            END.
            when "StandNo" then
            do:
               open query qry-bfrdbcmf 
                        FOR EACH bfrdbcmf NO-LOCK
                            where bfrdbcmf.StandNo >= wsSearch:SCREEN-VALUE USE-INDEX Stand.

            END.
        END.
        RETURN.
    END.

