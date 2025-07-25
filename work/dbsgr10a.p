session:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Acb ~
                                        COLUMN-LABEL ' Cashbook ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        FORM "x(40)" COLUMN-LABEL ' Description ':C ~
                               bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C
DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varDescrip LIKE dbtmf.Descrip.
DEF VAR varFactor LIKE  dbtat.Units.

DEFINE QUERY qry-tarif FOR dbtat scrolling.
DEF BROWSE brw-tarif QUERY qry-tarif
    DISPLAY dbtat.tarif varDescrip LABEL "Tariff" dbtat.Units 
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-picktarif FOR dbtmf scrolling.
DEF BROWSE brw-picktarif QUERY qry-picktarif
    DISPLAY  dbtmf.type dbtmf.Tarif dbtmf.Descrip dbtmf.Charge
    WITH 8 DOWN SEPARATORS.

DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".
DEF BUTTON btn-Water  LABEL "Add Water Tariff".
DEF BUTTON btn-Tar    LABEL "Add Other Tariff".
DEF BUTTON btn-Rate   LABEL "Add Rates Tariff".
DEF BUTTON btn-Cons   LABEL "Consumer Type".
DEF BUTTON btn-Ward   LABEL "Ward".
DEF BUTTON btn-Suburb LABEL "Suburb".
DEF BUTTON btn-Tarif  LABEL "Tariff".
DEF BUTTON btn-acc    LABEL "Account".

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

define frame frm-main
    SKIP (1)
     "------------------ACCOUNT DETAILS ------------------" COLON 5 SKIP
    btn-acc            colon 10 
    dbcmf.dbacc        NO-LABEL
    dbcmf.NAME         NO-LABEL VIEW-AS TEXT SKIP(.5)
    dbcmf.RegID        COLON 20 LABEL "ID/Co. Reg no" SKIP(.5)
    dbcmf.VatNo        COLON 20 LABEL "VAT Registration" SKIP(.5)
    dbcmf.Add1         colon 20 label "Address"
    dbcmf.Add2         colon 20
    dbcmf.Add3         colon 20 SKIP(.5)
    dbcmf.emailAdd     COLON 20 LABEL "Email Address"  SKIP(.5)
    dbcmf.cell         COLON 20 LABEL "Cell Number"  SKIP(.5)
    dbcmf.POwner       COLON 20 LABEL "Property Onwer" SKIP(.5)
    btn-Cons           COLON 5 NO-TAB-STOP 
    dbcmf.cons         NO-LABEL
    dbctf.Descrip        NO-LABEL VIEW-AS TEXT SKIP(1)
    "-------------PROPERTY DETAILS ---------------" AT ROW 2 COL 100
    dbcmf.standNo      AT ROW 3 COL 100 LABEL "Stand Number" SPACE(2)
    dbcmf.DeedNo          LABEL "Deed Number"
    btn-Suburb         AT ROW 4.5 COL 100
    dbcmf.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT
    btn-Ward           AT ROW 6   COL 100 
    dbcmf.Ward         NO-LABEL 
    dbwrd.Descrip      NO-LABEL VIEW-AS TEXT
    dbcmf.SIZE         AT ROW 7.5 COL 100 LABEL "     Stand Size"
    dbcmf.sitevalue    AT ROW 9   COL 100  LABEL "    Site Value"
    dbcmf.bldvalue     AT ROW 10.5 COL 100 LABEL "Building Value"
    skip(0.5)
    brw-tarif AT ROW 13 COL 99
    btn-Water AT ROW 21  COL 10 SPACE(30)
    btn-Rate SPACE(15) btn-Tar SPACE(15) btn-Del SPACE(15) btn-close 
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 1.4 COL 98
    rect-5 AT ROW 12.5  COL 98
    rect-6 AT ROW 20.5  COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE SIZE 160 BY 24
     TITLE "RATEPAYER TARIFF MAINTENANCE".
    
DEFINE FRAME frm-input 
    btn-Tarif           COLON 15 NO-TAB-STOP 
    dbtat.tarif         NO-LABEL
    dbtmf.Descrip        NO-LABEL VIEW-AS TEXT SKIP(.5)
    dbtat.units         COLON 22 LABEL "Points/Units"
    skip(0.5)
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Assignment".

DEFINE FRAME frm-picktarif 
    brw-picktarif AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Selection".

/* *** Triggers to frame frm-main*** */

ON LEAVE OF dbcmf.dbacc IN FRAME frm-main
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE) NO-ERROR.
   IF AVAILABLE dbcmf THEN DO:
      DISPLAY dbcmf WITH FRAME frm-main.
      OPEN QUERY qry-tarif FOR EACH dbtat 
       WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main). 
       RETURN.
   END.
      
END.

/* ***** Triggers for the PickTarif frame **** */
ON CHOOSE OF btn-Tarif IN FRAME frm-input
DO:
  VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf NO-LOCK.
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
   DISPLAY dbtmf.Descrip WITH FRAME frm-input.
   APPLY 'tab' TO SELF.
   RETURN.
END.

/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-ok IN FRAME frm-input
 DO:
     CREATE dbtat.
     ASSIGN dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main)
            dbtat.tarif = int(dbtat.tarif:SCREEN-VALUE)
            dbtat.Units = dec(dbtat.units:SCREEN-VALUE).
     OPEN QUERY qry-tarif FOR EACH dbtat 
       WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main).
     RETURN.
 END.

ON CHOOSE OF btn-Tar IN FRAME frm-main
 DO:
     RUN proc-input.
     RETURN.
 END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main.
ENABLE btn-acc  dbcmf.dbacc  btn-Del btn-Water  btn-Rate btn-Tar btn-close WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-CLOSE OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-tarif.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   ENABLE ALL WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   HIDE FRAME frm-input.
   OPEN QUERY qry-tarif FOR EACH dbtat 
       WHERE dbtat.dbacc = DEC(dbcmf.dbacc:SCREEN-VALUE IN FRAME frm-main). 
END.

