session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbtmf.p
   Notes:...... TARIFF maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "TARIFF already exist"
&SCOPED-DEFINE wsTitle           "TARIFF File Maintenance"
&SCOPED-DEFINE tmptable             dbtmf
&SCOPED-DEFINE skey                 dbtmf.tarif
&SCOPED-DEFINE UpdFields     bfr{&tmptable}.TYPE ~
                                        COLUMN-LABEL ' TYPE ':C ~
                             bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' DESCRIPTION ':C~
                              bfr{&tmptable}.iLedger ~
                                        COLUMN-LABEL ' LEDGER ':C~
                              bfr{&tmptable}.SiLedger ~
                                        COLUMN-LABEL ' LEDGER ':C~
                              bfr{&tmptable}.ViLedger ~
                                        COLUMN-LABEL ' LEDGER ':C~
                               bfr{&tmptable}.sgrp ~
                                        COLUMN-LABEL ' SERVICE ':C ~
                                bfr{&tmptable}.SOURCE ~
                                        COLUMN-LABEL ' SOURCE ':C ~
                               bfr{&tmptable}.Charge ~
                                        COLUMN-LABEL ' AMOUNT ':C ~
                              bfr{&tmptable}.Vat% ~
                                        COLUMN-LABEL ' VAT% ':C ~
                              bfr{&tmptable}.vCharge ~
                                        COLUMN-LABEL ' VARIABLE CHARGE ':C ~
                              bfr{&tmptable}.Freq ~
                                        COLUMN-LABEL ' FREQUENCY ':C

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.TYPE ~
                                        COLUMN-LABEL ' TYPE ':C ~
                              bfr{&tmptable}.tarif ~
                                        COLUMN-LABEL ' TARIFF ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' DESCRIPTION ':C ~
                              bfr{&tmptable}.iledger ~
                                        COLUMN-LABEL ' LEDGER ':C ~
                              bfr{&tmptable}.Charge ~
                                        COLUMN-LABEL ' AMOUNT ':C ~
                              bfr{&tmptable}.Vat% ~
                                        COLUMN-LABEL ' VAT% ':C ~
                               bfr{&tmptable}.Freq ~
                                        COLUMN-LABEL ' FREQ ':C
&SCOPED-DEFINE RateTarif     bfr{&tmptable}.LValue bfr{&tmptable}.BValue

&SCOPED-DEFINE WaterTarif    bfr{&tmptable}.Tear[1] bfr{&tmptable}.TCharge[1] bfr{&tmptable}.Tear[2]bfr{&tmptable}.TCharge[2] bfr{&tmptable}.Tear[3] ~
                             bfr{&tmptable}.TCharge[3]  bfr{&tmptable}.factor[1] bfr{&tmptable}.factor[2] bfr{&tmptable}.factor[3] bfr{&tmptable}.factor[4] ~
                             bfr{&tmptable}.factor[5] bfr{&tmptable}.Tear[4] bfr{&tmptable}.TCharge[4] bfr{&tmptable}.Tear[5] bfr{&tmptable}.TCharge[5]     
                             
 DEFINE RECTANGLE rect-w
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 46 by 8.                                        

{varlibrary.i}
DEF BUTTON btnsLedger LABEL "LEDGER".
DEF BUTTON btnvLedger LABEL "LEDGER".
DEF VAR wsdes LIKE glmf.DESCRIPTION EXTENT 3.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 18.5.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 7
    btn-add AT ROW 20.7 COL 5
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 105 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    SKIP(0.5)
    bfr{&tmptable}.TYPE COLON 15 LABEL "Charge Type"
                view-as combo-box list-item-pairs
       "1-WATER CHARGE",1,"2 - RATES CHARGES",2,"3 - SERVICES/AVAILABILITY",3
    skip(0.5)
    {&skey}                   COLON 10 label "TARIFF" 
    bfr{&tmptable}.descrip    COLON 30 label "Description"  skip(0.5)
    bfr{&tmptable}.SOURCE     COLON 20  LABEL "SOURCE"
             view-as combo-box list-item-pairs
       "Units/Points",1,"Area",2,"Water",3,"Quantity",4, "Value",5
    skip(0.5)
    btnService                COLON 9 NO-TAB-STOP
    bfr{&tmptable}.Sgrp       COLON 20 NO-LABEL
    SPACE(1) dbsgr.Descrip view-as FILL-IN SIZE 28 BY 1 no-label NO-TAB-STOP  skip(0.5)
    btnLedger                 COLON 8.5  NO-TAB-STOP
    bfr{&tmptable}.iLedger     NO-LABEL
    SPACE(1) wsdes[1]  view-as text no-label NO-TAB-STOP  skip(0.5)
    btnDept                 COLON 3  no-tab-stop
    bfr{&tmptable}.Dept              NO-LABEL
    SPACE(1) DDes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 7  no-tab-stop
    bfr{&tmptable}.Proj              NO-LABEL
    SPACE(1) PDes  view-as text no-label NO-TAB-STOP skip(0.5)
    bfr{&tmptable}.Freq       COLON 20 LABEL "Frequency"
                view-as combo-box list-item-pairs
       "Q-Quarterly","Q","M - Monthly","M","H - Half","H","A - Annual","A" skip(0.2)
    bfr{&tmptable}.Charge     COLON 20 LABEL "Basic Charge"  skip(0.2)
    bfr{&tmptable}.LValue     COLON 20 LABEL "Site Charge"  
    btnsLedger                 COLON 8.5  NO-TAB-STOP
    bfr{&tmptable}.SiLedger     NO-LABEL skip(0.2)
    bfr{&tmptable}.BValue     COLON 20 LABEL "Building Charge" 
    btnvLedger                 COLON 8.5  NO-TAB-STOP
    bfr{&tmptable}.ViLedger     NO-LABEL skip(0.2)
    bfr{&tmptable}.vat%       COLON 20 LABEL "VAT %" skip(0.5)
    bfr{&tmptable}.vCharge    COLON 20 LABEL "Variable Charge?" skip(0.5)
    "RANGE                  FACTOR          CHARGE "AT ROW 13 COL 46 skip(0.2)
    bfr{&tmptable}.Tear[1]     COLON 44.1 NO-LABEL
    bfr{&tmptable}.factor[1]               NO-LABEL
    bfr{&tmptable}.TCharge[1]  NO-LABEL skip(0.2)
    bfr{&tmptable}.Tear[2]     COLON 44.1 NO-LABEL
    bfr{&tmptable}.factor[2]               NO-LABEL
    bfr{&tmptable}.TCharge[2]  NO-LABEL skip(0.2)
    bfr{&tmptable}.Tear[3]     COLON 44.1 NO-LABEL
    bfr{&tmptable}.factor[3]               NO-LABEL
    bfr{&tmptable}.TCharge[3] NO-LABEL skip(0.2)
    bfr{&tmptable}.Tear[4]     COLON 44.1 NO-LABEL
    bfr{&tmptable}.factor[4]               NO-LABEL
    bfr{&tmptable}.TCharge[4]  NO-LABEL skip(0.2)
    bfr{&tmptable}.Tear[5]     COLON 44.1 NO-LABEL 
    bfr{&tmptable}.factor[5]               NO-LABEL
    bfr{&tmptable}.TCharge[5]  NO-LABEL skip(0.2)
    RECT-W                    AT ROW 12.9 COL 45.5
    SKIP(2)
    btn-ok colon 5
    btn-close colon 60
    SKIP (1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
   
/* ***** Triggers for on-frame buttons **** */    
ON CHOOSE OF btnLedger IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
   wsdes[1] = glmf.DESCRIPTION.
   DISPLAY glmf.acct @ bfr{&tmptable}.iLedger wsdes[1]  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnvLedger IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.viLedger  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnsLedger IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.siLedger  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-pickDept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickDept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickDept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickDept
          OR CHOOSE OF btn-ok IN FRAME frm-pickDept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-pickDept.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickDept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DDes = gldept.DESCRIP.
   DISPLAY gldept.dept @ bfr{&tmptable}.dept DDes  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-input
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   PDes = glproj.DESCRIPTION.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj PDes  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.dept 
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DDes = gldept.DESCRIP.
        DISPLAY DDes WITH FRAME frm-input.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.proj 
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmptable}.proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
         PDes = glproj.DESCRIPTION.
        DISPLAY PDes WITH FRAME frm-input.
    END.
    RETURN.
END.

ON CHOOSE OF btnService IN FRAME frm-input
DO:
  VIEW FRAME frm-pickService.
  OPEN QUERY qry-Service FOR EACH dbsgr NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickService.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickService 
          OR close of THIS-PROCEDURE IN FRAME frm-pickService
          OR CHOOSE OF btn-ok IN FRAME frm-pickService 
          OR 'enter':u OF brw-Service
          OR 'mouse-select-dblclick' OF brw-Service.
  CLOSE QUERY qry-Service.
  HIDE FRAME frm-pickService.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickService 
    OR 'enter':u OF brw-Service
    OR 'mouse-select-dblclick' OF brw-Service
DO: 
   GET CURRENT qry-Service EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbsgr.Sgrp @ bfr{&tmptable}.Sgrp dbsgr.Descrip  WITH FRAME frm-input.
   RETURN.
END.


/* ***** Triggers for the main frame **** */
ON CHOOSE OF btn-Add IN FRAME frm-main DO:
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
    run proc-edit.
    RETURN.
END.

ON 'tab' OF bfr{&tmptable}.TYPE IN FRAME frm-input
DO: 
    IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 1 THEN DO:
        bfr{&tmptable}.SOURCE:SCREEN-VALUE = "3".
        ENABLE {&watertarif} WITH FRAME frm-input.
        DISABLE {&ratetarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
    END.
    ELSE IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 2 THEN DO:
        bfr{&tmptable}.SOURCE:SCREEN-VALUE = "5".
        ENABLE {&ratetarif} WITH FRAME frm-input.
        DISABLE {&watertarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
    END.
    ELSE IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 3 THEN DO:
         bfr{&tmptable}.SOURCE:SCREEN-VALUE = "1".
         ENABLE {&watertarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
         DISABLE {&ratetarif} WITH FRAME frm-input.
         
    END.
END.


ON LEAVE OF {&skey} IN FRAME frm-input
    OR 'enter' OF {&skey} IN FRAME frm-input
    OR 'tab' OF {&skey} IN FRAME frm-input
DO: 
    wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
    IF wsid = 0 THEN DO:
        /*MESSAGE "No value entered" VIEW-AS ALERT-BOX. */
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid 
           AND {&tmptable}.TYPE = INT(bfr{&tmptable}.TYPE:SCREEN-VALUE)  NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.tarif.
    FIND FIRST dbtat WHERE dbta.tarif  = bfr{&tmptable}.tarif  NO-ERROR.
    IF AVAILABLE dbtat THEN DO:
        MESSAGE "TARIFF has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'enter':U OF bfr{&tmptable}.iLedger 
DO:
    IF DEC(bfr{&tmptable}.iLedger:SCREEN-VALUE) <> 0 THEN DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
         wsdes[1] = glmf.DESCRIPTION.
        FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
        IF AVAILABLE gldept THEN
             DDes = gldept.DESCRIP.
        FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
        IF AVAILABLE glproj THEN
             PDes = glproj.DESCRIPTION.
        DISPLAY glmf.Dept @ bfr{&tmptable}.Dept DDes glmf.Proj @ bfr{&tmptable}.Proj 
            PDes wsdes[1] WITH FRAME frm-input.
        IF glmf.dept <> 0 THEN
            DISABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
        ELSE  ENABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
        IF glm.proj <> 0 THEN
            DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
        ELSE ENABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
    END.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.siLedger 
DO:
    IF DEC(bfr{&tmptable}.siLedger:SCREEN-VALUE) <> 0 THEN DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.siLedger:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.viLedger 
DO:
    IF DEC(bfr{&tmptable}.viLedger:SCREEN-VALUE) <> 0 THEN DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.viLedger:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.sgrp 
DO:
    FIND FIRST dbsgr WHERE dbsgr.Sgrp = INT(bfr{&tmptable}.sgrp:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE dbsgr THEN DO:
        MESSAGE "Invalid Service entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY dbsgr.Descrip WITH FRAME frm-input.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
        CREATE bfr{&tmptable}. 
        ASSIGN bfr{&tmptable}.tarif = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.SOURCE = INT(bfr{&tmptable}.SOURCE:SCREEN-VALUE)
               bfr{&tmptable}.iLedger = DEC(bfr{&tmptable}.iLedger:SCREEN-VALUE)
               bfr{&tmptable}.siLedger = DEC(bfr{&tmptable}.siLedger:SCREEN-VALUE)
               bfr{&tmptable}.viLedger = DEC(bfr{&tmptable}.viLedger:SCREEN-VALUE)
               bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
               bfr{&tmptable}.Sgrp    = INT(bfr{&tmptable}.Sgrp:SCREEN-VALUE)
               bfr{&tmptable}.Charge  = DEC(bfr{&tmptable}.Charge:SCREEN-VALUE)
               bfr{&tmptable}.vat%    = INT(bfr{&tmptable}.vat%:SCREEN-VALUE)
               bfr{&tmptable}.vCharge   
               bfr{&tmptable}.Freq    = bfr{&tmptable}.Freq:SCREEN-VALUE
               bfr{&tmptable}.LValue  = DEC(bfr{&tmptable}.LValue:SCREEN-VALUE)  
               bfr{&tmptable}.BValue  = DEC(bfr{&tmptable}.BValue:SCREEN-VALUE)
               bfr{&tmptable}.type    = int(bfr{&tmptable}.type:SCREEN-VALUE)
               bfr{&tmptable}.Tear[1] = INT(bfr{&tmptable}.Tear[1]:SCREEN-VALUE)
               bfr{&tmptable}.Tear[2] = INT(bfr{&tmptable}.Tear[2]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[3] = INT(bfr{&tmptable}.Tear[3]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[4] = INT(bfr{&tmptable}.Tear[4]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[5] = INT(bfr{&tmptable}.Tear[5]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[1] = INT(bfr{&tmptable}.Factor[1]:SCREEN-VALUE)
               bfr{&tmptable}.Factor[2] = INT(bfr{&tmptable}.Factor[2]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[3] = INT(bfr{&tmptable}.Factor[3]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[4] = INT(bfr{&tmptable}.Factor[4]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[5] = INT(bfr{&tmptable}.Factor[5]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[1] = DEC(bfr{&tmptable}.TCharge[1]:SCREEN-VALUE)
               bfr{&tmptable}.TCharge[2] = DEC(bfr{&tmptable}.TCharge[2]:SCREEN-VALUE)
               bfr{&tmptable}.TCharge[3] = DEC(bfr{&tmptable}.TCharge[3]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[4] = DEC(bfr{&tmptable}.TCharge[4]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[5] = DEC(bfr{&tmptable}.TCharge[5]:SCREEN-VALUE).
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.SOURCE = INT(bfr{&tmptable}.SOURCE:SCREEN-VALUE)
               bfr{&tmptable}.iLedger = DEC(bfr{&tmptable}.iLedger:SCREEN-VALUE)
               bfr{&tmptable}.siLedger = DEC(bfr{&tmptable}.siLedger:SCREEN-VALUE)
               bfr{&tmptable}.viLedger = DEC(bfr{&tmptable}.viLedger:SCREEN-VALUE)
               bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
               bfr{&tmptable}.Sgrp    = INT(bfr{&tmptable}.Sgrp:SCREEN-VALUE)
               bfr{&tmptable}.Charge  = DEC(bfr{&tmptable}.Charge:SCREEN-VALUE)
               bfr{&tmptable}.vat%    = DEC(bfr{&tmptable}.vat%:SCREEN-VALUE)
               bfr{&tmptable}.vCharge  
               bfr{&tmptable}.Freq    = bfr{&tmptable}.Freq:SCREEN-VALUE
               bfr{&tmptable}.LValue  = DEC(bfr{&tmptable}.LValue:SCREEN-VALUE)
               bfr{&tmptable}.BValue  = DEC(bfr{&tmptable}.BValue:SCREEN-VALUE)
               bfr{&tmptable}.Tear[1] = INT(bfr{&tmptable}.Tear[1]:SCREEN-VALUE)
               bfr{&tmptable}.Tear[2] = INT(bfr{&tmptable}.Tear[2]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[3] = INT(bfr{&tmptable}.Tear[3]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[4] = INT(bfr{&tmptable}.Tear[4]:SCREEN-VALUE) 
               bfr{&tmptable}.Tear[5] = INT(bfr{&tmptable}.Tear[5]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[1] = INT(bfr{&tmptable}.Factor[1]:SCREEN-VALUE)
               bfr{&tmptable}.Factor[2] = INT(bfr{&tmptable}.Factor[2]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[3] = INT(bfr{&tmptable}.Factor[3]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[4] = INT(bfr{&tmptable}.Factor[4]:SCREEN-VALUE) 
               bfr{&tmptable}.Factor[5] = INT(bfr{&tmptable}.Factor[5]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[1] = DEC(bfr{&tmptable}.TCharge[1]:SCREEN-VALUE)
               bfr{&tmptable}.TCharge[2] = DEC(bfr{&tmptable}.TCharge[2]:SCREEN-VALUE)
               bfr{&tmptable}.TCharge[3] = DEC(bfr{&tmptable}.TCharge[3]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[4] = DEC(bfr{&tmptable}.TCharge[4]:SCREEN-VALUE) 
               bfr{&tmptable}.TCharge[5] = DEC(bfr{&tmptable}.TCharge[5]:SCREEN-VALUE).
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END. 
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT {&WaterTarif} {&RateTarif} WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-Edit:
   GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
   ASSIGN wsid = int(bfr{&tmptable}.tarif).
   /*FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.tarif  = wsid AND bfr{&tmptable}.TYPE NO-ERROR.*/
   FIND FIRST dbsgr WHERE dbsgr.sgrp = bfr{&tmptable}.sgrp NO-LOCK NO-ERROR.
   IF  bfr{&tmptable}.iLedger <> 0 THEN
       FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.iLedger NO-LOCK NO-ERROR.
   IF AVAILABLE glmf THEN
        wsdes[1] = glmf.DESCRIPTION.
   IF  bfr{&tmptable}.siLedger <> 0 THEN
       FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.siLedger NO-LOCK NO-ERROR.
   IF AVAILABLE glmf THEN
        wsdes[2] = glmf.DESCRIPTION.
   IF  bfr{&tmptable}.viLedger <> 0 THEN
       FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.viLedger NO-LOCK NO-ERROR.
   IF AVAILABLE glmf THEN
        wsdes[3] = glmf.DESCRIPTION.
   FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
   IF AVAILABLE gldept THEN
        DDes = gldept.DESCRIP.
   FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
   IF AVAILABLE glproj THEN
    PDes = glproj.DESCRIPTION.
   DISPLAY wsid @ {&skey} {&UpdFields} {&watertarif} {&RateTarif} 
       wsdes[1] dbsgr.Descrip DDes PDes WITH FRAME frm-input.
   /* CLEAR FRAME frm-input ALL. */
   ENABLE ALL EXCEPT bfr{&tmptable}.TYPE {&skey}  WITH FRAME frm-input.
   IF glmf.dept <> 0 THEN
       DISABLE btndept bfr{&tmptable}.dept WITH FRAME frm-input.
   IF glmf.proj <> 0 THEN
       DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-input.
   IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 1 THEN DO:
        bfr{&tmptable}.SOURCE:SCREEN-VALUE = "3".
        DISABLE bfr{&tmptable}.SOURCE WITH FRAME frm-input.
        ENABLE {&watertarif} WITH FRAME frm-input.
        DISABLE {&ratetarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
    END.
    ELSE IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 2 THEN DO:
        bfr{&tmptable}.SOURCE:SCREEN-VALUE = "5".
        ENABLE {&ratetarif} WITH FRAME frm-input.
        DISABLE {&watertarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
    END.
    ELSE IF INT(bfr{&tmptable}.TYPE:SCREEN-VALUE) = 3 THEN DO:
         DISABLE {&ratetarif} WITH FRAME frm-input.
         ENABLE {&watertarif} bfr{&tmptable}.vCharge WITH FRAME frm-input.
    END.
        
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

procedure Search.ip.
    hCol = browse brw-{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column"
                /*frame Search-opt:x     = hCol:x
                frame Search-opt:y     = browse brw-{&tmptable}:y */.
       IF trim(hCol:label) <> "DESCRIPTION" AND trim(hCol:label)<> "LEDGER"  AND trim(hCol:label)<> "TARIFF" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
        case trim(hCol:label):
            when "DESCRIPTION" then
            do:
               OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.Descrip.

            END.
            when "LEDGER" then
            do:
               OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.iLedger >= INT(wsSearch:SCREEN-VALUE) USE-INDEX iledger.
            END.
            when "TARIFF" then
            do:
               OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.tarif >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX Tariff.

            END.
            OTHERWISE DO:
               RETURN NO-APPLY.
            END.     
        END.
        RETURN.
    END.
