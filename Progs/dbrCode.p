session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbrCode.p
   Notes:...... Receipting cosde file maintenance
   Author:.................S. Mawire
   Modified:..............S. Chisoro
*/
&SCOPED-DEFINE wsMsg           "Receipting already exist"
&SCOPED-DEFINE wsTitle           "Receipting Code File Maintenance"
&SCOPED-DEFINE tmptable             dbRCod
&SCOPED-DEFINE skey                 dbRCod.rCode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                              bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C~
                               bfr{&tmptable}.TARGET ~
                                        COLUMN-LABEL ' Receipt To ':C~
                               bfr{&tmptable}.sgrp ~
                                        COLUMN-LABEL ' Service ':C ~
                               bfr{&tmptable}.Track ~
                                        COLUMN-LABEL ' Track ':C ~
                               bfr{&tmptable}.hsCode ~
                                        COLUMN-LABEL ' HS Code ':C ~
                                bfr{&tmptable}.taxID ~
                                        COLUMN-LABEL ' Tax ID ':C ~
                               bfr{&tmptable}.taxCode ~
                                        COLUMN-LABEL ' Tax Code ':C ~
                               bfr{&tmptable}.VAT ~
                                        COLUMN-LABEL ' VAT ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.rCode ~
                                        COLUMN-LABEL ' Receipting Code ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.sgrp ~
                                        COLUMN-LABEL ' Service ':C ~
                              bfr{&tmptable}.acb ~
                                        COLUMN-LABEL ' Cashbook ':C ~
                              bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C ~
                              bfr{&tmptable}.VAT ~
                                        COLUMN-LABEL ' VAT ':C
                              
DEF BUTTON btnTax LABEL "Tax ID".
{varlibrary.i}
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.

DEF    QUERY qry-pickTax FOR fdmsTax SCROLLING.
DEF BROWSE brw-pickTax QUERY qry-pickTax
        DISPLAY fdmsTax.taxID fdmsTax.taxName fdmsTax.taxPerc
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickTax 
        brw-pickTax AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tax Selection".


DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                   COLON 30 label "Receipting Code" skip(0.5)
    bfr{&tmptable}.descrip    COLON 30 label "Description" skip(0.5)
    bfr{&tmptable}.TARGET     COLON 30 LABEL "Receipt to"
           view-as combo-box list-item-pairs
                     "C-Consumer","C","V-Vote","V","L-Landsales","L" skip(0.5)
    btnService                 COLON 18.5 NO-TAB-STOP
    bfr{&tmptable}.sgrp                 NO-LABEL
    SPACE(1) dbsgr.Descrip  view-as text no-label NO-TAB-STOP skip(0.5)
    btnLedger                 COLON 18 NO-TAB-STOP 
    bfr{&tmptable}.Ledger              NO-LABEL
    SPACE(1) ADes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnDept                 COLON 6.5  no-tab-stop
    bfr{&tmptable}.Dept              NO-LABEL
    SPACE(1) DDes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 17  no-tab-stop
    bfr{&tmptable}.Proj              NO-LABEL
    SPACE(1) PDes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnAcb                    COLON 10 NO-TAB-STOP
    bfr{&tmptable}.Acb                 NO-LABEL
    SPACE(1) cbkmf.descrip  view-as text no-label NO-TAB-STOP skip(0.5)
    btnTax COLON 18.5 NO-TAB-STOP
    bfr{&tmptable}.taxID NO-LABEL
    SPACE(1) fdmsTax.taxName VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    bfr{&tmptable}.VAT       COLON 30 LABEL "VAT %" SKIP(0.5)
    bfr{&tmptable}.hsCode COLON 30 LABEL "HS CODE" SKIP(0.5)
    bfr{&tmptable}.taxCode COLON 30 LABEL "TAX CODE" SKIP(0.5)
    bfr{&tmptable}.track       COLON 30 LABEL "Event Tracking Y/N"
    skip(2)
    btn-ok colon 15
    btn-close colon 60 SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
/* ***** Triggers for on-frame buttons **** */    
ON CHOOSE OF btnTax IN FRAME frm-input
DO:
  VIEW FRAME frm-pickTax.
  OPEN QUERY qry-pickTax FOR EACH fdmsTax WHERE fdmsTax.taxID < 10 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickTax.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickTax 
          OR close of THIS-PROCEDURE IN FRAME frm-pickTax
          OR CHOOSE OF btn-ok IN FRAME frm-pickTax 
          OR 'enter':u OF brw-pickTax
          OR 'mouse-select-dblclick' OF brw-pickTax.
  CLOSE QUERY qry-pickTax.
  HIDE FRAME frm-pickTax.
  APPLY 'tab' TO SELF.
    RETURN.
END.
 ON 'enter':U OF bfr{&tmptable}.taxID IN frame frm-input
 DO:
     IF INT(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) = 0 THEN DO:
            fdmsTax.taxName:SCREEN-VALUE IN FRAME frm-input  = "".
            VAT:SCREEN-VALUE IN FRAME frm-input = "0". 
     END.
     ELSE DO:
         FIND FIRST fdmsTax WHERE fdmsTax.taxID = INT(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) NO-LOCK NO-ERROR.
         IF AVAILABLE fdmsTax THEN DO:
                DISPLAY fdmsTax.TaxID @ bfr{&tmptable}.taxID fdmsTax.taxName  fdmsTax.taxPerc @ bfr{&tmptable}.VAT WITH FRAME frm-input.
         END.
         IF NOT AVAILABLE fdmsTax THEN DO:
             MESSAGE "Tax ID Does not exist for this device"  VIEW-AS ALERT-BOX.
         END.
     END.
     RETURN.
 END.

ON CHOOSE OF btn-ok IN FRAME frm-pickTax 
    OR 'enter':u OF brw-pickTax
    OR 'mouse-select-dblclick' OF brw-pickTax
DO: 
   GET CURRENT qry-pickTax EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY fdmsTax.TaxID @ bfr{&tmptable}.taxID fdmsTax.taxName  fdmsTax.taxPerc @ bfr{&tmptable}.VAT WITH FRAME frm-input.
   RETURN.
END.

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
   Ades = glmf.DESCRIPTION.
   DISPLAY glmf.acct @ bfr{&tmptable}.Ledger ADes  WITH FRAME frm-input.
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
   Ddes = DDes.
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
   Pdes = glproj.DESCRIPTION.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj PDes  WITH FRAME frm-input.
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
   IF dbsgr.Sgrp <> 0  THEN
      DISPLAY dbsgr.Sgrp @ bfr{&tmptable}.sgrp dbsgr.Descrip  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnAcb IN FRAME frm-input
DO:
  VIEW FRAME frm-pickAcb.
  OPEN QUERY qry-Acb FOR EACH cbkmf /*WHERE cbkmf.acb <> 0 */ NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcb.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickAcb 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcb
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcb 
          OR 'enter':u OF brw-Acb
          OR 'mouse-select-dblclick' OF brw-Acb.
  CLOSE QUERY qry-Acb.
  HIDE FRAME frm-pickAcb.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcb 
    OR 'enter':u OF brw-Acb
    OR 'mouse-select-dblclick' OF brw-Acb
DO: 
   GET CURRENT qry-Acb EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbkmf.bank @ bfr{&tmptable}.Acb cbkmf.descrip  WITH FRAME frm-input.
   RETURN.
END. 

ON 'enter' OF  bfr{&tmptable}.Acb IN FRAME frm-input
    OR 'TAB' OF  bfr{&tmptable}.Acb IN FRAME frm-input
DO: 
     FIND FIRST cbkmf WHERE cbkmf.bank = INT( bfr{&tmptable}.Acb:SCREEN-VALUE) NO-LOCK NO-ERROR.
       IF NOT AVAILABLE cbkmf AND INT( bfr{&tmptable}.Acb:SCREEN-VALUE) <> 0 THEN
       DO:
          MESSAGE  "Invalid Cashbook.." VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
       ELSE IF AVAILABLE cbkmf THEN
       DO:
         DISPLAY cbkmf.bank @ bfr{&tmptable}.Acb cbkmf.descrip  WITH FRAME frm-input.
          APPLY 'TAB' TO SELF.
       END.
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
    CLEAR FRAME frm-input.
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.rCode.
    FIND FIRST dbRec WHERE dbRec.rCode  = bfr{&tmptable}.rCode  NO-ERROR.
    IF AVAILABLE Dbrec THEN DO:
        MESSAGE "Receipting Code has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'enter' OF {&skey} IN FRAME frm-input
    OR 'leave' OF {&skey} IN FRAME frm-input
DO: 
     wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
    IF wsid = "0" THEN DO:
       CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
    END.
END.

ON 'enter':U OF bfr{&tmptable}.Ledger 
DO:
    IF DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE) <> 0 THEN DO:
   
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            Ades = glmf.DESCRIPTION.
            IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
                FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
                IF AVAILABLE gldept THEN
                    DDes = gldept.descrip.
                FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
                IF AVAILABLE glproj THEN
                    Pdes = glproj.DESCRIPTION.
                DISPLAY glmf.Dept @ bfr{&tmptable}.Dept DDes glmf.Proj @ bfr{&tmptable}.Proj 
                    pDes ADes WITH FRAME frm-input.
            END.
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

ON 'leave':U OF bfr{&tmptable}.TARGET 
DO:
    IF  bfr{&tmptable}.TARGET:SCREEN-VALUE = "C" THEN DO:
       /*DISABLE btnLedger bfr{&tmptable}.ledger 
               btnDept bfr{&tmptable}.Dept
               btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input. */
       ENABLE btnService bfr{&tmptable}.sgrp WITH FRAME frm-Input.
    END.
   ELSE IF  bfr{&tmptable}.TARGET:SCREEN-VALUE = "V" THEN DO:
       ENABLE btnLedger bfr{&tmptable}.ledger 
              btnDept bfr{&tmptable}.Dept
              btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
       DISABLE btnService bfr{&tmptable}.sgrp WITH FRAME frm-Input.
   END.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.dept 
DO:
    IF INT(bfr{&tmptable}.dept:SCREEN-VALUE) <> 0 THEN DO:
        FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE gldept THEN DO:
            MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DDes = gldept.descrip.
            DISPLAY DDes WITH FRAME frm-input.
        END.
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
        PDes = glProj.descrip.
        DISPLAY PDes WITH FRAME frm-input.
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
  wsValid = YES.
  IF NOT CAN-FIND(dbsgr WHERE dbsgr.Sgrp = INT(bfr{&tmptable}.sgrp:SCREEN-VALUE))
      AND  bfr{&tmptable}.TARGET:SCREEN-VALUE = "C" THEN
           ASSIGN wsValid = NO WHEN INT(bfr{&tmptable}.sgrp:SCREEN-VALUE) <> 0.
  IF NOT CAN-FIND(glmf WHERE glmf.acct = DEC(bfr{&tmptable}.ledger:SCREEN-VALUE))
       AND bfr{&tmptable}.TARGET:SCREEN-VALUE = "V" THEN
      wsValid = NO.
  IF NOT CAN-FIND(cbkmf WHERE cbkmf.bank = INT(bfr{&tmptable}.Acb:SCREEN-VALUE)) THEN
     wsValid = NO.
  IF {&skey}:SCREEN-VALUE = "0" OR {&skey}:SCREEN-VALUE = ""  THEN
     wsValid = NO.
  IF wsValid = NO THEN DO:
      MESSAGE "Invalid entries found on the input. No record saved" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  ELSE DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.rCode = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
               bfr{&tmptable}.Dept    = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj    = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
               bfr{&tmptable}.Sgrp    = INT(bfr{&tmptable}.sgrp:SCREEN-VALUE)
               bfr{&tmptable}.Acb     = DEC(bfr{&tmptable}.Acb:SCREEN-VALUE)
               bfr{&tmptable}.TARGET  = bfr{&tmptable}.TARGET:SCREEN-VALUE IN FRAME frm-input
                  bfr{&tmptable}.hsCode = bfr{&tmptable}.hsCode:SCREEN-VALUE IN FRAME frm-input
                  bfr{&tmptable}.taxCode = bfr{&tmptable}.taxCode:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.taxID = int(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) WHEN int(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) <> 0 
                  bfr{&tmptable}.VAT    = DEC(bfr{&tmptable}.VAT:SCREEN-VALUE)
               bfr{&tmptable}.track    = LOGICAL(bfr{&tmptable}.track:SCREEN-VALUE).
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
               bfr{&tmptable}.Dept    = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj    = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
               bfr{&tmptable}.Sgrp    = INT(bfr{&tmptable}.sgrp:SCREEN-VALUE)
               bfr{&tmptable}.Acb     = DEC(bfr{&tmptable}.Acb:SCREEN-VALUE)
               bfr{&tmptable}.TARGET  = bfr{&tmptable}.TARGET:SCREEN-VALUE
                bfr{&tmptable}.hsCode = bfr{&tmptable}.hsCode:SCREEN-VALUE IN FRAME frm-input
                  bfr{&tmptable}.taxCode = bfr{&tmptable}.taxCode:SCREEN-VALUE IN FRAME frm-input
                  bfr{&tmptable}.taxID = int(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) WHEN int(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) <> 0
                  bfr{&tmptable}.taxID = int("") WHEN int(bfr{&tmptable}.taxID:SCREEN-VALUE IN FRAME frm-input) = 0 
               bfr{&tmptable}.VAT    = DEC(bfr{&tmptable}.VAT:SCREEN-VALUE)
               bfr{&tmptable}.track   = LOGICAL(bfr{&tmptable}.track:SCREEN-VALUE).
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
  END.
END.
/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-Edit:
  ASSIGN wsid = bfr{&tmptable}.rCode.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.rCode  = wsid NO-ERROR.
   IF  bfr{&tmptable}.TARGET = "C" THEN DO:
       FIND FIRST dbsgr WHERE dbsgr.sgrp = bfr{&tmptable}.sgrp NO-ERROR.
       DISABLE btnLedger bfr{&tmptable}.ledger
               btnDept bfr{&tmptable}.Dept
               btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
       ENABLE btnService bfr{&tmptable}.sgrp WITH FRAME frm-Input.
       IF  bfr{&tmptable}.sgrp <> 0 THEN
         DISPLAY  dbsgr.sgrp @ bfr{&tmptable}.sgrp dbsgr.Descrip WITH FRAME frm-input.
       FIND FIRST gldept WHERE gldept.dept = bfr{&tmptable}.dept NO-LOCK NO-ERROR.
       IF AVAILABLE gldept THEN DO:
           DISPLAY bfr{&tmptable}.dept gldept.descrip @ ddes WITH FRAME frm-input.
       END.
   END.
   ELSE IF  bfr{&tmptable}.TARGET = "V"  THEN DO:
       FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Ledger NO-ERROR.
       IF AVAILABLE glmf THEN
           ADes = glmf.DESCRIPTION.
       ELSE ADes = "INVALID LEDGER".
       FIND FIRST gldept WHERE gldept.dept = bfr{&tmptable}.dept NO-LOCK NO-ERROR.
       IF AVAILABLE gldept THEN
           DDes = gldept.descrip.
       ELSE Ddes = "Invalid Department".
       FIND FIRST glproj WHERE glproj.proj = bfr{&tmptable}.proj NO-LOCK NO-ERROR.
       IF AVAILABLE glproj THEN
           PDes = GlProj.DESCRIPTION.
       ELSE PDes = "Invalid Project".
       ENABLE btnLedger bfr{&tmptable}.ledger 
              btnDept bfr{&tmptable}.Dept
              btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
       DISABLE btnService bfr{&tmptable}.sgrp WITH FRAME frm-Input.
       DISPLAY  bfr{&tmptable}.Ledger bfr{&tmptable}.proj bfr{&tmptable}.dept Ades DDes PDes  WITH FRAME frm-input.
       IF AVAILABLE glmf AND glmf.dept <> 0 THEN
            DISABLE btndept bfr{&tmptable}.dept WITH FRAME frm-input.
        IF AVAILABLE glmf AND glmf.proj <> 0 THEN
            DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-input.
   END.
   FIND FIRST cbkmf WHERE cbkmf.bank = bfr{&tmptable}.acb NO-LOCK NO-ERROR.
       IF AVAILABLE cbkmf THEN
          DISPLAY bfr{&tmptable}.acb cbkmf.descrip  WITH FRAME frm-input.
   IF bfr{&tmptable}.taxID <> 0 THEN DO:
            FIND FIRST fdmsTax WHERE fdmsTax.taxID = bfr{&tmptable}.taxID NO-LOCK NO-ERROR.
                IF AVAILABLE fdmsTax  THEN
                    DISPLAY fdmsTax.taxName WITH FRAME frm-input.
   END.
   
   DISPLAY wsid @ {&skey} {&UpdFields} 
       WITH FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE Validate-ip:
    IF wsid = "0" THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
          APPLY 'back-tab' TO {&skey} IN FRAME frm-Input.
       END.
    END.
END.

procedure Search.ip.
    hCol = browse brw-{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column".
       IF trim(hCol:label)<> "Description" AND trim(hCol:label)<> "Receipting Code" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
    case trim(hCol:label):
      when "Description" then
         do:
          OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.Descrip.

            END.
            when "Receipting Code" then
         do:
          OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.rCode >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.rCode.

            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END.
    RETURN.
END.



