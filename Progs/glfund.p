session:DATA-ENTRY-RETURN = TRUE.
/* Program.................glfund.p
   Notes:...... Debit raising module
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Fund file already exist"
&SCOPED-DEFINE wsTitle           "Fund File Maintenance"
&SCOPED-DEFINE tmptable             glfund
&SCOPED-DEFINE skey                 glfund.fund
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.fund ~
                                        COLUMN-LABEL ' Fund ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        WIDTH 60 COLUMN-LABEL ' Description ':C
                              

DEF VAR X AS INT.
DEF VAR ws AS CHAR EXTENT 7.
{varlibrary.i}
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 18.5.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 8
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
    {&skey}                 COLON 30 LABEL "Fund" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 30 LABEL "Description"
    SKIP(0.5)
    btn-ok colon 5
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
/* ***** Triggers for the main frame **** */
{trilib.i} 


ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST glfund WHERE glfund.fund  = wsid NO-ERROR.
        IF AVAILABLE glfund THEN DO:
           MESSAGE  "Fund already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.fund = wsid
                 bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
           /* RUN Contr-ledger.ip. */
            OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.fund.
    IF CAN-FIND(FIRST gldept WHERE gldept.fund  = bfr{&tmptable}.fund) THEN DO:
        MESSAGE "Fund has related Departments - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF CAN-FIND (FIRST glmf WHERE glmf.fund  = bfr{&tmptable}.fund) THEN DO:
        MESSAGE "Fund has related Ledgers - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE glmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
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
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.fund).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.fund  = wsid EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip  WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

 PROCEDURE Contr-ledger.ip:
     FIND FIRST simctr NO-LOCK NO-ERROR.
     ASSIGN ws[1] = STRING(wsid) + "00" + STRING(SIMCTR.SURACC)
            ws[2] = STRING(wsid) + "00" + STRING(simctr.vat[1])
            ws[3] = STRING(wsid) + "00" + STRING(simctr.vat[2])
            ws[4] = STRING(wsid) + "00" + STRING(simctr.vat[3])
            ws[5] = STRING(wsid) + "00" + STRING(simctr.vat[4])
            ws[6] = STRING(wsid) + "00" + STRING(SIMCTR.fundDb)
            ws[7] = STRING(wsid) + "00" + STRING(SIMCTR.fundCr).
     DO X = 1 TO 7:
         FIND FIRST glmf WHERE glmf.acct = INT(ws[X]) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE glmf THEN DO:
             CREATE glmf.
             ASSIGN glmf.acct    = INT(ws[X])
                    glmf.ACCTYPE = "BS"
                    glmf.VOTE      = 2
                    glmf.FUND      = wsid
                    glmf.CREDATE = TODAY
                    glmf.ITEM      = 0.
             IF  LENGTH(ws[X]) = 9 THEN
                 ASSIGN glmf.CAT     = INT(SUBSTR(ws[X],4,2)) 
                        glmf.subcat    = INT(SUBSTR(ws[X],5,2)).
             ELSE 
                  ASSIGN glmf.CAT     = INT(SUBSTR(ws[X],5,2)) 
                        glmf.subcat    = INT(SUBSTR(ws[X],6,2)).
             IF (X = 1 OR X = 2 OR X = 3 OR X = 7 ) THEN
                  glmf.DrCr      = 1.
              ELSE glmf.DrCr      = 2.
              IF X = 1 THEN 
                  glmf.DESCRIPTION = "SURPLUS/DEFICIT - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                ELSE IF X = 2 THEN 
                  glmf.DESCRIPTION = "DEBTORS VAT PROVISION - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                 ELSE IF X = 3 THEN 
                   glmf.DESCRIPTION = "DEBTORS VAT PAYABLE - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                  ELSE IF X = 4 THEN 
                    glmf.DESCRIPTION = "CREDITORS VAT PROVISION - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                   ELSE IF X = 5 THEN 
                     glmf.DESCRIPTION = "CREDITORS VAT RECEIVABLE - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                     ELSE IF X = 6 THEN 
                       glmf.DESCRIPTION = "INTERFUND DEBTORS - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                       ELSE IF X = 7 THEN 
                         glmf.DESCRIPTION = "INTERFUND CREDITORS - " 
                                     + bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input.
                  glmf.SUBVOTE   = glmf.subcat.
         END.
     END.
END.
     

