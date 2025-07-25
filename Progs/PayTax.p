session:DATA-ENTRY-RETURN = TRUE.
/* Program.................PayTax.p
   Notes:...... Payroll item codes capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg          "Tax Table Range already exist"
&SCOPED-DEFINE wsTitle        "Tax table Maintenance"
&SCOPED-DEFINE skey          payTax.Taxfrom
&SCOPED-DEFINE tmptable       PayTax
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Taxfrom ~
                                    COLUMN-LABEL ' FROM '~
                              bfr{&tmptable}.Taxto ~
                                        COLUMN-LABEL ' TO ' ~
                              bfr{&tmptable}.Tax% ~
                                        COLUMN-LABEL ' TAX% ' ~
                               bfr{&tmptable}.Deduct ~
                                        COLUMN-LABEL ' DUDUCT AMOUNT '
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Taxfrom ~
                                    COLUMN-LABEL ' FROM '~
                              bfr{&tmptable}.Taxto ~
                                        COLUMN-LABEL ' TO ' ~
                              bfr{&tmptable}.Tax% ~
                                        COLUMN-LABEL ' TAX% ' ~
                               bfr{&tmptable}.Deduct ~
                                        COLUMN-LABEL ' DEDUCT AMOUNT '
                              
{varlib.i}                             .
DEF VAR wsTaxTo LIKE PayTax.Taxto.

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.
DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 15
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
     bfr{&tmptable}.TaxFrom   COLON 20 LABEL "FROM" SKIP(0.5)
     bfr{&tmptable}.TaxTo     COLON 20 LABEL "TO" SKIP(0.5)
     bfr{&tmptable}.Tax%      COLON 20 LABEL "TAX %"  SKIP(0.5)        
     bfr{&tmptable}.Deduct    COLON 20 LABEL "DEDUC AMOUNT" SKIP(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYE TAX TABLE MAINTENANCE".
    
/* ***** Triggers for the main frame **** */

ON 'enter':U OF bfr{&tmptable}.TaxTo
    OR 'tab':U OF bfr{&tmptable}.TaxTo
DO:
    IF DEC(bfr{&tmptable}.TaxTo:SCREEN-VALUE) < DEC(bfr{&tmptable}.Taxfrom:SCREEN-VALUE) THEN DO:
        MESSAGE "Upper Range cannot be smaller than Lower Range" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    wsTaxTo = DEC(bfr{&tmptable}.TaxTo:SCREEN-VALUE). 
    APPLY 'tab' TO SELF.
END.

ON 'enter':U OF bfr{&tmptable}.Taxfrom
    OR 'tab':U OF bfr{&tmptable}.Taxfrom

DO:
    IF DEC(bfr{&tmptable}.Taxfrom:SCREEN-VALUE) < wsTaxto THEN DO:
        MESSAGE "Start Amount cannot be less than Previous Range" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.   
    APPLY 'tab' TO SELF.
END.

/* ***** Triggers for the main frame **** */
ON CHOOSE OF btn-Add IN FRAME frmMain DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw{&tmptable}
        OR 'mouse-select-dblclick' OF brw{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        run proc-edit.
        RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST PayTax WHERE PayTax.Taxfrom   = dec(bfr{&tmptable}.Taxfrom:SCREEN-VALUE)
                         AND PayTax.Taxto     = dec(bfr{&tmptable}.Taxto:SCREEN-VALUE)  NO-ERROR.
        IF AVAILABLE PayTax THEN DO:
           MESSAGE  "Tax range already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.Taxfrom   = dec(bfr{&tmptable}.Taxfrom:SCREEN-VALUE)
                   bfr{&tmptable}.Taxto     = dec(bfr{&tmptable}.Taxto:SCREEN-VALUE)
                   bfr{&tmptable}.Tax%      = dec(bfr{&tmptable}.Tax%:SCREEN-VALUE)
                   bfr{&tmptable}.Deduct    = dec(bfr{&tmptable}.Deduct:SCREEN-VALUE).
            wsTaxto = dec(bfr{&tmptable}.Taxto:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO bfr{&tmptable}.Taxfrom IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Tax%      = dec(bfr{&tmptable}.Tax%:SCREEN-VALUE)
                bfr{&tmptable}.Deduct    = dec(bfr{&tmptable}.Deduct:SCREEN-VALUE)
                bfr{&tmptable}.taxfrom   = dec(bfr{&tmptable}.taxfrom:screen-value)
                bfr{&tmptable}.taxto     = DEC( bfr{&tmptable}.taxto:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DELETE bfr{&tmptable}.
     Method-Status = brw{&tmptable}:DELETE-SELECTED-ROWS().
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
FOR EACH bfr{&tmptable}.
    wstaxTo = bfr{&tmptable}.Taxto. 
END.
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK BY bfr{&tmptable}.TaxFrom .
ENABLE ALL WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE proc-input:
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
   /* GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Itemcode  = wsid EXCLUSIVE-LOCK NO-ERROR.
    IF bfr{&tmptable}.Linked <> 0 THEN DO:
        FIND FIRST PayTax WHERE PayTax.ItemCode = bfr{&tmptable}.Linked NO-LOCK NO-ERROR.
        wsDesc = PayTax.Descrip.
    END. */
    DISPLAY bfr{&tmptable}.taxfrom bfr{&tmptable}.taxto bfr{&tmptable}.Deduct bfr{&tmptable}.tax%  WITH FRAME frm-input.
    ENABLE bfr{&tmptable}.taxfrom bfr{&tmptable}.taxto bfr{&tmptable}.Deduct bfr{&tmptable}.tax% btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

     

