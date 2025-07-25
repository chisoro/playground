session:DATA-ENTRY-RETURN = TRUE.
/* Program.................crdmf.p
   Notes:..................Supplier master file
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Supplier record already exist"
&SCOPED-DEFINE wsTitle           "Supplier Master File Maintenance"
&SCOPED-DEFINE tmptable           crdmf
&SCOPED-DEFINE skey               crdmf.Acc
&SCOPED-DEFINE UpdFields    bfr{&tmptable}.Name ~
                              bfr{&tmptable}.discount~
                              bfr{&tmptable}.Contact~
                              bfr{&tmptable}.Add1~
                              bfr{&tmptable}.Add2~
                              bfr{&tmptable}.town~
                              bfr{&tmptable}.Phone~
                              bfr{&tmptable}.Cell~
                              bfr{&tmptable}.RegNo~
                              bfr{&tmptable}.terms~
                              bfr{&tmptable}.email~
                              bfr{&tmptable}.VatNo~
                              

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Acc ~
                                   COLUMN-LABEL 'ACCOUNT ':C ~
                              bfr{&tmptable}.Name ~
                                   COLUMN-LABEL ' SUPPLIER ' WIDTH 40~
                              bfr{&tmptable}.contact~
                                  COLUMN-LABEL 'CONTACT '~
                              bfr{&tmptable}.CELL~
                                  COLUMN-LABEL 'CELL '~
                              bfr{&tmptable}.phone ~
                                  COLUMN-LABEL 'PHONE ' ~
 /**/                            
{varlib.i}
DEF VAR X AS INT.
DEF VAR varUser LIKE simusr.usercode.

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 128 by 2.3.
DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 128 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 20
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 135 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input SKIP(1)
        {&skey}  AT ROW 2 COL 6 LABEL "Supplier Account" 
        bfr{&tmptable}.NAME    LABEL "Supplier " SKIP(0.5)
        bfr{&tmptable}.VatNo   COLON 20 LABEL "VAT Registration" 
        bfr{&tmptable}.RegNo            LABEL "Company Registration Number" SKIP(0.5)
        bfr{&tmptable}.Add1    COLON 20 FORM "X(40)" LABEL "Address" SKIP(0.2)
        bfr{&tmptable}.Add2    COLON 20 FORM "X(40)" NO-LABEL  SKIP(0.2)
        bfr{&tmptable}.Town    COLON 20 FORM "X(40)" LABEL  "Town" SKIP(0.5)
        bfr{&tmptable}.Contact COLON 20  LABEL "Contact Person" SKIP(0.2)
        bfr{&tmptable}.cell    COLON 20 LABEL "Cell" SKIP(0.2) 
        bfr{&tmptable}.phone   COLON 20 LABEL "Phone" SKIP(0.2) 
        bfr{&tmptable}.email   COLON 20 LABEL "eMail" SKIP(1.5) 
        bfr{&tmptable}.Terms   COLON 20 LABEL "Terms" SKIP(0.5)
        bfr{&tmptable}.Discount COLON 20 LABEL "Discount %" 
        SKIP(1.5)
    btn-ok colon 20
    btn-close colon 60 SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "SUPPLIER MASTER RECORD MAINTENANCE".


/* Triggers for the frmMain frame */
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
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
        RETURN.
END.

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
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

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST Crdmf WHERE Crdmf.Acc  = wsid NO-ERROR.
        IF AVAILABLE Crdmf THEN DO:
           MESSAGE {&wsMsg} VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.Acc      = wsid
                   bfr{&tmptable}.name     = bfr{&tmptable}.name:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.vatNo    = bfr{&tmptable}.VatNo:SCREEN-VALUE
                   bfr{&tmptable}.Contact  = bfr{&tmptable}.Contact:SCREEN-VALUE
                   bfr{&tmptable}.Add1     = bfr{&tmptable}.Add1:SCREEN-VALUE
                   bfr{&tmptable}.Add2     = bfr{&tmptable}.Add2:SCREEN-VALUE 
                   bfr{&tmptable}.Town     = bfr{&tmptable}.Town:SCREEN-VALUE 
                   bfr{&tmptable}.Phone    = INT( bfr{&tmptable}.Phone:SCREEN-VALUE)
                   bfr{&tmptable}.Cell     = INT(bfr{&tmptable}.cell:SCREEN-VALUE)
                   bfr{&tmptable}.email    = bfr{&tmptable}.email:SCREEN-VALUE
                   bfr{&tmptable}.discount = dec(bfr{&tmptable}.discount:SCREEN-VALUE)
                   bfr{&tmptable}.Terms    = INT(bfr{&tmptable}.Terms:SCREEN-VALUE)
                   bfr{&tmptable}.Stat      = 0
                   bfr{&tmptable}.RegNo  = bfr{&tmptable}.RegNo:SCREEN-VALUE.                               
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
  END.
  ELSE DO:
       ASSIGN  bfr{&tmptable}.name     = bfr{&tmptable}.name:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.vatNo    = bfr{&tmptable}.VatNo:SCREEN-VALUE
               bfr{&tmptable}.Contact  = bfr{&tmptable}.Contact:SCREEN-VALUE
               bfr{&tmptable}.Add1     = bfr{&tmptable}.Add1:SCREEN-VALUE
               bfr{&tmptable}.Add2     = bfr{&tmptable}.Add2:SCREEN-VALUE 
               bfr{&tmptable}.Town     = bfr{&tmptable}.Town:SCREEN-VALUE 
               bfr{&tmptable}.Phone    = INT( bfr{&tmptable}.Phone:SCREEN-VALUE)
               bfr{&tmptable}.Cell     = INT(bfr{&tmptable}.cell:SCREEN-VALUE)
               bfr{&tmptable}.email    = bfr{&tmptable}.email:SCREEN-VALUE
               bfr{&tmptable}.discount = dec(bfr{&tmptable}.discount:SCREEN-VALUE)
               bfr{&tmptable}.Terms    = INT(bfr{&tmptable}.Terms:SCREEN-VALUE)
               bfr{&tmptable}.Stat      = 0
               bfr{&tmptable}.RegNo  = bfr{&tmptable}.RegNo:SCREEN-VALUE.
      RELEASE {&tmptable}.
      APPLY 'close' TO SELF.
      HIDE FRAME frm-input.
      brw{&tmptable}:REFRESH() IN FRAME frmMain.
  END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Acc.
    FIND FIRST crdtmf WHERE crdtmf.Acc  = bfr{&tmptable}.Acc NO-ERROR.
    IF AVAILABLE crdtmf THEN DO:
        MESSAGE "Supplier has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE crdtmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frmMain.
ENABLE ALL WITH FRAME frmMain.
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.Acc).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Acc  = wsid EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} WITH FRAME frm-input.
    DISABLE ALL WITH FRAME frm-input.
    ENABLE  ALL EXCEPT {&skey} WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

     


