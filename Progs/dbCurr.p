
/*Program:...........dbCurr.p
   Notes:...............Create ISO Currency
   Programmer:................S. Chisoro */
 session:DATA-ENTRY-RETURN = TRUE.

 &SCOPED-DEFINE wsTitle           "ISO Currency File Maintenance"
 &SCOPED-DEFINE tmptable            tblCurrency
&SCOPED-DEFINE skey               tblCurrency.code
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.CODE ~
                                        COLUMN-LABEL '  CURRENCY CODE ':C
&SCOPED-DEFINE tmpFields   bfr{&tmptable}.CODE ~
                                        COLUMN-LABEL ' CURRENCY CODE ' FORMAT 'X(15)':C~
                                         bfr{&tmptable}.Num ~
                                        COLUMN-LABEL ' ISO CODE ':C~
                                         bfr{&tmptable}.Name~
                                         COLUMN-LABEL ' CURRENCY NAME ' FORMAT 'X(40)':C


define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 80 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 80 by 8.4.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 80 by 18.4.



{varlib.i}

define frame frm-input
    SKIP(0.5)
    
    {&skey}   AT ROW 1.2 COL 5 LABEL "CURRENCY CODE"  SKIP(2)
    bfr{&tmptable}.NUM   COLON 20 LABEL "ISO CODE"  SKIP(2)
    bfr{&tmptable}.NAME COLON 20 LABEL "CURRENCY NAME"   FORMAT 'X(55)' SKIP(0.2) 
    
    rect-2 AT ROW 1 COL 2
    rect-1 AT ROW 9.5 COL 2
    btn-ok AT ROW 9.8 COL  15
    btn-close colon 60 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "ISO CURRENCY MASTER CONTROL FILE".
    
DEF NEW SHARED FRAME frm-main
     SKIP(0.5)
    brw-{&tmptable}  COLON 5
    btn-Add AT ROW 20.7 COL 5
    Space(15) btn-Edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-3 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.


/* *** Triggers to input frame frm-input*** */
ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   
    wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
     IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         RUN simip.p.
        wsBtn = "ADD".
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.CODE = wsid
                 bfr{&tmptable}.Name = bfr{&tmptable}.Name:SCREEN-VALUE
                 bfr{&tmptable}.NUM =  int(bfr{&tmptable}.NUM:SCREEN-VALUE)
                 bfr{&tmptable}.DeviceName = lc-host
                 bfr{&tmptable}.UID = varUser
                 bfr{&tmptable}.creDate =NOW.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.cStat = "A"  BY bfr{&tmptable}.CODE.
         HIDE FRAME frm-input.
         brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
    ELSE DO:
        wsBtn = "EDIT".
        ASSIGN 
                 bfr{&tmptable}.Name = bfr{&tmptable}.Name:SCREEN-VALUE
                 bfr{&tmptable}.NUM =  int(bfr{&tmptable}.NUM:SCREEN-VALUE).
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.cStat = "A"  BY bfr{&tmptable}.CODE.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
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
    brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
   GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.CODE.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.CODE = wsid EXCLUSIVE-LOCK NO-ERROR.
    wsBtn = "EDIT".
      ASSIGN bfr{&tmptable}.cStat = "D".
       RELEASE {&tmptable}.
       OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.cStat = "A"  BY bfr{&tmptable}.CODE.
     APPLY 'entry' TO btn-exit.
END.


ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.




{audit.i}
/********** MAIN LOGIC **********/

OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.cStat = "A"  BY bfr{&tmptable}.CODE.
ENABLE ALL EXCEPT  WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.


PROCEDURE proc-input:
  CLEAR FRAME frm-input ALL.
   ENABLE ALL  WITH FRAME frm-input.
  ASSIGN {&skey}:SCREEN-VALUE = "".
  WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
      OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.


PROCEDURE proc-Edit:
   ASSIGN wsid = bfr{&tmptable}.CODE.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.CODE = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} 
        bfr{&tmptable}.Name bfr{&tmptable}.num
       WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.




