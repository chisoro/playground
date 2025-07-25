
/* Program.................simFox.p
   Notes:................. Currency /Addupdate
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "This Currency already exist"
&SCOPED-DEFINE wsTitle           "Foreign Currecny File Maintenance"
&SCOPED-DEFINE tmptable             tblForex
&SCOPED-DEFINE skey                 tblForex.txtCur
DEF SHARED VAR varUser LIKE simusr.UID.
DEF VAR hCol           AS WIDGET-HANDLE.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsSearch       AS CHAR FORM "x(40)".
DEF VAR wsid           LIKE {&skey}.
DEF VAR wsCur LIKE tblForex.txtCur.

DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-Del    LABEL "DEL".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btnSearch LABEL "Search".

DEF TEMP-TABLE tmptable1
    FIELD dtRate LIKE tblForexH.dtRate
    FIELD txtCur LIKE tblForexH.txtCur
    FIELD decRate LIKE tblForexH.decRate.

DEF BUFFER bfr{&tmptable} FOR {&tmptable} PRESELECT.

DEF QUERY qry-{&tmptable} FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
    DISPLAY bfr{&tmptable}.dtRate LABEL "DATE" bfr{&tmptable}.txtCur WIDTH 8 bfr{&tmptable}.decRate /*FORM "zzz,zz9.9999-" */ 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF QUERY qry-tmptable1 FOR tmptable1 SCROLLING.
DEF BROWSE brw-tmptable1 QUERY qry-tmptable1
    DISPLAY tmptable1.dtRate LABEL "DATE" 
            tmptable1.txtCur LABEL "CURRENCY" WIDTH 8 
            tmptable1.decRate LABEL "RATE" /*FORM "zzz,zz9.9999-" */
    WITH 20 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

browse brw-{&tmptable}:allow-column-searching = true.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 18.5.

DEF FRAME frm-main
    {&skey}                 AT ROW 2.0 COL 5 label "CURRENCY"
    bfr{&tmptable}.dtRate   AT ROW 3.5 COL 4 label "RATE DATE"
    bfr{&tmptable}.decRate  AT ROW 5.0 COL 5 label "RATE"
    
    brw-{&tmptable} AT ROW 1.8 COL 27
    "HISTORICAL" AT ROW 1.8 COL 62
    brw-tmptable1 AT ROW 3.3 COL 62
    btn-add AT ROW 20.7 COL 10
    space(54) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS KEEP-TAB-ORDER CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 95 BY 23.2
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.


/* ***** Triggers for the main frame **** */

ON 'mouse-select-dblclick' OF brw-{&tmptable}
    DO:
        btn-ADD:LABEL IN  FRAME frm-Main = "UPDATE".
        GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        wsId = bfr{&tmptable}.txtCur.
        current-record = ROWID(bfr{&tmptable}).
        run proc-hist.
        DISPLAY bfr{&tmptable}.txtCur @ {&skey} 
                bfr{&tmptable}.decRate 
                bfr{&tmptable}.dtRate WITH FRAME frm-main.
        APPLY 'entry' TO {&skey}.
        RETURN.
END.

ON 'choose':U OF btn-Add IN FRAME frm-Main 
DO:
    IF btn-Add:LABEL = "UPDATE" THEN DO:
        FIND FIRST bfr{&tmptable} WHERE  ROWID(bfr{&tmptable})= current-record EXCLUSIVE-LOCK NO-ERROR.
        IF CAN-FIND(FIRST tblForexH WHERE tblForexH.txtCur = {&skey}:SCREEN-VALUE
                       AND tblForexH.dtRate = date(bfr{&tmptable}.dtRate:SCREEN-VALUE)) THEN DO:
            MESSAGE "Date rate has already been captured" VIEW-AS ALERT-BOX.
            RETURN.
        END.
        ELSE DO:
            IF bfr{&tmptable}.dtRate <>  DATE(bfr{&tmptable}.dtRate:SCREEN-VALUE) THEN DO:
                /*take new input to history */
                IF NOT CAN-FIND(FIRST tblForexH WHERE tblForexH.txtCur = {&skey} :SCREEN-VALUE
                           /* AND tblForexH.decRate = DEC(bfr{&tmptable}.decRate:SCREEN-VALUE) */ 
                            AND tblForexH.dtRate = date(bfr{&tmptable}.dtRate:SCREEN-VALUE)) THEN DO:
                    CREATE tblforexH.
                    ASSIGN tblForexH.txtCur = {&skey}:SCREEN-VALUE
                           tblForexH.decRate = DEC(bfr{&tmptable}.decRate:SCREEN-VALUE) 
                           tblForexH.dtRate = date(bfr{&tmptable}.dtRate:SCREEN-VALUE).
                    ASSIGN bfr{&tmptable}.txtCur = {&skey}:SCREEN-VALUE WHEN bfr{&tmptable}.txtCur <> {&skey}:SCREEN-VALUE
                               bfr{&tmptable}.decRate = DEC(bfr{&tmptable}.decRate:SCREEN-VALUE)
                                                            WHEN dec(bfr{&tmptable}.decRate:SCREEN-VALUE) <> bfr{&tmptable}.decRate
                               bfr{&tmptable}.DtRate = DATE(bfr{&tmptable}.dtRate:SCREEN-VALUE) WHEN dec(bfr{&tmptable}.decRate:SCREEN-VALUE) <> bfr{&tmptable}.decRate
                               bfr{&tmptable}.UID = varUser.
                    RELEASE {&tmptable}.
                END.
            END.
            IF bfr{&tmptable}.dtRate <=  DATE(bfr{&tmptable}.dtRate:SCREEN-VALUE) THEN DO:
                IF NOT CAN-FIND(FIRST tblForexH WHERE tblForexH.txtCur = bfr{&tmptable}.txtCur
                            /*AND tblForexH.decRate = bfr{&tmptable}.decRate  */
                            AND tblForexH.dtRate = bfr{&tmptable}.dtRate) THEN DO:
                    CREATE tblforexH. /* create history */
                    BUFFER-COPY bfr{&tmptable} TO tblForexH.
                    ASSIGN bfr{&tmptable}.txtCur = {&skey}:SCREEN-VALUE WHEN bfr{&tmptable}.txtCur <> {&skey}:SCREEN-VALUE
                           bfr{&tmptable}.decRate = DEC(bfr{&tmptable}.decRate:SCREEN-VALUE)
                                                        WHEN dec(bfr{&tmptable}.decRate:SCREEN-VALUE) <> bfr{&tmptable}.decRate
                           bfr{&tmptable}.DtRate = DATE(bfr{&tmptable}.dtRate:SCREEN-VALUE) WHEN dec(bfr{&tmptable}.decRate:SCREEN-VALUE) <> bfr{&tmptable}.decRate
                           bfr{&tmptable}.UID = varUser.
                    RELEASE {&tmptable}.
                 END.
            END.
        END.
        run proc-hist.
        FIND LAST tblforexh WHERE tblforexh.txtCur = {&skey}:SCREEN-VALUE NO-ERROR.
       ASSIGN bfr{&tmptable}.decRate = tblforexh.decrate
              bfr{&tmptable}.dtrate  = tblforexh.dtrate.
        CLEAR FRAME frm-Main ALL.
        DISPLAY bfr{&tmptable}.txtCur bfr{&tmptable}.decRate WITH BROWSE brw-{&tmptable}.
        btn-Add:LABEL = "ADD".
        APPLY 'TAB' TO SELF.
        APPLY 'entry' TO {&skey}.
    END.
    ELSE 
        APPLY 'entry' TO {&skey}.
END.

ON 'enter':U OF {&skey} IN FRAME frm-main
    OR 'tab':U OF {&skey} IN FRAME frm-main
DO: 
     wsid = {&skey}:SCREEN-VALUE IN FRAME frm-main.
   IF wsid = "" THEN DO:
        CLEAR FRAME frm-main ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-main.
    END.
    ELSE DO:
        FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
        IF AVAILABLE {&tmptable} AND btn-Add:LABEL = "ADD" THEN DO:
           MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           CLEAR FRAME frm-main ALL.
           RETURN NO-APPLY.
        END.
        ELSE IF AVAILABLE {&tmptable} AND btn-Add:LABEL = "UPDATE" 
                  AND current-record <> ROWID(bfr{&tmptable}) THEN DO:
           MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           CLEAR FRAME frm-main ALL.
           RETURN NO-APPLY.
        END.
    END.
END.

ON 'enter' OF bfr{&tmptable}.decRate IN FRAME frm-Main
DO:
  IF (btn-Add:LABEL IN  FRAME frm-Main) = "ADD" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.txtCur  = wsid NO-ERROR.
     IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Grade already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
     END.
     ELSE DO:
          CREATE bfr{&tmptable}.
          ASSIGN bfr{&tmptable}.txtCur = wsid
                 bfr{&tmptable}.dtRate = DATE(bfr{&tmptable}.dtRate:SCREEN-VALUE)
                 /*bfr{&tmptable}.decRate = DEC(bfr{&tmptable}.decRate:SCREEN-VALUE IN FRAME frm-Main) */
                 bfr{&tmptable}.UID = varUser
                 bfr{&tmptable}.decRate.
          CLEAR FRAME frm-Main ALL.
          OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
          APPLY 'TAB' TO BROWSE brw-{&tmptable}.
          APPLY 'entry' TO {&skey}.
        END.
     END.
     ELSE DO:
         APPLY 'TAB' TO BROWSE brw-{&tmptable}.
         APPLY 'entry' TO btn-Add IN FRAME frm-Main.
     END.
END.

ON 'enter':U OF bfr{&tmptable}.dtRate IN FRAME frm-Main 
DO:
    APPLY 'entry' TO btn-add IN FRAME frm-Main. 
    RETURN.
END.
/*ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.txtCur.
    FIND FIRST egData WHERE egData.txtCur  = bfr{&tmptable}.txtCur NO-ERROR.
    IF AVAILABLE egData THEN DO:
        MESSAGE "Grade has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE egData THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    CLEAR FRAME frm-main ALL.
    APPLY 'entry' TO btn-exit.
END. */

ON 'start-search':u OF BROWSE brw-{&tmptable}
    run SEARCH.ip.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
CLOSE QUERY qry-tmptable1.
HIDE FRAME frm-main.

PROCEDURE proc-hist:
   ASSIGN wsCur = bfr{&tmptable}.txtCur.
   FOR EACH tmptable1:
       DELETE tmptable1.
   END.
   FOR EACH tblForexH WHERE wsCur = tblForexH.txtCur  BY tblForexH.dtRate DESC:
        CREATE tmptable1.
        ASSIGN
            tmptable1.dtRate = tblForexH.dtRate
            tmptable1.txtCur = tblForexH.txtCur 
            tmptable1.decRate = tblForexH.decRate.
    END. 
    OPEN QUERY qry-tmptable1 FOR EACH tmptable1 NO-LOCK .
   VIEW FRAME frm-main.
 END.

