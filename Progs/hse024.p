/* Program.................hse024.p
   Notes:...... Stand Master record initial creation
   Author:.................S. Mawire/S. Chisoro
   to work on reposesing after finding the rules
*/
session:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE tmptable           hsecmf
&SCOPED-DEFINE skey               hsecmf.dbacc
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.scheme ~
                                        COLUMN-LABEL ' SCHEME ':C ~
                              bfr{&tmptable}.standNo ~
                                        COLUMN-LABEL ' STANDNO ':C ~
                              bfr{&tmptable}.dbacc ~
                                        COLUMN-LABEL ' ACCOUNT ':C ~
                              bfr{&tmptable}.Suburb ~
                                        COLUMN-LABEL 'SURBUB ':C ~
                              bfr{&tmptable}.sitevalue ~
                                       COLUMN-LABEL ' SITE VALUE ':C ~
                              bfr{&tmptable}.PPrice ~
                                        COLUMN-LABEL ' PURCHASE PRICE ':C ~
                              bfr{&tmptable}.accstat ~
                                        COLUMN-LABEL ' STATUS ':C ~



{varlibrary.i}

DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR wsSource  AS CHAR INITIAL "HS".
DEF VAR wsdate    AS DATE.
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsn       AS INT.
DEF VAR wsr       AS   DEC FORM "zzz9.999999999-".
DEF VAR X AS INT.

DEF BUTTON btn-Ward   LABEL "Ward".
DEF BUTTON btn-Sub    LABEL "Suburb".
DEF BUTTON btn-Scheme LABEL "Scheme".

DEFINE QUERY qry-PickWard FOR dbwrd scrolling.
DEF BROWSE brw-PickWard QUERY qry-PickWard
    DISPLAY  dbwrd.Ward dbwrd.Descrip
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-PickSuburb FOR dbsmf scrolling.
DEF BROWSE brw-PickSuburb QUERY qry-PickSuburb
    DISPLAY  dbsmf.suburb dbsmf.Descrip
    WITH 8 DOWN SEPARATORS.

DEF    QUERY qry-pickScheme FOR hsesch SCROLLING.
DEF BROWSE brw-pickScheme QUERY qry-pickScheme
        DISPLAY hsesch.scheme hsesch.Descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickScheme 
        brw-pickScheme AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Scheme Selection".

define RECTANGLE rec-1
     edge-pixels 2 graphic-edge  no-fill
     size 105 by 2.3.

define RECTANGLE rec-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 105 by 18.5.

define RECTANGLE rec-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 87 by 10.5.
define RECTANGLE rec-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 75 by 19.5.

define RECTANGLE rec-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define RECTANGLE rec-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 87 by 2.3.


DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.


define frame frm-input
    SKIP (0.5)
     "------------------PROPERTY DETAILS ------------------" COLON 10 SKIP
    btn-Scheme          COLON 10 bfr{&tmpTable}.Scheme NO-LABEL hsesch.Descrip NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfr{&tmpTable}.standNo      COLON 20 LABEL "Stand Number" SKIP(.2)
    bfr{&tmpTable}.dbacc        colon 20 LABEL "Account" SKIP(.2)
    bfr{&tmpTable}.DeedNo       COLON 20    LABEL "Deed Number" SKIP(.2)
    bfr{&tmpTable}.SIZE         COLON 20 LABEL "Stand Size" SKIP(.2)
    btn-Sub            COLON 11 NO-TAB-STOP 
    bfr{&tmpTable}.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)
    btn-Ward           COLON 12.5 NO-TAB-STOP 
    bfr{&tmpTable}.Ward         NO-LABEL 
    dbwrd.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)

    btn-ok  AT ROW 13.6  COL 10
    btn-close AT ROW 13.6  COL 70
    rec-3 AT ROW 1.4 COL 3
    
    rec-6 AT ROW 13  COL 3
    with view-as dialog-box keep-tab-order no-validate
    side-labels no-underline three-d SCROLLABLE SIZE 93 BY 17
     TITLE "STAND MAINTENANCE".
    
DEF FRAME frm-Main
    brw-{&tmpTable} AT ROW 2 COL 6 SPACE (2) "STATUS KEY"
    "1 = Vacant" AT ROW 3 COL 92
    "2 = Reposed" AT ROW 4 COL 92
    btn-add AT ROW 20.7 COL 15
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rec-2 AT ROW 1.4 COL 3
    rec-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
     NO-LABEL no-underline three-d SCROLLABLE SIZE 110 BY 24 CENTERED
    TITLE "STAND MAINTENANCE".

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
    APPLY 'entry' TO bfr{&tmpTable}.scheme IN FRAME frm-input.
    RETURN NO-APPLY.
END.

ON CHOOSE OF btn-Scheme IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickScheme.
  OPEN QUERY qry-pickScheme FOR EACH hsesch NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickScheme.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickScheme 
          OR close of THIS-PROCEDURE IN FRAME frm-pickScheme 
          OR CHOOSE OF btn-ok IN FRAME frm-pickScheme 
          OR 'enter':u OF brw-pickScheme
          OR 'mouse-select-dblclick' OF brw-pickScheme.
  CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO  bfr{&tmpTable}.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickScheme 
    OR 'enter':u OF brw-PickScheme
    OR 'mouse-select-dblclick' OF brw-PickScheme
DO: 
   GET CURRENT qry-PickScheme NO-LOCK NO-WAIT.
   DISPLAY hsesch.Scheme @ bfr{&tmpTable}.Scheme hsesch.descrip WITH FRAME frm-input.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO bfr{&tmpTable}.Scheme.
  RETURN. 
END.

ON  'enter':u OF bfr{&tmpTable}.Scheme IN FRAME  frm-Input
     OR 'tab':u OF bfr{&tmpTable}.Scheme IN FRAME  frm-Input
DO:
   FIND FIRST hsesch WHERE hsesch.Scheme = INT(bfr{&tmpTable}.Scheme:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsesch THEN DO:
      MESSAGE "Project does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE hsesch THEN DO:
        DISPLAY hsesch.descrip WITH FRAME frm-input.
   END.
   RETURN.    
END.

ON 'enter':U OF bfr{&tmpTable}.StandNo  
    OR 'tab':U OF bfr{&tmpTable}.StandNo 
    OR 'leave':U OF bfr{&tmpTable}.StandNo
DO:
     FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.Scheme = INT(bfr{&tmpTable}.Scheme:SCREEN-VALUE)
         AND bfr{&tmpTable}.StandNo = bfr{&tmpTable}.StandNo:SCREEN-VALUE
         NO-LOCK  NO-ERROR. 
       IF AVAILABLE bfr{&tmpTable} THEN
       DO:
          MESSAGE "Stand Number " bfr{&tmpTable}.StandNo:SCREEN-VALUE " OF Project  "
               hsesch.Descrip " is already created"
              VIEW-AS ALERT-BOX.
          bfr{&tmpTable}.standno:SCREEN-VALUE = "".
          APPLY "entry" TO SELF IN FRAME  frm-Input.
          RETURN NO-APPLY.
       END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:  
    IF (btn-ok:LABEL IN  FRAME frm-input) = "UPDATE"  THEN DO:
        /*DO TRANSACTION: */
           /* FIND CURRENT bfr{&tmpTable} EXCLUSIVE-LOCK. */
            IF CURRENT-CHANGED bfr{&tmpTable} THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
              FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc =  DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN  
                    bfr{&tmpTable}.scheme    = INT(bfr{&tmpTable}.scheme:SCREEN-VALUE)
                    bfr{&tmpTable}.StandNo   = bfr{&tmpTable}.StandNo:SCREEN-VALUE
                    bfr{&tmpTable}.suburb    = int(bfr{&tmpTable}.suburb:SCREEN-VALUE)
                    bfr{&tmpTable}.ward      = int(bfr{&tmpTable}.ward:SCREEN-VALUE)
                    bfr{&tmpTable}.SIZE      = DEC(bfr{&tmpTable}.SIZE:SCREEN-VALUE)
                    bfr{&tmpTable}.DeedNo    = bfr{&tmpTable}.DeedNo:SCREEN-VALUE.
                    
            END.
            FIND CURRENT bfr{&tmpTable} NO-LOCK.                    
       /* END. *//* Do Transaction */
        HIDE FRAME frm-input.
        APPLY 'close' TO THIS-PROCEDURE.
        brw-{&tmpTable}:REFRESH() IN FRAME frm-Main.
        RETURN.
    END.
    ELSE DO:
        IF bfr{&tmpTable}.dbacc:SCREEN-VALUE = "" THEN DO:
            MESSAGE "BLANK not a valid Account...." VIEW-AS ALERT-BOX.
            APPLY 'entry' TO bfr{&tmpTable}.dbacc IN FRAME frm-input.
            RETURN NO-APPLY.
        END.
        ELSE IF  DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) <> 0 AND bfr{&tmpTable}.standno:SCREEN-VALUE <> string(0) THEN DO:
            FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE bfr{&tmpTable} THEN DO:
                MESSAGE "Stand and Account already exist.... no record saved" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                 wsbtn = "ADD".
                CREATE bfr{&tmpTable}.
                 ASSIGN bfr{&tmpTable}.dbacc     = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE)
                        bfr{&tmpTable}.scheme    = INT(bfr{&tmpTable}.scheme:SCREEN-VALUE)
                        bfr{&tmpTable}.StandNo   = bfr{&tmpTable}.StandNo:SCREEN-VALUE
                        bfr{&tmpTable}.suburb    = INT(bfr{&tmpTable}.suburb:SCREEN-VALUE)
                        bfr{&tmpTable}.ward      = INT(bfr{&tmpTable}.ward:SCREEN-VALUE)
                        bfr{&tmpTable}.SIZE      = DEC(bfr{&tmpTable}.SIZE:SCREEN-VALUE)
                        bfr{&tmpTable}.accStat   = 1
                        bfr{&tmptable}.DeviceName = lc-host
                        bfr{&tmptable}.Credate = TODAY 
                        bfr{&tmptable}.UID     = varuser.
                
            END.
            RELEASE bfr{&tmpTable}.
           CLEAR FRAME frm-input ALL.
           VIEW FRAME frm-input.
           APPLY 'entry' TO bfr{&tmpTable}.scheme.
        END. /* eof if dbacc <> 0 */
     END. /* eof ELSE */
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
    RETURN.
END.


ON LEAVE OF bfr{&tmpTable}.dbacc 
    OR 'enter':U OF bfr{&tmpTable}.dbAcc IN FRAME frm-input
DO:
   IF  DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Account cannot be zero" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   IF  DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) <> 0 THEN DO:
       FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE) NO-ERROR.
       IF AVAILABLE bfr{&tmpTable} AND btn-ok:LABEL = "SAVE" THEN DO:
          MESSAGE "Record already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
  END.
  RETURN.    
END.

/*...... Triggers for button on-form buttons ....*/

/*------------------ Ward Button -------------*/
ON CHOOSE OF btn-Ward IN FRAME frm-input
DO:
  VIEW FRAME frm-Ward.
  OPEN QUERY qry-Ward FOR EACH dbwrd NO-LOCK.
  ENABLE ALL EXCEPT WITH FRAME frm-Ward.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Ward 
          OR close of THIS-PROCEDURE IN FRAME frm-Ward
          OR CHOOSE OF btn-ok IN FRAME frm-Ward 
          OR 'enter':u OF brw-Ward
          OR 'mouse-select-dblclick' OF brw-Ward.
  CLOSE QUERY qry-Ward.
  HIDE FRAME frm-Ward.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO bfr{&tmpTable}.ward.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ward
    OR 'enter':u OF brw-Ward
    OR 'mouse-select-dblclick' OF brw-Ward
DO: 
   GET CURRENT qry-Ward EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbwrd.Descrip dbwrd.Ward @ bfr{&tmpTable}.ward  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.ward IN FRAME frm-input
    OR 'tab':U OF bfr{&tmpTable}.ward IN FRAME frm-input
DO:
    FIND FIRST dbwrd WHERE dbwrd.Ward = INT(bfr{&tmpTable}.ward:SCREEN-VALUE) NO-ERROR.
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
  ENABLE ALL EXCEPT WITH FRAME frm-Sub.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Sub 
          OR close of THIS-PROCEDURE IN FRAME frm-Sub
          OR CHOOSE OF btn-ok IN FRAME frm-Sub 
          OR 'enter':u OF brw-Sub
          OR 'mouse-select-dblclick' OF brw-Sub.
  CLOSE QUERY qry-Sub.
  HIDE FRAME frm-Sub.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmpTable}.suburb.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Sub
    OR 'enter':u OF brw-Sub
    OR 'mouse-select-dblclick' OF brw-Sub
DO: 
   GET CURRENT qry-Sub EXCLUSIVE-LOCK NO-WAIT.
        DISPLAY dbsmf.Descrip dbsmf.suburb @ bfr{&tmpTable}.suburb  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.suburb IN FRAME frm-input
    OR 'tab':U OF bfr{&tmpTable}.suburb IN FRAME frm-input
DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = INT(bfr{&tmpTable}.suburb:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbsmf THEN
        DISPLAY dbsmf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Suburb...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.


/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-Add IN FRAME frm-Main DO:
    btn-ok:LABEL IN  FRAME frm-input = "SAVE".
     wsbtn = "ADD".
    CLEAR FRAME frm-input ALL.
    VIEW FRAME frm-input.
    ENABLE ALL  WITH FRAME frm-input.
    APPLY 'entry' TO bfr{&tmpTable}.scheme.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
    OPEN QUERY qry-{&tmpTable} FOR EACH bfr{&tmpTable}  WHERE accStat <> 0 NO-LOCK.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-Main 
    OR 'mouse-select-dblclick' OF brw-{&tmpTable} 
DO: 
    btn-ok:LABEL IN FRAME frm-input = "UPDATE".
     wsbtn = "EDIT".
    GET CURRENT qry-{&tmpTable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmpTable}.dbacc.
    DISABLE bfr{&tmpTable}.dbacc WITH FRAME frm-input.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-Main 
DO:
    GET CURRENT qry-{&tmpTable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmpTable}.dbacc.
    FIND FIRST hsehtf WHERE hsehtf.dbacc  = wsid NO-LOCK NO-ERROR.
    IF AVAILABLE hsehtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE hsehtf THEN DO:
       DELETE bfr{&tmpTable}.
       Method-Status = brw-{&tmpTable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

 on 'start-search':u of browse brw-{&tmpTable}
    run SEARCH.ip.

{audit.i}
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-{&tmpTable}:allow-column-searching = true.
OPEN QUERY qry-{&tmpTable} FOR EACH bfr{&tmpTable}  WHERE bfr{&tmpTable}.accStat <> 0 NO-LOCK.
ENABLE ALL EXCEPT btn-Edit WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Main.
CLOSE QUERY qry-{&tmpTable}.
HIDE FRAME frm-Main.

/******* INTERNAL PROCEDURES *********/
PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO bfr{&tmpTable}.dbacc.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-{&tmpTable} FOR EACH bfr{&tmpTable}  WHERE accStat <> 0 NO-LOCK.
END PROCEDURE.

PROCEDURE proc-edit:
   CLEAR FRAME frm-input ALL.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   FIND FIRST dbwrd WHERE dbwrd.ward = bfr{&tmpTable}.ward NO-LOCK NO-ERROR.
   FIND FIRST dbsmf WHERE dbsmf.suburb = bfr{&tmpTable}.suburb NO-LOCK NO-ERROR.
   FIND FIRST hsesch WHERE hsesch.scheme = bfr{&tmpTable}.scheme NO-LOCK NO-ERROR.
   DISPLAY bfr{&tmpTable}.scheme bfr{&tmpTable}.standno  bfr{&tmpTable}.dbacc  bfr{&tmpTable}.deed  hsesch.Descrip
       bfr{&tmpTable}.SIZE bfr{&tmpTable}.ward bfr{&tmpTable}.suburb dbsmf.descrip dbwrd.descrip WITH FRAME frm-input.
   APPLY 'entry' TO bfr{&tmpTable}.scheme.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-{&tmpTable} FOR EACH bfr{&tmpTable}  WHERE accStat <> 0 NO-LOCK.
END PROCEDURE.

PROCEDURE SEARCH.ip.
    hCol = BROWSE brw-{&tmpTable}:CURRENT-COLUMN.
        VIEW FRAME Search-opt.
        ENABLE ALL WITH FRAME Search-opt.
        WAIT-FOR 'choose' OF btnSearch OR 'tab' of btnSearch OR 'enter' OF btnSearch 
            OR 'window-close' OF FRAME Search-opt OR CLOSE OF THIS-PROCEDURE OR
                'esc','f4' OF FRAME Search-opt.
        HIDE FRAME Search-opt.
        CASE trim(hCol:label):
            when "Name" then
            do:
               open query qry-{&tmpTable} preselect 
                        EACH bfr{&tmpTable} no-lock 
                            where bfr{&tmpTable}.sortname >= wsSearch:SCREEN-VALUE USE-INDEX sortname
                               BY bfr{&tmpTable}.NAME.

            END.
            when "Account" then
            do:
               open query qry-{&tmpTable} preselect 
                        EACH bfr{&tmpTable} no-lock 
                            where  bfr{&tmpTable}.accstat <> 0 AND bfr{&tmpTable}.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbacc.

            END.
            when "StandNo" then
            do:
               open query qry-{&tmpTable} preselect 
                        EACH bfr{&tmpTable} no-lock 
                            where  bfr{&tmpTable}.accstat <> 0 AND bfr{&tmpTable}.StandNo >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmpTable}.StandNo.

            END.
        END.
        RETURN.
END PROCEDURE.


    

