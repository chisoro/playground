/* Program.................hse024.p
   Notes:...... Update stand Purcahse Price
   Author:.................S. Mawire/S. Chisoro
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
                              bfr{&tmptable}.VAT ~
                                        COLUMN-LABEL ' VAT ':C ~

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
     size 100 by 2.3.

define RECTANGLE rec-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 100 by 18.5.

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
     "------------------PROPERTY VALUE DETAILS ------------------" COLON 10 SKIP
    btn-scheme         COLON 10  NO-LABEL  bfr{&tmptable}.scheme VIEW-AS TEXT hsesch.Descrip NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.standNo      COLON 20 LABEL "Stand Number" VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.sitevalue     COLON 20 LABEL " Site Value" SKIP(.2)
    bfr{&tmptable}.PPrice COLON 20 LABEL "Purchase Price" SKIP(.2)
    bfr{&tmptable}.Vat    COLON 20 LABEL "VAT %"  VIEW-AS TEXT skip(.2)
    btn-ok  AT ROW 13.6  COL 10
    btn-close AT ROW 13.6  COL 70
    rec-3 AT ROW 1.4 COL 3
    
    rec-6 AT ROW 13  COL 3
    with view-as dialog-box keep-tab-order no-validate
    side-labels no-underline three-d SCROLLABLE SIZE 93 BY 17
     TITLE "STAND VALUE MAINTENANCE".
    
DEF FRAME frm-bfr{&tmptable}
    brw-{&tmptable} AT ROW 2 COL 6 
    btn-edit AT ROW 20.7 COL 20
    Space(40) btn-exit SKIP(1)
    rec-2 AT ROW 1.4 COL 3
    rec-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
     NO-LABEL no-underline three-d SCROLLABLE SIZE 105 BY 24 CENTERED
    TITLE "STAND VALUE MAINTENANCE".


ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO bfr{&tmptable}.scheme IN FRAME frm-input.
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
  APPLY 'tab' TO  bfr{&tmptable}.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickScheme 
    OR 'enter':u OF brw-PickScheme
    OR 'mouse-select-dblclick' OF brw-PickScheme
DO: 
   GET CURRENT qry-PickScheme NO-LOCK NO-WAIT.
   DISPLAY hsesch.Scheme @ bfr{&tmptable}.Scheme hsesch.descrip WITH FRAME frm-input.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO bfr{&tmptable}.Scheme.
  RETURN. 
END.

ON  'enter':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
     OR 'tab':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
DO:
   FIND FIRST hsesch WHERE hsesch.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsesch THEN DO:
      MESSAGE "Project does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE hsesch THEN DO:
        DISPLAY hsesch.descrip WITH FRAME frm-input.
   END.
   RETURN.    
END.

ON 'enter':U OF bfr{&tmptable}.StandNo  
    OR 'tab':U OF bfr{&tmptable}.StandNo 
    OR 'leave':U OF bfr{&tmptable}.StandNo
DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
         AND bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE
         NO-LOCK  NO-ERROR. 
       IF NOT AVAILABLE bfr{&tmptable} THEN
       DO:
          MESSAGE "Stand Number " bfr{&tmptable}.StandNo:SCREEN-VALUE " OF Project  "
               hsesch.Descrip " is not created"
              VIEW-AS ALERT-BOX.
          bfr{&tmptable}.standno:SCREEN-VALUE = "".
          APPLY "entry" TO SELF IN FRAME  frm-Input.
          RETURN NO-APPLY.
       END.
      DISPLAY bfr{&tmptable}.pprice bfr{&tmptable}.vat bfr{&tmptable}.sitevalue  WITH FRAME frm-input.
      IF accStat = 0 THEN DO:
          DISABLE bfr{&tmptable}.vat WITH FRAME frm-input.
      END.
      ELSE
          ENABLE ALL WITH FRAME frm-input.
     
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:  
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Update"  THEN DO:
        /*DO TRANSACTION: */
           /* FIND CURRENT bfr{&tmptable} EXCLUSIVE-LOCK. */
        wsbtn = "EDIT".
            IF CURRENT-CHANGED bfr{&tmptable} THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
              FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.scheme =  INT(bfr{&tmptable}.scheme:SCREEN-VALUE)AND bfr{&tmptable}.standNo = bfr{&tmptable}.standno:SCREEN-VALUE EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN  
                    bfr{&tmptable}.sitevalue    = DEC(bfr{&tmptable}.sitevalue:SCREEN-VALUE)
                    bfr{&tmptable}.pprice    = DEC(bfr{&tmptable}.pprice:SCREEN-VALUE).
                    
            END.
            CLEAR FRAME frm-input ALL.
           APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
    END.  
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    brw-{&tmptable}:REFRESH() IN FRAME frm-bfr{&tmptable}.
    RETURN.
END.

/* ***** Triggers for the main frame **** */
ON 'mouse-select-click' OF brw-{&tmptable}  
DO:
    ENABLE btn-edit WITH FRAME frm-bfr{&tmptable}.
    RETURN.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-bfr{&tmptable} 
    OR 'mouse-select-dblclick' OF brw-{&tmptable} 
DO: 
    btn-ok:LABEL IN FRAME frm-input = "UPDATE".
     wsbtn = "EDIT".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.dbacc.
   RUN proc-edit.
   HIDE FRAME frm-input.
    brw-{&tmptable}:REFRESH() IN FRAME frm-bfr{&tmptable}.
    RETURN.
END.



 on 'start-search':u of browse brw-{&tmptable}
    run SEARCH.ip.

{audit.i}
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-{&tmptable}:allow-column-searching = true.
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.accstat <> 0 NO-LOCK .
ENABLE ALL EXCEPT BTN-EDIT WITH FRAME Frm-bfr{&tmptable}.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-bfr{&tmptable}.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-bfr{&tmptable}.

/******* INTERNAL PROCEDURES *********/


PROCEDURE proc-Edit:
   CLEAR FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO bfr{&tmptable}.SiteValue IN FRAME frm-input.
   FIND FIRST hsesch WHERE hsesch.scheme = bfr{&tmptable}.scheme NO-LOCK NO-ERROR.
   DISPLAY bfr{&tmptable}.Scheme bfr{&tmptable}.standNo bfr{&tmptable}.sitevalue bfr{&tmptable}.PPrice bfr{&tmptable}.Vat  WITH FRAME frm-input.
   DISPLAY hsesch.Descrip WITH FRAME frm-input.  
   WAIT-FOR CHOOSE OF  btn-ok OR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   RETURN.
END PROCEDURE. 

PROCEDURE SEARCH.ip.
    hCol = BROWSE brw-{&tmptable}:CURRENT-COLUMN.
        VIEW FRAME Search-opt.
        ENABLE ALL WITH FRAME Search-opt.
        WAIT-FOR 'choose' OF btnSearch OR 'tab' of btnSearch OR 'enter' OF btnSearch 
            OR 'window-close' OF FRAME Search-opt OR CLOSE OF THIS-PROCEDURE OR
                'esc','f4' OF FRAME Search-opt.
        HIDE FRAME Search-opt.
        CASE trim(hCol:label):
            when "Name" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.sortname >= wsSearch:SCREEN-VALUE USE-INDEX sortname.

            END.
            when "Account" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where  bfr{&tmpTable}.accstat <> 0 AND bfr{&tmptable}.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbAcc.

            END.
            when "StandNo" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where  bfr{&tmpTable}.accstat <> 0 AND bfr{&tmptable}.StandNo >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.StandNo.

            END.
        END.
        RETURN.
END PROCEDURE.


    

