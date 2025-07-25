/* Program.................dbcmf01s.p
   Notes:...... Account Status maintenance
   Author:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF  SHARED VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR wsR       AS INT.
DEF VAR wsRd       AS DATE.

DEF VAR wst AS INT.
DEF VAR X AS INT.
DEF VAR wsSearch   LIKE dbcmf.NAME.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-exit   LABEL "Exit".
DEF BUFFER bfrdbcmf FOR dbcmf.
DEF BUTTON btn-Activate LABEL "Activate".
DEF BUTTON btn-Finalise LABEL "Finalise".
DEF BUTTON btn-AClose LABEL "Close Account".
DEF VAR wsChce AS INTEGER.



DEFINE QUERY qry-bfrdbcmf FOR bfrdbcmf scrolling.
DEF BROWSE brw-bfrdbcmf QUERY qry-bfrdbcmf
    DISPLAY bfrdbcmf.dbacc  bfrdbcmf.NAME bfrdbcmf.Regid LABEL "ID/Co. Registration"
    bfrdbcmf.StandNo bfrdbcmf.Suburb bfrdbcmf.POwner bfrdbcmf.AccBal bfrdbcmf.AccStat WIDTH 15
    WITH 20 DOWN SEPARATORS.



define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 130 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 130 by 18.5.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 102.5 by 19.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 71 by 19.5.

define rectangle rect-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define rectangle rect-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 174 by 2.3.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

 
DEF FRAME frm-bfrdbcmf
    brw-bfrdbcmf AT ROW 2 COL 5 
    "KEY" AT ROW 2 COL 118
    "0: Active" AT ROW 3 COL 118
    "95: Finalisation" FGCOLOR 13 AT ROW 4 COL 118
    "99: Closed" FGCOLOR 12 AT ROW 5 COL 118 
     btn-Activate AT ROW 20.7 COL 10
    SPACE(28) btn-Finalise
    space(28) btn-AClose
    space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 135 BY 24 CENTERED
    TITLE "RATEPAYER ACCOUNT STATUS MAINTENANCE". 

ON row-display OF  brw-bfrdbcmf DO:
   ASSIGN 
        bfrdbcmf.dbacc:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99   
        bfrdbcmf.NAME:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99  
        bfrdbcmf.Regid:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99 
        bfrdbcmf.StandNo:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99  
        bfrdbcmf.Suburb:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99  
        bfrdbcmf.POwner:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99  
        bfrdbcmf.AccBal:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99 
        bfrdbcmf.AccStat:FGCOLOR IN BROWSE brw-bfrdbcmf = 12 WHEN bfrdbcmf.AccStat = 99
       bfrdbcmf.dbacc:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95   
        bfrdbcmf.NAME:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95  
        bfrdbcmf.Regid:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95 
        bfrdbcmf.StandNo:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95  
        bfrdbcmf.Suburb:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95  
        bfrdbcmf.POwner:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95  
        bfrdbcmf.AccBal:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95 
        bfrdbcmf.AccStat:FGCOLOR IN BROWSE brw-bfrdbcmf = 13 WHEN bfrdbcmf.AccStat = 95
        bfrdbcmf.AccBal:FGCOLOR IN BROWSE brw-bfrdbcmf = 9 WHEN bfrdbcmf.AccBal < 0 AND bfrdbcmf.AccStat <> 99 AND bfrdbcmf.AccStat <> 95.
END.

/* *** Triggers to Tarif allocations frames *** */




ON CHOOSE OF btn-AClose IN FRAME frm-bfrdbcmf
 DO:
    wsChce = 99.
     RUN proc-AStatus.
     RETURN.
 END.

 ON CHOOSE OF btn-Activate IN FRAME frm-bfrdbcmf
  DO:
     wsChce = 0.
      RUN proc-AStatus.
      RETURN.
  END.

  ON CHOOSE OF btn-Finalise IN FRAME frm-bfrdbcmf
 DO:
    wsChce = 95.
     RUN proc-AStatus.
     RETURN.
 END.





 on 'start-search':u of browse brw-bfrdbcmf
    run SEARCH.ip.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-bfrdbcmf:allow-column-searching = true.
OPEN QUERY qry-bfrdbcmf FOR EACH bfrdbcmf NO-LOCK.
ENABLE ALL WITH FRAME Frm-bfrdbcmf.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-bfrdbcmf.
CLOSE QUERY qry-bfrdbcmf.
HIDE FRAME frm-bfrdbcmf.



PROCEDURE proc-AStatus:

  IF wsChce = 99 THEN DO:
      IF bfrdbcmf.AccBal <> 0 THEN DO:
       MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account has got a Non-Zero Balance. Cannot be closed. Do you want to put this Account on Finalisation?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
           TITLE "" UPDATE lChoice AS
           LOGICAL.
       CASE lChoice:
           WHEN TRUE THEN DO:
               FOR EACH dbcmf WHERE dbcmf.dbacc = bfrdbcmf.dbAcc EXCLUSIVE-LOCK.
                        ASSIGN
                           dbcmf.AccStat = 95.
                           MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account Status Changed to Finalised." VIEW-AS ALERT-BOX.
               END.
               
           END.
           WHEN FALSE THEN DO:
               MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account Status Not Changed" VIEW-AS ALERT-BOX.
           END.
           OTHERWISE  ENABLE ALL WITH FRAME frm-bfrdbcmf.
       END CASE.
    END.
    ELSE DO:
        FOR EACH dbcmf WHERE dbcmf.dbacc = bfrdbcmf.dbAcc EXCLUSIVE-LOCK.
                        ASSIGN
                           dbcmf.AccStat = 99.
                           MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account Status Changed to Closed." VIEW-AS ALERT-BOX.
         END.
        
              
    END.

  END.

  IF wsChce = 95 THEN DO:
     FOR EACH dbcmf WHERE dbcmf.dbacc = bfrdbcmf.dbAcc EXCLUSIVE-LOCK.
                        ASSIGN
                           dbcmf.AccStat = 95.
                           MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account Status Changed to Finalised." VIEW-AS ALERT-BOX.
       END.
  END.

  IF wsChce = 0 THEN DO:
      FOR EACH dbcmf WHERE dbcmf.dbacc = bfrdbcmf.dbAcc EXCLUSIVE-LOCK.
                        ASSIGN
                           dbcmf.AccStat = 0.
                           MESSAGE "Account: " + string(bfrdbcmf.dbacc) SKIP "Account Status Changed to Active." VIEW-AS ALERT-BOX.
      END.
  END.
brw-bfrdbcmf:REFRESH().
END.



procedure Search.ip.
    hCol = browse brw-bfrdbcmf:current-column.
        /*assign  frame Search-opt:title = hCol:label + " Column"
                frame Search-opt:x     = hCol:x
                frame Search-opt:y     = browse b-{&t_zoom_table}:y. */
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
        case trim(hCol:label):
            when "Name" then
            do:
               open query qry-bfrdbcmf
                        FOR EACH bfrdbcmf no-lock 
                            where bfrdbcmf.sortname >= wsSearch:SCREEN-VALUE USE-INDEX NAME.

            END.
            when "Account" then
            do:
               open query qry-bfrdbcmf 
                        FOR EACH bfrdbcmf no-lock 
                            where bfrdbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbAcc.

            END.
            when "StandNo" then
            do:
               open query qry-bfrdbcmf 
                        FOR EACH bfrdbcmf NO-LOCK
                            where bfrdbcmf.StandNo >= wsSearch:SCREEN-VALUE USE-INDEX Stand.

            END.
        END.
        RETURN.
    END.

