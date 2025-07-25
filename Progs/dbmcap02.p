/* Program.................dbmcap02.p
   Notes:...... Debtors meter history file maintenance
   Author:.................S. Mawire
   Modifide:...............S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF SHARED VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varDescrip LIKE dbtmf.Descrip.

DEF VAR wsChoice AS CHAR.
DEF VAR wsDescrip LIKE dbtmf.descrip.
DEF VAR wsDes     LIKE dbtmf.descrip EXTENT 15 FORM "x(30)".
DEF VAR wsSearch   LIKE dbcmf.NAME.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-exit   LABEL "Close".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".
DEF BUTTON btn-Water  LABEL "Amend History".
DEF BUTTON btn-meter.
DEF BUFFER bfrdbcmf FOR dbcmf.



DEFINE QUERY qry-meter FOR dbmmf scrolling.
DEF BROWSE brw-meter QUERY qry-meter
    DISPLAY dbmmf.meter dbmmf.serial 
    WITH 8 DOWN SEPARATORS.



DEFINE QUERY qry-bfrdbcmf FOR bfrdbcmf scrolling.
DEF BROWSE brw-bfrdbcmf QUERY qry-bfrdbcmf
    DISPLAY bfrdbcmf.dbacc  bfrdbcmf.NAME bfrdbcmf.Regid LABEL "ID/Co. Registration"
    bfrdbcmf.StandNo bfrdbcmf.Suburb bfrdbcmf.POwner bfrdbcmf.AccBal WIDTH 15
    WITH 20 DOWN SEPARATORS.



define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 116 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 116 by 18.5.

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
    brw-bfrdbcmf AT ROW 2 COL 15
     btn-Water AT ROW 20.7 COL 10 LABEL "AMEND HISTORY"
    
    space(65) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 120 BY 24 CENTERED
    TITLE "RATEPAYER METER HISTORY MAINTENANCE". 



DEFINE FRAME frm-WTarifinput1
    dbmmf.dbacc     COLON 20 LABEL "Account" VIEW-AS TEXT SKIP(.2) 
    btn-Meter  COLON 5 LABEL "Meter Number"
    dbmmf.Meter      NO-LABEL SPACE(5)
    dbmmf.Serial     LABEL "Meter Serial Number" VIEW-AS TEXT SKIP(.2)
    dbmmf.Route      COLON 20 LABEL "Route" VIEW-AS TEXT SKIP(.2)
    dbmmf.tarif      COLON 20 LABEL "Water Tariff"   VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    dbmmf.mStats COLON 20 LABEL "Meter Status" VIEW-AS TEXT SPACE(5)
    "KEY" SKIP(.1)
    "1: Active" COLON 26 SKIP(.1)
    "2: Non-Functional" COLON 26 SKIP(.1)
    "3: Closed" COLON 26 SKIP(.2)
    "Reading Date" COLON 10 SPACE (5)
    "Reading" SPACE(5)
    "Consumption" SKIP(.1)
    dbmmf.RDate[1] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[1] NO-LABEL SPACE (5)
    dbmmf.Consum[1] NO-LABEL SKIP(.1)
    dbmmf.RDate[2] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[2] NO-LABEL SPACE (5)
    dbmmf.Consum[2] NO-LABEL SKIP(.1)
    dbmmf.RDate[3] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[3] NO-LABEL SPACE (5)
    dbmmf.Consum[3] NO-LABEL SKIP(.1)
    dbmmf.RDate[4] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[4] NO-LABEL SPACE (5)
    dbmmf.Consum[4] NO-LABEL SKIP(.1)
    dbmmf.RDate[5] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[5] NO-LABEL SPACE (5)
    dbmmf.Consum[5] NO-LABEL SKIP(.1)
    dbmmf.RDate[6] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[6] NO-LABEL SPACE (5)
    dbmmf.Consum[6] NO-LABEL SKIP(.1)
    dbmmf.RDate[7] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[7] NO-LABEL SPACE (5)
    dbmmf.Consum[7] NO-LABEL SKIP(.1)
    dbmmf.RDate[8] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[8] NO-LABEL SPACE (5)
    dbmmf.Consum[8] NO-LABEL SKIP(.1)
    dbmmf.RDate[9] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[9] NO-LABEL SPACE (5)
    dbmmf.Consum[9] NO-LABEL SKIP(.1)
    dbmmf.RDate[10] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[10] NO-LABEL SPACE (5)
    dbmmf.Consum[10] NO-LABEL SKIP(.1)
    dbmmf.RDate[11] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[11] NO-LABEL SPACE (5)
    dbmmf.Consum[11] NO-LABEL SKIP(.1)
    dbmmf.RDate[12] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[12] NO-LABEL SPACE (5)
    dbmmf.Consum[12] NO-LABEL SKIP(.1)
    dbmmf.RDate[13] COLON 10 NO-LABEL SPACE (5)
    dbmmf.Read[13] NO-LABEL SPACE (5)
    dbmmf.Consum[13] NO-LABEL           
    skip(1)  
    btn-ok colon 5  SPACE(40)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter History Update".



DEFINE FRAME frm-meter 
    brw-meter AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 10 LABEL "OK" SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Available Meters".



ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO dbmmf.dbacc IN FRAME frm-WTarifinput1.
    RETURN NO-APPLY.
END.


/* *** Triggers to Tarif allocations frames *** */

ON CHOOSE OF btn-Meter IN FRAME frm-WTarifinput1 /* Water Tarif */
DO:
        VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH dbmmf WHERE dbmmf.dbacc = dec(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput1)  NO-LOCK.
          ENABLE ALL WITH FRAME frm-meter.
          WAIT-FOR CHOOSE OF btn-close IN FRAME frm-meter 
                  OR close of THIS-PROCEDURE IN FRAME frm-meter
                 OR CHOOSE OF btn-ok IN FRAME frm-meter
                OR 'enter':u OF brw-meter
                OR 'mouse-select-dblclick' OF brw-meter.
          CLOSE QUERY qry-meter.
          HIDE FRAME frm-meter.
          APPLY 'tab' TO SELF.
          RETURN.
END.


ON CHOOSE OF btn-ok IN FRAME frm-meter
    OR 'enter':u OF brw-meter
    OR 'mouse-select-dblclick' OF brw-meter
DO: 
    GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
         DISPLAY dbmmf.meter dbmmf.serial WITH FRAME frm-WTarifinput1.
        APPLY 'enter' TO dbmmf.meter IN FRAME frm-WTarifinput1.
        RETURN.
 
END.




ON CHOOSE OF btn-Water IN FRAME frm-bfrdbcmf
 DO:
    RUN proc-WTarifinput1.
     RETURN.
 END.

 

 ON 'enter':U OF  dbmmf.Meter IN FRAME frm-WTarifinput1
     OR 'tab':U OF dbmmf.Meter IN FRAME frm-WTarifinput1
     OR 'leave':U OF dbmmf.Meter IN FRAME frm-WTarifinput1
 DO:
            FIND FIRST dbmmf WHERE dbmmf.meter = INT(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1) AND dbmmf.dbacc = DEC(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput1)  NO-ERROR.
            IF AVAILABLE dbmmf THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-ERROR.
               IF AVAILABLE dbtmf THEN
                   wsDescrip = dbtmf.Descrip.
               ELSE wsDescrip = "".
                DISPLAY wsDescrip dbmmf.Serial dbmmf.Route dbmmf.Tarif dbmmf.mStats 
                     dbmmf.Consum[1] dbmmf.Consum[2] dbmmf.Consum[3] dbmmf.Consum[4] dbmmf.Consum[5] dbmmf.Consum[6] dbmmf.Consum[7] dbmmf.Consum[8] dbmmf.Consum[9] dbmmf.Consum[10] dbmmf.Consum[11] dbmmf.Consum[12] dbmmf.Consum[13] 
                    dbmmf.RDate[1] dbmmf.RDate[2] dbmmf.RDate[3] dbmmf.RDate[4] dbmmf.RDate[5] dbmmf.RDate[6] dbmmf.RDate[7] dbmmf.RDate[8] dbmmf.RDate[9] dbmmf.RDate[10] dbmmf.RDate[11] dbmmf.RDate[12] dbmmf.RDate[13] 
                    dbmmf.Read[1] dbmmf.Read[2] dbmmf.Read[3] dbmmf.Read[4] dbmmf.Read[5] dbmmf.Read[6] dbmmf.Read[7] dbmmf.Read[8] dbmmf.Read[9] dbmmf.Read[10] dbmmf.Read[11] dbmmf.Read[12] dbmmf.Read[13] with frame frm-WTarifinput1.
              RETURN.
            END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             MESSAGE "Meter number not associated with account" VIEW-AS ALERT-BOX.
             dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1 = "".
             RETURN NO-APPLY.
         END.
        
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-WTarifinput1
 DO:
     FOR EACH dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc AND dbmmf.meter = int(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1).
                  ASSIGN
                      dbmmf.Consum[1] = dec(dbmmf.Consum[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[2] = dec(dbmmf.READ[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[3] = dec(dbmmf.READ[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[4] = dec(dbmmf.READ[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[5] = dec(dbmmf.READ[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[6] = dec(dbmmf.READ[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[7] = dec(dbmmf.READ[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[8] = dec(dbmmf.READ[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[9] = dec(dbmmf.READ[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[10] = dec(dbmmf.READ[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[11] = dec(dbmmf.READ[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[12] = dec(dbmmf.READ[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Consum[13] = dec(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1) - dec(dbmmf.READ[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1) WHEN dbmmf.rDate[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> "" 
                      dbmmf.RDate[1] =  DATE(dbmmf.RDate[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[2] =  DATE(dbmmf.RDate[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[3] =  DATE(dbmmf.RDate[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[4] =  DATE(dbmmf.RDate[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[5] =  DATE(dbmmf.RDate[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[6] =  DATE(dbmmf.RDate[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[7] =  DATE(dbmmf.RDate[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[8] =  DATE(dbmmf.RDate[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[9] =  DATE(dbmmf.RDate[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[10] =  DATE(dbmmf.RDate[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[11] =  DATE(dbmmf.RDate[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[12] =  DATE(dbmmf.RDate[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.RDate[13] =  DATE(dbmmf.RDate[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[1] =  DEC(dbmmf.READ[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[1]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[2] =  DEC(dbmmf.READ[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[2]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[3] =  DEC(dbmmf.READ[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[3]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[4] =  DEC(dbmmf.READ[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[4]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[5] =  DEC(dbmmf.READ[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[5]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[6] =  DEC(dbmmf.READ[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[6]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[7] =  DEC(dbmmf.READ[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[7]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[8] =  DEC(dbmmf.READ[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[8]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[9] =  DEC(dbmmf.READ[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[9]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[10] =  DEC(dbmmf.READ[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[10]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[11] =  DEC(dbmmf.READ[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[11]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[12] =  DEC(dbmmf.READ[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[12]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> ""
                      dbmmf.Read[13] =  DEC(dbmmf.READ[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1)  WHEN dbmmf.rDate[13]:SCREEN-VALUE IN FRAME frm-WTarifinput1 <> "".
                  MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Meter History Update Completed." VIEW-AS ALERT-BOX.
              END.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-WTarifInput1.
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


PROCEDURE proc-WTarifinput1:
    wsChoice = "Water".
   
   FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-ERROR.
   
   IF AVAILABLE dbmmf THEN DO:
       CLEAR FRAME frm-WtarifInput1 ALL.
        VIEW FRAME frm-WTarifInput1.
         ENABLE ALL EXCEPT WITH FRAME frm-WTarifinput1.
        DISPLAY  dbmmf.dbacc WITH FRAME frm-WTarifinput1.
        btn-ok:LABEL = "UPDATE".
        btn-CLOSE:LABEL = "CANCEL".
        WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-WTarifinput1.
        HIDE FRAME frm-WTarifinput1.
   END.
   ELSE DO:
         MESSAGE "Account does not have a meter attached to it." VIEW-AS ALERT-BOX.
   END.
   
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

