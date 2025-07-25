/* Program.................dbcmf01.p
   Notes:...... Debtors meter file maintenance
   Author:.................S. Mawire
   Modifide:...............S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF SHARED VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsid LIKE dbcmf.dbacc.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.

DEF VAR varDescrip LIKE dbtmf.Descrip.
DEF VAR varFactor LIKE  dbtat.Units.
DEF VAR wsType    LIKE dbtmf.TYPE.
DEF VAR wsR       AS INT.
DEF VAR wsRd       AS DATE.
DEF VAR wsChoice AS CHAR.
DEF VAR wsDescrip LIKE dbtmf.descrip.
DEF VAR wsDes     LIKE dbtmf.descrip EXTENT 15 FORM "x(30)".
DEF VAR wsTar    AS LOGICAL INITIAL NO.
DEF VAR wst AS INT.
DEF VAR X AS INT.
DEF VAR wsAcc LIKE dbmmf.dbacc.
DEF VAR wsMeter LIKE dbmmf.meter.


DEF VAR wsSearch   LIKE dbcmf.NAME.
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-del    LABEL "Delete Meter".
DEF BUTTON btn-exit   LABEL "Close". 

DEF BUTTON btn-Stat LABEL "Update Status".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".

DEF BUTTON btn-ok1     LABEL "NON-FUNCTIONAL".
DEF BUTTON btn-Tarif  LABEL "Add Meter And Route".
DEF BUTTON btn-Water  LABEL "Add/Update Meter".

DEF BUTTON btn-NonFun     LABEL "NON-FUNCTIONAL".
DEF BUTTON btn-Route  LABEL "Add Meter And Route".
DEF BUTTON btn-AddM  LABEL "Add/Update Meter".
DEF BUTTON btn-Tar    LABEL "Rates Tarif".
DEF BUTTON btn-meter.
DEF BUFFER bfrdbcmf FOR dbcmf.
DEF VAR choice AS INTEGER.
DEF VAR wsChoice1 AS INTEGER.

DEFINE QUERY qry-tarif FOR dbtat scrolling.
DEF BROWSE brw-tarif QUERY qry-tarif
    DISPLAY dbtat.tarif varDescrip LABEL "Tariff" dbtat.Units 
    WITH 8 DOWN SEPARATORS.



DEFINE QUERY qry-meter FOR dbmmf scrolling.
DEF BROWSE brw-meter QUERY qry-meter
    DISPLAY dbmmf.meter dbmmf.serial 
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-picktarif FOR dbtmf scrolling.
DEF BROWSE brw-picktarif QUERY qry-picktarif
    DISPLAY  dbtmf.type dbtmf.Tarif dbtmf.Descrip dbtmf.Charge
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
     btn-Water AT ROW 20.7 COL 10
    SPACE(15) btn-Stat
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 120 BY 24 CENTERED
    TITLE "RATEPAYER METER MAINTENANCE". 

DEFINE FRAME frm-WTarifinput
    dbmmf.dbacc     COLON 20 LABEL "Account" VIEW-AS TEXT SKIP(.2) 
    btn-Meter  COLON 5 LABEL "Meter Number"
    dbmmf.Meter       NO-LABEL SKIP(.2)
    dbmmf.Serial     COLON 20 LABEL "Meter Serial Number" SKIP(.2)
    dbmmf.Route      COLON 20 LABEL "Route" SKIP(.2)
    btn-Tar          COLON 20 LABEL "Water Tariff" 
    dbmmf.tarif               NO-LABEL VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    dbmmf.READ[12]   COLON 20 LABEL "Meter Reading"
    dbmmf.rDate[12]  LABEL "Reading Date" SKIP(.2)
    dbmmf.wNextP     COLON 20 LABEL "Next Billing Period"
    skip(1.5)  
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter and Route Assignment".





DEFINE FRAME frm-WTarifinput1
    dbmmf.dbacc     COLON 20 LABEL "Account" VIEW-AS TEXT SKIP(.2) 
    btn-Meter  COLON 5 LABEL "Meter Number"
    dbmmf.Meter      NO-LABEL SKIP(.2)
    dbmmf.Serial     COLON 20 LABEL "Meter Serial Number" VIEW-AS TEXT SKIP(.2)
    dbmmf.Route      COLON 20 LABEL "Route" VIEW-AS TEXT SKIP(.2)
    dbmmf.tarif      COLON 20 LABEL "Water Tariff"   VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    dbmmf.mStats COLON 20 LABEL "Meter Status" VIEW-AS TEXT SPACE(5)
    "KEY" SKIP(.1)
    "1: Activate" COLON 26 SKIP(.1)
    "2: Non-Functional" COLON 26 SKIP(.1)
    "3: Closed" COLON 26 SKIP(.1)
    skip(1)  
    btn-ok colon 5 SPACE(10)btn-ok1 SPACE(10)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter Status Update".

DEFINE FRAME frm-Tarifinput 
    btn-Tar           COLON 15 LABEL "Click to Select Tariff" 
    dbtat.tarif         NO-LABEL VIEW-AS TEXT
    dbtmf.Descrip       NO-LABEL VIEW-AS TEXT SKIP(.2)
    dbtat.units         COLON 22 LABEL "Points/Units"
    skip(0.5)
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Assignment".

DEFINE FRAME frm-meter 
    brw-meter AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 10 LABEL "OK" SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Available Meters".

DEFINE FRAME frm-picktarif 
    brw-picktarif AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "Ok"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Tariff Selection".




ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO dbmmf.dbacc IN FRAME frm-WTarifinput.
    RETURN NO-APPLY.
END.


/* *** Triggers to Tarif allocations frames *** */

ON CHOOSE OF btn-Tar IN FRAME frm-WTarifinput /* Water Tarif */
DO:
  VIEW FRAME frm-picktarif.
  OPEN QUERY qry-picktarif FOR EACH dbtmf WHERE dbtmf.TYPE = 1 NO-LOCK.
  ENABLE ALL WITH FRAME frm-picktarif.
  wsChoice = "Water".
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picktarif 
          OR close of THIS-PROCEDURE IN FRAME frm-picktarif
          OR CHOOSE OF btn-ok IN FRAME frm-picktarif 
          OR 'enter':u OF brw-picktarif
          OR 'mouse-select-dblclick' OF brw-picktarif.
  CLOSE QUERY qry-picktarif.
  HIDE FRAME frm-picktarif.
  APPLY 'tab' TO SELF.
  RETURN.
END.

ON CHOOSE OF btn-Meter IN FRAME frm-WTarifinput /* Water Tarif */
DO:
  IF choice = 1 THEN DO:
          VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH dbmmf WHERE dbmmf.dbacc = dec(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput)  NO-LOCK.
          DISABLE ALL WITH FRAME frm-meter.
          ENABLE btn-close WITH FRAME frm-meter.
         WAIT-FOR CHOOSE OF btn-close IN FRAME frm-meter 
                  OR close of THIS-PROCEDURE IN FRAME frm-meter.
                 
          CLOSE QUERY qry-meter.
          HIDE FRAME frm-meter.
          APPLY 'tab' TO SELF.
          RETURN.
  END.
    IF choice = 2 THEN DO:
          VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH dbmmf WHERE dbmmf.dbacc = dec(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput)  NO-LOCK.
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
END.

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
    IF wsChoice1 = 0 THEN DO:
        GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
       DISPLAY dbmmf.meter dbmmf.serial WITH FRAME frm-WTarifinput.
       APPLY 'tab' TO SELF.
       APPLY 'enter' TO dbmmf.meter IN FRAME frm-WTarifinput.
       RETURN.
   END.
   IF wsChoice1 = 1 THEN DO:
            GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
         DISPLAY dbmmf.meter dbmmf.serial WITH FRAME frm-WTarifinput1.
        APPLY 'tab' TO SELF.
        APPLY 'enter' TO dbmmf.meter IN FRAME frm-WTarifinput1.
        RETURN.
   END.
   IF wsChoice1 = 2  THEN DO:
        GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
        wsAcc = dbmmf.dbacc.
         wsMeter = dbmmf.meter.
        MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Do you really want to delete this meter?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
           TITLE "" UPDATE lChoice AS
           LOGICAL.
       CASE lChoice:
           WHEN TRUE THEN DO:
              FOR EACH dbmmf WHERE dbmmf.dbacc = wsAcc AND dbmmf.meter = wsMeter.
                  ASSIGN
                      dbmmf.mStats = 3.
                  MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Meter deletion completed." VIEW-AS ALERT-BOX.
              END.
           END.
           WHEN FALSE THEN DO:
               MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Meter deletion cancelled." VIEW-AS ALERT-BOX.
           END.
           OTHERWISE RETURN.
               /*HIDE FRAME frm-bfrdbcmf.*/
       END CASE.
        APPLY 'tab' TO SELF.
        RETURN.
   END.
END.

ON CHOOSE OF btn-ok IN FRAME frm-picktarif 
    OR 'enter':u OF brw-picktarif
    OR 'mouse-select-dblclick' OF brw-picktarif
DO: 
   GET CURRENT qry-picktarif EXCLUSIVE-LOCK NO-WAIT.
   ASSIGN wsType = dbtmf.TYPE.
   DISPLAY dbtmf.Tarif @ dbmmf.tarif dbtmf.Descrip @ wsDescrip WITH FRAME frm-WTarifinput.
   
           
   APPLY 'tab' TO SELF.
   RETURN.
END.


ON CHOOSE OF btn-Water IN FRAME frm-bfrdbcmf
 DO:
    wsChoice1 = 0.
     RUN proc-WTarifinput.
     RETURN.
 END.

 ON CHOOSE OF btn-Stat IN FRAME frm-bfrdbcmf
 DO:
     wsChoice1 = 1.
     RUN proc-WTarifinput1.
     RETURN.
 END.

 ON 'enter':U OF  dbmmf.Meter IN FRAME frm-WTarifinput
     OR 'tab':U OF dbmmf.Meter IN FRAME frm-WTarifinput
     OR 'leave':U OF dbmmf.Meter IN FRAME frm-WTarifinput
 DO:
     IF choice = 1 THEN DO:
         FIND FIRST dbmmf WHERE dbmmf.meter = INT(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput) AND dbmmf.dbacc = DEC(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput) NO-LOCK NO-ERROR.
         IF AVAILABLE dbmmf THEN DO:
             MESSAGE "Meter number already taken" VIEW-AS ALERT-BOX.
             dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             APPLY "ENTRY" TO dbmmf.serial.
         END.
         RETURN.
     END.
     IF choice = 2  THEN DO:
            FIND FIRST dbmmf WHERE dbmmf.meter = INT(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput) AND dbmmf.dbacc = DEC(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput)  NO-ERROR.
            IF AVAILABLE dbmmf THEN DO:
                DISPLAY dbmmf.route dbmmf.tarif dbmmf.wNextP dbmmf.read[13] dbmmf.rDate[13] WITH FRAME frm-WTarifinput.
             APPLY "ENTRY" TO dbmmf.serial.
             
         END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             MESSAGE "Meter number not associated with account" VIEW-AS ALERT-BOX.
             dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput = "".
             RETURN NO-APPLY.
         END.
         RETURN.
     END.
 END.

  ON 'enter':U OF  dbmmf.serial IN FRAME frm-WTarifinput
      OR 'tab':U OF dbmmf.serial IN FRAME frm-WTarifinput
      OR 'leave':U OF dbmmf.serial IN FRAME frm-WTarifinput
 DO:
   IF choice = 1 THEN DO:
      FIND FIRST dbmmf WHERE dbmmf.serial = dbmmf.serial:SCREEN-VALUE IN FRAME frm-WTarifinput  NO-LOCK NO-ERROR.
         IF AVAILABLE dbmmf THEN DO:
             MESSAGE "Serial number already used" VIEW-AS ALERT-BOX.
             dbmmf.serial:SCREEN-VALUE IN FRAME frm-WTarifinput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             APPLY "ENTRY" TO dbmmf.route.
         END.
         RETURN.
     END.
     IF choice = 2 THEN DO:
         FIND FIRST dbmmf WHERE dbmmf.serial = dbmmf.serial:SCREEN-VALUE IN FRAME frm-WTarifinput AND dbmmf.dbacc <> DEC(dbmmf.dbacc:SCREEN-VALUE IN FRAME frm-WTarifinput)   NO-ERROR.
         IF AVAILABLE dbmmf THEN DO:
             MESSAGE "Serial number already used" VIEW-AS ALERT-BOX.
             dbmmf.serial:SCREEN-VALUE IN FRAME frm-WTarifinput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             APPLY "ENTRY" TO dbmmf.route.
         END.
         RETURN.
     END.
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-WTarifinput
 DO:
     IF btn-ok:LABEL = "Update" THEN DO:
         ASSIGN dbmmf.Serial   = dbmmf.Serial:SCREEN-VALUE IN FRAME frm-WTarifinput
                dbmmf.Route    = INT(dbmmf.Route:SCREEN-VALUE)
                dbmmf.Tarif    = int(dbmmf.Tarif:SCREEN-VALUE)
                dbmmf.wNextP    = DEC(dbmmf.wNextP:SCREEN-VALUE)
                dbmmf.READ[13] = int(dbmmf.READ[13]:SCREEN-VALUE)
                dbmmf.rDate[13] = DATE(dbmmf.rDate[13]:SCREEN-VALUE).
     END.
     ELSE DO:
         CREATE dbmmf.
         ASSIGN dbmmf.Serial   = dbmmf.Serial:SCREEN-VALUE IN FRAME frm-WTarifinput
                dbmmf.Route    = INT(dbmmf.Route:SCREEN-VALUE)
                dbmmf.Tarif    = INT(dbmmf.Tarif:SCREEN-VALUE)
                dbmmf.READ[13] = DEC(dbmmf.READ[13]:SCREEN-VALUE)
                dbmmf.rDate[13] = DATE(dbmmf.rDate[13]:SCREEN-VALUE)
                dbmmf.wNextP    = DEC(dbmmf.wNextP:SCREEN-VALUE)
                dbmmf.dbacc     = bfrdbcmf.dbacc
                dbmmf.Meter     = INT(dbmmf.Meter:SCREEN-VALUE).
                
     END.
     IF dbmmf.tarif <> 0 AND mStat <> 2 THEN
         mStats          = 1.
     ELSE mStats          = 3.
     DISABLE dbmmf.Meter WITH FRAME frm-WTarifInput.
     DISPLAY dbmmf.Tarif dbtmf.descrip @ wsdes[15] dbmmf.wNextP WITH FRAME frm-input.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-WTarifInput.
 END.

 ON CHOOSE OF btn-del IN FRAME frm-bfrdbcmf
 DO:
     wsChoice1 = 2.
    /*find meters attached t oaccount and let the user select the meter required */
    FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-ERROR.
     IF AVAILABLE dbmmf THEN DO:
        VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc  AND dbmmf.mstats <> 3 NO-LOCK.
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
    ELSE  DO:
       MESSAGE "Account does not have a meter attached to it." VIEW-AS ALERT-BOX.
    END. 
     
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
                DISPLAY wsDescrip dbmmf.Serial dbmmf.Route dbmmf.Tarif dbmmf.mStats with frame frm-WTarifinput1.
             
            END.
         ELSE IF NOT AVAILABLE dbmmf THEN DO:
             MESSAGE "Meter number not associated with account" VIEW-AS ALERT-BOX.
             dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1 = "".
             RETURN NO-APPLY.
         END.
         RETURN.
     
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-WTarifinput1
 DO:
     FOR EACH dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc AND dbmmf.meter = int(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1).
                  ASSIGN
                      dbmmf.mStats = 1.
                  MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Meter Activation Completed." VIEW-AS ALERT-BOX.
              END.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-WTarifInput1.
 END.

 ON CHOOSE OF btn-ok1 IN FRAME frm-WTarifinput1
 DO:
     FOR EACH dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc AND dbmmf.meter = int(dbmmf.meter:SCREEN-VALUE IN FRAME frm-WTarifinput1).
                  ASSIGN
                      dbmmf.mStats = 2.
                  MESSAGE "Account: " + string(dbmmf.dbacc) SKIP "Meter Number: " + string(dbmmf.Meter) SKIP "Meter Set To Non-Functional." VIEW-AS ALERT-BOX.
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




PROCEDURE proc-WTarifinput:
    wsChoice = "Water".
   CLEAR FRAME frm-WtarifInput ALL.
   VIEW FRAME frm-WTarifInput.
   FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-ERROR.
   FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-ERROR.
   IF AVAILABLE dbtmf THEN
       wsDescrip = dbtmf.Descrip.
   ELSE wsDescrip = "".
   ENABLE ALL EXCEPT WITH FRAME frm-WTarifinput.
    
   IF AVAILABLE dbmmf THEN DO:
        MESSAGE "Account already has a meter." SKIP "Do you really want to add a New Meter?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
           TITLE "" UPDATE lChoice AS
           LOGICAL.
          CASE lChoice:
           WHEN TRUE THEN DO:
               choice = 1.
                btn-ok:LABEL = "SAVE".
                DISPLAY  bfrdbcmf.dbacc @ dbmmf.dbacc WITH FRAME frm-WTarifinput.
           END.
           WHEN FALSE THEN DO:
               DISPLAY  bfrdbcmf.dbacc @ dbmmf.dbacc  WITH FRAME frm-WTarifinput.
                btn-ok:LABEL = "Update".
                choice = 2.
           END.
           OTHERWISE RETURN.
          END CASE.
   END.
   ELSE DO:
            DISPLAY  bfrdbcmf.dbacc @ dbmmf.dbacc  WITH FRAME frm-WTarifinput.
   END.
   
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-WTarifinput.
   HIDE FRAME frm-WTarifinput.
END.


PROCEDURE proc-WTarifinput1:
    wsChoice = "Water".
   
   FIND FIRST dbmmf WHERE dbmmf.dbacc = bfrdbcmf.dbacc NO-ERROR.
   
   IF AVAILABLE dbmmf THEN DO:
       CLEAR FRAME frm-WtarifInput1 ALL.
        VIEW FRAME frm-WTarifInput1.
         ENABLE ALL EXCEPT WITH FRAME frm-WTarifinput1.
        DISPLAY  dbmmf.dbacc WITH FRAME frm-WTarifinput1.
        btn-ok:LABEL = "ACTIVATE".
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

