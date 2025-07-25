/* Program.................dbcmf01.p
   Notes:...... Add/Update meter file maintenance
   Author:.................S. Mawire
   Modifide:...............S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable            dbmmf
&SCOPED-DEFINE skey                dbmmf.dbacc

{varlibrary.i}
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

DEF BUTTON btn-Stat LABEL "Update Status".
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

DEFINE QUERY qry-meter FOR bfr{&tmpTable} scrolling.
DEF BROWSE brw-meter QUERY qry-meter
    DISPLAY bfr{&tmpTable}.meter bfr{&tmpTable}.serial 
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
     btn-AddM AT ROW 20.7 COL 10
    SPACE(15) btn-Stat
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    with view-as dialog-box keep-tab-order no-validate
         NO-LABEL no-underline three-d SCROLLABLE SIZE 120 BY 24 CENTERED
    TITLE "RATEPAYER METER MAINTENANCE". 

DEFINE FRAME frm-MeterInput
    bfr{&tmpTable}.dbacc     COLON 20 LABEL "Account" VIEW-AS TEXT SKIP(.2) 
    btn-Meter  COLON 5 LABEL "Meter Number"
    bfr{&tmpTable}.Meter       NO-LABEL SKIP(.2)
    bfr{&tmpTable}.Serial     COLON 20 LABEL "Meter Serial Number" SKIP(.2)
    bfr{&tmpTable}.Route      COLON 20 LABEL "Route" SKIP(.2)
    btn-Tar          COLON 20 LABEL "Water Tariff" 
    bfr{&tmpTable}.tarif               NO-LABEL VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfr{&tmpTable}.READ[12]   COLON 20 LABEL "Meter Reading"
    bfr{&tmpTable}.rDate[12]  LABEL "Reading Date" SKIP(.2)
    bfr{&tmpTable}.wNextP     COLON 20 LABEL "Next Billing Period"
    skip(1.5)  
    btn-ok colon 10 SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Water Meter and Route Assignment".

DEFINE FRAME frm-MeterStatus
    bfr{&tmpTable}.dbacc     COLON 20 LABEL "Account" VIEW-AS TEXT SKIP(.2) 
    btn-Meter  COLON 5 LABEL "Meter Number"
    bfr{&tmpTable}.Meter      NO-LABEL SKIP(.2)
    bfr{&tmpTable}.Serial     COLON 20 LABEL "Meter Serial Number" VIEW-AS TEXT SKIP(.2)
    bfr{&tmpTable}.Route      COLON 20 LABEL "Route" VIEW-AS TEXT SKIP(.2)
    bfr{&tmpTable}.tarif      COLON 20 LABEL "Water Tariff"   VIEW-AS TEXT
    wsDescrip             NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfr{&tmpTable}.mStats COLON 20 LABEL "Meter Status" VIEW-AS TEXT SPACE(5)
    "KEY" SKIP(.1)
    "1: Activate" COLON 26 SKIP(.1)
    "2: Non-Functional" COLON 26 SKIP(.1)
    "3: Closed" COLON 26 SKIP(.1)
    skip(1)  
    btn-ok colon 5 SPACE(10)btn-NonFun SPACE(10)  btn-close
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
    APPLY 'entry' TO bfr{&tmpTable}.dbacc IN FRAME frm-MeterInput.
    RETURN NO-APPLY.
END.


/* *** Triggers to Tarif allocations frames *** */

ON CHOOSE OF btn-Tar IN FRAME frm-MeterInput /* Water Tarif */
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

ON CHOOSE OF btn-Meter IN FRAME frm-MeterInput /* Water Meter */
DO:
  IF choice = 1 THEN DO:
          VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = dec(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterInput)  NO-LOCK.
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
          OPEN QUERY qry-meter FOR EACH bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = dec(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterInput)  NO-LOCK.
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

ON CHOOSE OF btn-Meter IN FRAME frm-MeterStatus /* Water Tarif */
DO:
        VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = dec(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterStatus)  NO-LOCK.
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
       DISPLAY bfr{&tmpTable}.meter bfr{&tmpTable}.serial WITH FRAME frm-MeterInput.
       APPLY 'tab' TO SELF.
       APPLY 'enter' TO bfr{&tmpTable}.meter IN FRAME frm-MeterInput.
       RETURN.
   END.
   IF wsChoice1 = 1 THEN DO:
         GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
         DISPLAY bfr{&tmpTable}.meter bfr{&tmpTable}.serial WITH FRAME frm-MeterStatus.
        APPLY 'tab' TO SELF.
        APPLY 'enter' TO bfr{&tmpTable}.meter IN FRAME frm-MeterStatus.
        RETURN.
   END.
   IF wsChoice1 = 2  THEN DO:
        GET CURRENT qry-meter EXCLUSIVE-LOCK NO-WAIT.
        ASSIGN wsAcc = bfr{&tmpTable}.dbacc
               wsMeter = bfr{&tmpTable}.meter.
        MESSAGE "Account: " + string(bfr{&tmpTable}.dbacc) SKIP "Meter Number: " + string(bfr{&tmpTable}.Meter) 
            SKIP "Do you really want to delete this meter?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            TITLE "" UPDATE lChoice AS LOGICAL.
       CASE lChoice:
           WHEN TRUE THEN DO:
               wsbtn = "EDIT".
              FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = wsAcc AND bfr{&tmpTable}.meter = wsMeter NO-ERROR.
                  ASSIGN bfr{&tmpTable}.mStats = 3.
                  MESSAGE "Account: " + string(bfr{&tmpTable}.dbacc) 
                      SKIP "Meter Number: " + string(bfr{&tmpTable}.Meter) SKIP "Meter deletion completed." VIEW-AS ALERT-BOX.
           END.
           WHEN FALSE THEN DO:
               MESSAGE "Account: " + string(bfr{&tmpTable}.dbacc) SKIP "Meter Number: " + string(bfr{&tmpTable}.Meter) SKIP "Meter deletion cancelled." VIEW-AS ALERT-BOX.
           END.
           OTHERWISE RETURN.
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
   DISPLAY dbtmf.Tarif @ bfr{&tmpTable}.tarif dbtmf.Descrip @ wsDescrip WITH FRAME frm-MeterInput.         
   APPLY 'tab' TO SELF.
   RETURN.
END.


ON CHOOSE OF btn-AddM IN FRAME frm-bfrdbcmf
 DO:
    wsbtn = "ADD".
    wsChoice1 = 0.
     RUN proc-WTarifinput.
     RETURN.
 END.

 ON CHOOSE OF btn-Stat IN FRAME frm-bfrdbcmf
 DO:
     wsChoice1 = 1.
     wsbtn = "EDIT".
     RUN proc-WTarifinput1.
     RETURN.
 END.

 ON 'enter':U OF  bfr{&tmpTable}.Meter IN FRAME frm-MeterInput
     OR 'tab':U OF bfr{&tmpTable}.Meter IN FRAME frm-MeterInput
     OR 'leave':U OF bfr{&tmpTable}.Meter IN FRAME frm-MeterInput
 DO:
     IF choice = 1 THEN DO:
         FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.meter = INT(bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterInput) AND bfr{&tmpTable}.dbacc = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterInput) NO-LOCK NO-ERROR.
         IF AVAILABLE bfr{&tmpTable} THEN DO:
             MESSAGE "Meter number already assigned " VIEW-AS ALERT-BOX.
             bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterInput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
             APPLY "ENTRY" TO bfr{&tmpTable}.serial.
         END.
         RETURN.
     END.
     IF choice = 2  THEN DO:
            FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.meter = INT(bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterInput) AND bfr{&tmpTable}.dbacc = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterInput)  NO-ERROR.
            IF AVAILABLE bfr{&tmpTable} THEN DO:
                DISPLAY bfr{&tmpTable}.route bfr{&tmpTable}.tarif bfr{&tmpTable}.wNextP bfr{&tmpTable}.read[12] bfr{&tmpTable}.rDate[12] WITH FRAME frm-MeterInput.
             APPLY "ENTRY" TO bfr{&tmpTable}.serial.
             
         END.
         ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
             MESSAGE "Meter number not associated with account" VIEW-AS ALERT-BOX.
             bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterInput = "".
             RETURN NO-APPLY.
         END.
         RETURN.
     END.
 END.

  ON 'enter':U OF  bfr{&tmpTable}.serial IN FRAME frm-MeterInput
      OR 'tab':U OF bfr{&tmpTable}.serial IN FRAME frm-MeterInput
      OR 'leave':U OF bfr{&tmpTable}.serial IN FRAME frm-MeterInput
 DO:
   IF choice = 1 THEN DO:
      FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.serial = bfr{&tmpTable}.serial:SCREEN-VALUE IN FRAME frm-MeterInput  NO-LOCK NO-ERROR.
         IF AVAILABLE bfr{&tmpTable} THEN DO:
             MESSAGE "Serial number already used" VIEW-AS ALERT-BOX.
             bfr{&tmpTable}.serial:SCREEN-VALUE IN FRAME frm-MeterInput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
             APPLY "ENTRY" TO bfr{&tmpTable}.route.
         END.
         RETURN.
     END.
     IF choice = 2 THEN DO:
         FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.serial = bfr{&tmpTable}.serial:SCREEN-VALUE IN FRAME frm-MeterInput AND bfr{&tmpTable}.dbacc <> DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterInput)   NO-ERROR.
         IF AVAILABLE bfr{&tmpTable} THEN DO:
             MESSAGE "Serial number already used" VIEW-AS ALERT-BOX.
             bfr{&tmpTable}.serial:SCREEN-VALUE IN FRAME frm-MeterInput = "".
             RETURN NO-APPLY.
         END.
         ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
             APPLY "ENTRY" TO bfr{&tmpTable}.route.
         END.
         RETURN.
     END.
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-MeterInput
 DO:
     IF btn-ok:LABEL = "Update" THEN DO:
         wsbtn = "EDIT".
         ASSIGN bfr{&tmpTable}.Serial   = bfr{&tmpTable}.Serial:SCREEN-VALUE IN FRAME frm-MeterInput
                bfr{&tmpTable}.Route    = INT(bfr{&tmpTable}.Route:SCREEN-VALUE)
                bfr{&tmpTable}.Tarif    = int(bfr{&tmpTable}.Tarif:SCREEN-VALUE)
                bfr{&tmpTable}.wNextP    = DEC(bfr{&tmpTable}.wNextP:SCREEN-VALUE)
                bfr{&tmpTable}.READ[12] = DEC(bfr{&tmpTable}.READ[12]:SCREEN-VALUE)
                bfr{&tmpTable}.rDate[12] = DATE(bfr{&tmpTable}.rDate[12]:SCREEN-VALUE).
     END.
     ELSE DO:
         wsBtn = "ADD".
         CREATE bfr{&tmpTable}.
         ASSIGN bfr{&tmpTable}.Serial   = bfr{&tmpTable}.Serial:SCREEN-VALUE IN FRAME frm-MeterInput
                bfr{&tmpTable}.Route    = INT(bfr{&tmpTable}.Route:SCREEN-VALUE)
                bfr{&tmpTable}.Tarif    = INT(bfr{&tmpTable}.Tarif:SCREEN-VALUE)
                bfr{&tmpTable}.READ[12] = DEC(bfr{&tmpTable}.READ[12]:SCREEN-VALUE)
                bfr{&tmpTable}.rDate[12] = DATE(bfr{&tmpTable}.rDate[12]:SCREEN-VALUE)
                bfr{&tmpTable}.wNextP    = DEC(bfr{&tmpTable}.wNextP:SCREEN-VALUE)
                bfr{&tmpTable}.dbacc     = bfrdbcmf.dbacc
                bfr{&tmpTable}.Meter     = INT(bfr{&tmpTable}.Meter:SCREEN-VALUE)
                bfr{&tmpTable}.Credate   = TODAY 
                bfr{&tmpTable}.DeviceName = lc-host
                bfr{&tmpTable}.UID       = varuser.
                
     END.
     IF bfr{&tmpTable}.tarif <> 0 AND mStat <> 2 THEN
         mStats          = 1.
     ELSE mStats          = 3.
     DISABLE bfr{&tmpTable}.Meter WITH FRAME frm-MeterInput.
     DISPLAY bfr{&tmpTable}.Tarif dbtmf.descrip @ wsdes[15] bfr{&tmpTable}.wNextP WITH FRAME frm-input.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-MeterInput.
 END.

 ON CHOOSE OF btn-del IN FRAME frm-bfrdbcmf
 DO:
     wsChoice1 = 2.
    /*find meters attached t oaccount and let the user select the meter required */
    FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc NO-ERROR.
     IF AVAILABLE bfr{&tmpTable} THEN DO:
        VIEW FRAME frm-meter.
          OPEN QUERY qry-meter FOR EACH bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc  AND bfr{&tmpTable}.mstats <> 3 NO-LOCK.
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

 ON 'enter':U OF  bfr{&tmpTable}.Meter IN FRAME frm-MeterStatus
     OR 'tab':U OF bfr{&tmpTable}.Meter IN FRAME frm-MeterStatus
     OR 'leave':U OF bfr{&tmpTable}.Meter IN FRAME frm-MeterStatus
 DO:
            FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.meter = INT(bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterStatus) 
                AND bfr{&tmpTable}.dbacc = DEC(bfr{&tmpTable}.dbacc:SCREEN-VALUE IN FRAME frm-MeterStatus)  NO-ERROR.
            IF AVAILABLE bfr{&tmpTable} THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = bfr{&tmpTable}.tarif AND dbtmf.TYPE = 1 NO-ERROR.
               IF AVAILABLE dbtmf THEN
                   wsDescrip = dbtmf.Descrip.
               ELSE wsDescrip = "".
                DISPLAY wsDescrip bfr{&tmpTable}.Serial bfr{&tmpTable}.Route bfr{&tmpTable}.Tarif bfr{&tmpTable}.mStats with frame frm-MeterStatus.
             
            END.
         ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
             MESSAGE "Meter number not associated with account" VIEW-AS ALERT-BOX.
             bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterStatus = "".
             RETURN NO-APPLY.
         END.
         RETURN.
     
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-MeterStatus
 DO:
     wsbtn = "EDIT".
     FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc AND bfr{&tmpTable}.meter = int(bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterStatus).
     ASSIGN bfr{&tmpTable}.mStats = 1.
     MESSAGE "Account: " + string(bfr{&tmpTable}.dbacc) SKIP "Meter Number: " + string(bfr{&tmpTable}.Meter) SKIP "Meter Activation Completed." VIEW-AS ALERT-BOX.
     APPLY "close" TO THIS-PROCEDURE IN FRAME frm-MeterStatus.
 END.

 ON CHOOSE OF btn-NonFun IN FRAME frm-MeterStatus
 DO:
     wsbtn = "EDIT".
     FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc AND bfr{&tmpTable}.meter = int(bfr{&tmpTable}.meter:SCREEN-VALUE IN FRAME frm-MeterStatus).
     ASSIGN bfr{&tmpTable}.mStats = 2.
     MESSAGE "Account: " + string(bfr{&tmpTable}.dbacc) SKIP "Meter Number: " + string(bfr{&tmpTable}.Meter) SKIP "Meter Set To Non-Functional." VIEW-AS ALERT-BOX.
    APPLY "close" TO THIS-PROCEDURE IN FRAME frm-MeterStatus.
 END.

 on 'start-search':u of browse brw-bfrdbcmf
    run SEARCH.ip.

/* {audit.i}*/

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
   CLEAR FRAME frm-MeterInput ALL.
   VIEW FRAME frm-MeterInput.
   FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc NO-ERROR.
   FIND FIRST dbtmf WHERE dbtmf.tarif = bfr{&tmpTable}.tarif AND dbtmf.TYPE = 1 NO-ERROR.
   IF AVAILABLE dbtmf THEN
       wsDescrip = dbtmf.Descrip.
   ELSE wsDescrip = "".
   ENABLE ALL EXCEPT WITH FRAME frm-MeterInput.
    
   IF AVAILABLE bfr{&tmpTable} THEN DO:
        MESSAGE "Account already has a meter." SKIP "Do you really want to add a New Meter?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
           TITLE "" UPDATE lChoice AS
           LOGICAL.
          CASE lChoice:
           WHEN TRUE THEN DO:
               choice = 1.
                btn-ok:LABEL = "SAVE".
                DISPLAY  bfrdbcmf.dbacc @ bfr{&tmpTable}.dbacc WITH FRAME frm-MeterInput.
           END.
           WHEN FALSE THEN DO:
               DISPLAY  bfrdbcmf.dbacc @ bfr{&tmpTable}.dbacc  WITH FRAME frm-MeterInput.
                btn-ok:LABEL = "Update".
                choice = 2.
           END.
           OTHERWISE RETURN.
          END CASE.
   END.
   ELSE DO:
            DISPLAY  bfrdbcmf.dbacc @ bfr{&tmpTable}.dbacc  WITH FRAME frm-MeterInput.
   END.
   
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-MeterInput.
   HIDE FRAME frm-MeterInput.
END.


PROCEDURE proc-WTarifinput1:
    wsChoice = "Water".
   
   FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = bfrdbcmf.dbacc NO-ERROR.
   
   IF AVAILABLE bfr{&tmpTable} THEN DO:
       CLEAR FRAME frm-MeterStatus ALL.
        VIEW FRAME frm-MeterStatus.
         ENABLE ALL EXCEPT WITH FRAME frm-MeterStatus.
        DISPLAY  bfr{&tmpTable}.dbacc WITH FRAME frm-MeterStatus.
        btn-ok:LABEL = "ACTIVATE".
        btn-CLOSE:LABEL = "CANCEL".
        WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-MeterStatus.
        HIDE FRAME frm-MeterStatus.
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

