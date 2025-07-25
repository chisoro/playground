/*....Program   :ImpMeter.p
      Notes     : Migrate Promun Data to the SimAcc system.
*************************************************************/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR stAcc   LIKE dbcmf.dbacc.
DEF VAR enAcc   LIKE dbcmf.dbacc.
DEF VAR wsstatus  LIKE dbcmf.dbacc.
DEF  VAR wsUpper AS DEC.
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsQty     AS DEC.
DEF VAR wsNo      AS INT. /* Count of months with meter readings */

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEFINE FRAME frm-main
    SKIP(2)
    stAcc   COLON 30 LABEL "Enter Start Account" SKIP(1)
    enAcc   COLON 30 LABEL "Enter End Account" SKIP(1.5)
    wsStatus  COLON 5 NO-LABEL view-as text "Processing...." 
    skip(4)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "Promun Meter Reading Importation".


ON CHOOSE OF btn-oK IN FRAME frm-main DO:
    ASSIGN stAcc 
           enAcc.
    session:set-wait-state("").
    RUN Imp-ip.
    MESSAGE "IMPORT OF Readings COMPLETED..." VIEW-AS ALERT-BOX.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN stAcc:SCREEN-VALUE = "0"
       enAcc:SCREEN-VALUE = "999999999998".
       
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Imp-ip:
/* Ratepayer Account */
    FOR EACH munrmf WHERE  munrmf.acc >= stAcc AND munrmf.acc <= enAcc /* AND munrmf.read[13] > 0 */ NO-LOCK:
      DISPLAY munrmf.acc @ wsStatus WITH FRAME frm-main.
      PAUSE 0.
       FIND FIRST dbmmf WHERE dbmmf.dbacc = munrmf.acc AND dbmmf.meter = munrmf.meter-id  NO-ERROR.
       IF AVAILABLE dbmmf THEN
       DO:
           /*dbmmf.serial = munrmf.meter-no. */
            ASSIGN dbmmf.READ[13] = munrmf.READ[13]
                   dbmmf.rDate[13] = munrm.read-date[13]
                   dbmmf.READ[12] = munrmf.READ[12]
                   dbmmf.rDate[12] = munrm.read-date[12]
                   dbmmf.READ[11] = munrmf.READ[11]
                   dbmmf.rDate[11] = munrm.read-date[11]
                   dbmmf.READ[10] = munrmf.READ[10]
                   dbmmf.rDate[10] = munrm.read-date[10]
                   dbmmf.READ[9] = munrmf.READ[9]
                   dbmmf.rDate[9] = munrm.read-date[9]
                   dbmmf.READ[8] = munrmf.READ[8]
                   dbmmf.rDate[8] = munrm.read-date[8]
                   dbmmf.READ[7] = munrmf.READ[7]
                   dbmmf.rDate[7] = munrm.read-date[7]
                   dbmmf.READ[6] = munrmf.READ[6]
                   dbmmf.rDate[6] = munrm.read-date[6]
                   dbmmf.READ[5] = munrmf.READ[5]
                   dbmmf.rDate[5] = munrm.read-date[5]
                   dbmmf.READ[4] = munrmf.READ[4]
                   dbmmf.rDate[4] = munrm.read-date[4]
                   dbmmf.READ[3] = munrmf.READ[3]
                   dbmmf.rDate[3] = munrm.read-date[3]
                   dbmmf.READ[2] = munrmf.READ[2]
                   dbmmf.rDate[2] = munrm.read-date[2]
                   dbmmf.READ[1] = munrmf.READ[1]
                   dbmmf.rDate[1] = munrm.read-date[1].
            DO X = 2 TO 13:
                ASSIGN dbmmf.consum[X] = dbmmf.READ[X] - dbmmf.READ[X - 1].

            END.
            dbmmf.consum[1] = 0.
            IF dbmmf.READ[13] < dbmmf.READ[12] AND dbmmf.READ[13] > 0 THEN DO: /* meter turn */
                wsUpper = 1.
                DO j = 1 TO LENGTH(STRING(INT(dbmmf.READ[12]))): /* Set Maxmum Meter Reading */
                    wsUpper = wsUpper * 10. 
                END.
                ASSIGN dbmmf.consum[13] = ( wsUpper - dbmmf.READ[12]) + dbmmf.READ[13]
                      dbmmf.com[13] = 99. /* Meter turn */
            END.
            ELSE IF dbmmf.READ[13] = 0  THEN
                ASSIGN dbmmf.com[13] = 98 /* NOT READ */
                       dbmmf.consum[13] = munrmf.consum.
            ELSE IF dbmmf.read[13] < dbmmf.READ[12] AND dbmmf.READ[13] <> ? AND dbmmf.READ[12] <> ? THEN 
                    dbmmf.consum[13] = dbmmf.READ[13] - dbmmf.READ[12]. /* Actual Reading */
            ELSE IF dbmmf.RDate[13] = ? OR dbmmf.READ[13] = ? THEN DO: /* not read - average */
                ASSIGN wsQty = 0.
                       X = 13.
                       wsNo = 0.
                 DO WHILE X >=  2:
                      ASSIGN wsQty = wsQty + dbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                                      WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                             wsNo = wsNo + 1 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                             X = X - 1.
                 END.
                 ASSIGN dbmmf.comm[13]   = 96
                        dbmmf.consum[13] = ROUND((wsQty / wsNo),2).
              END.
       END.
    END.
END PROCEDURE.
