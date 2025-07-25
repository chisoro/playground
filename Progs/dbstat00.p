/* Program.................dbStat00.p
   Notes:................. Monthly statement print
   Author:.................S. Mawire
*/
&SCOPED-DEFINE pagesize           0

session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR c AS INT.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsTitle AS CHAR FORM "x(80)" INIT "============================ MONTHLY SERVICES STATEMENTS===========================".
DEF VAR varRegNo LIKE SIMCTR.REGNO.
DEF VAR vaAcc LIKE dbcmf.dbacc.
DEF VAR varCouncil LIKE SIMCTR.CONAME.
DEF VAR varAdd1 LIKE SIMCTR.Add1.
DEF VAR varAdd2 LIKE SIMCTR.Add2.
DEF VAR varAdd3 LIKE SIMCTR.Add3.
DEF VAR varVat AS DEC.
DEF VAR varVats AS CHAR.
DEF VAR varTotal AS DEC.
DEF VAR wsline AS CHAR FORM "x(80)"
    INIT "=============================================================================================".
DEF VAR varDate AS DATE LABEL "DATE".
DEF VAR varRef LIKE dbmtf.Ref LABEL "REF" FORM "x(10)".
DEF VAR varDesc LIKE dbtmf.DESCRIP LABEL "DESCRIPTION".
DEF VAR varAmt LIKE dbmtf.Amt LABEL "AMOUNT" FORM "zzzzzz9.99-".
DEF VAR wsAge LIKE dbmtf.Amt FORM "zzzzzz9.99-" EXTENT 6 .
DEF VAR wsCr LIKE dbmtf.Amt FORM "zzzzzz9.99-".
DEF VAR varDue LIKE dbmtf.Amt FORM "zzzzzz9.99-".
DEF VAR wsMessage1 AS CHAR FORM "X(80)" .
DEF VAR wsMessage2 AS CHAR FORM "X(80)".
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsStatus AS CHAR.
DEF VAR wsZero AS LOGICAL.
DEF VAR wsAccDate AS DATE.
DEF VAR wsDueDate AS DATE.
DEF VAR wsDueDate1 AS DATE.
DEF VAR wsPeriod LIKE simctr.curper.
DEF VAR wsVarTotal AS DEC.
DEF VAR wsCharge AS DEC.
DEF VAR wsAcc LIKE dbmmf.dbacc.
DEF VAR wsTar LIKE dbtmf.Tarif.
DEF VAR wsMeter LIKE dbmmf.serial.
DEF VAR varName LIKE dbcmf.NAME.
DEF VAR varSgrp LIKE dbsgr.sgrp.
DEF VAR varAcc LIKE dbcmf.dbacc.
DEF VAR stGrp  LIKE tblGroup.intgrp.
DEF VAR endGrp  LIKE tblGroup.intgrp.
DEF VAR wsOpt AS LOGICAL INITIAL NO.
DEF VAR wslabel AS CHAR.

DEF TEMP-TABLE wsDbblf NO-UNDO
     FIELD dbacc LIKE dbblf.dbacc
     FIELD amt LIKE dbblf.amt
     FIELD INT LIKE dbblf.INT.

DEF TEMP-TABLE wsDbcmf no-undo
    FIELD dbacc LIKE dbcmf.dbacc
    FIELD AccBal LIKE dbcmf.AccBal
    FIELD NAME LIKE dbcmf.Name
    FIELD Add1 LIKE dbcmf.Add1 
    FIELD  add2 LIKE dbcmf.add2
    FIELD ADD3 LIKE dbcmf.Add3
    FIELD balbf LIKE dbcmf.balbf
    INDEX acc dbacc ASC.

DEF TEMP-TABLE wsdbtmf no-undo
    FIELD tarif LIKE dbtmf.tarif
    FIELD TYPE LIKE dbtmf.TYPE
    FIELD sgrp LIKE dbtmf.sgrp
    FIELD charge LIKE dbtmf.charge.

DEF TEMP-TABLE tmpwork LIKE dbmtf.

DEF BUTTON stBGrp LABEL "Start Group".
DEF BUTTON enBGrp LABEL "End Group".
DEF BUTTON btn-exit   LABEL "Exit".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 12.5.

DEF QUERY qry-grp FOR tblGroup SCROLLING.
DEF BROWSE brw-grp QUERY qry-grp
    DISPLAY tblGroup.intgrp tblGroup.Descrip
     WITH NO-LABEL 12 DOWN SEPARATORS.

DEF FRAME frm-main
    SKIP(1)
    wsOpt       LABEL "Group Accounts (Y/N)" COLON 30 SKIP(0.2)
    stBGrp      NO-LABEL COLON 18 stgrp  NO-LABEL 
    enBGrp      NO-LABEL  COLON 55 endGrp SKIP(0.2)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.2)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.2)
    wsAccDate   LABEL "Accounting Date" COLON 30 SPACE(10)
    wsDueDate   LABEL "Last Receipt Date"                   SKIP(0.2)
    wsPeriod    LABEL "Accounting Period" COLON 30 SPACE (13)
    wsDueDate1   LABEL "Pay Before or On"             SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.2)
     wsmessage1 LABEL "MESSAGE" COLON 15 SKIP          
     wsmessage2 NO-LABEL        COLON 15
    SKIP(0.2)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 14.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 14 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 105 BY 17.5 KEEP-TAB-ORDER
    TITLE "MONTHLY STATEMENT PRINT" VIEW-AS DIALOG-BOX.   

DEF FRAME frm-grp
    brw-grp AT ROW 1 COL 5
    btn-ok  AT ROW 11 COL 7
    SPACE(12) btn-exit SKIP
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 40 BY 13 KEEP-TAB-ORDER
    TITLE "Group Selection" VIEW-AS DIALOG-BOX. 

/* statement header */
form
  skip(1)
     /*ws-tax-invoice         at 13 */
     varRegNo              at 33
     wsAcc                 at 70
  skip(1)
     dbcmf.name            at 5
  skip
     dbcmf.add1            at 5
     /*ws-deposit-total       at 55 
     space(0) 
     ws-d-b
     space(0)*/
     wsDueDate            at 68
     wsAccDate            at 77
  skip
     dbcmf.add2           at 5
  skip
     dbcmf.add3           at 5
  SKIP
     /*dbcmf.pcode           at 5   */
     dbcmf.stno           at 55
     dbcmf.street         at 65
  skip(1)
     "DATE"              at 6
     "REFERENCE"         at 14
     "DESCRIPTION"       at 24
     /*d-rd-date              at 41
     d-read-date            at 55 */
     "(USD) AMOUNT"            at 73
  skip(1)
         with width 132
              NO-BOX STREAM-IO
              no-labels
              frame frm-hdr.

FORM                                              /* tran line */
    varDate   AT 5
     varRef   AT 14              
     varDesc  AT 26 FORM "x(40)"
     varAmt   AT 75 SPACE(0)
     varVats 
     with width 132
	  no-box
	  no-labels
      DOWN
      STREAM-IO
	  frame frm-trans.

form
     varName                at 5
     varAcc                 at 49
     varSgrp                at 70 FORM ">>"
     varAmt                 at 75
     with width 132
          no-box
          no-labels
          down
          frame frm-stub. 


FORM
     /*wsline          AT 4 SKIP*/
     "CREDIT"        AT 8
     "90+"           AT 21
     "60"            AT 33
     "30"            AT 44
     "CURRENT"       AT 53
     ""    AT 70
     SKIP(1)
     wsAge[1]      AT 4  
     wsage[5]      AT 16
     wsage[4]      AT 27
     wsage[3]      AT 38
     wsage[2]      AT 49
     wsDueDate1      at 64
     wsAge[6]      AT 75
     SKIP(1)
     wsmessage1    AT 5         
     wsmessage2    AT 5
     /*SKIP(1)*/
     with width 132
	  no-box
	  no-labels
      STREAM-IO
	  FRAME frm-footer.

      
/* ******** Triggers  ***********/

ON 'tab':U OF wsOpt IN FRAME frm-main
    OR 'leave':U OF wsOpt IN FRAME frm-main
DO:
    ASSIGN wsOpt = LOGICAL(wsOpt:SCREEN-VALUE).
    IF wsOpt = YES THEN DO:
        DISABLE wsStart wsEnd WITH FRAME frm-main.
        ENABLE stBgrp enBgrp stGrp endGrp WITH FRAME frm-main.
    END.
    ELSE DO:
       ENABLE wsStart wsEnd WITH FRAME frm-main.
       DISABLE stBgrp enBgrp stGrp endGrp WITH FRAME frm-main.
    END.
    RETURN.
END.

ON 'choose':U OF stBgrp IN FRAME frm-main     
DO:
    wslabel = stBgrp:LABEL.
    VIEW FRAME frm-grp.
      open query qry-grp preselect 
                    each tblgroup NO-LOCK.
      ENABLE ALL WITH FRAME frm-Grp.
      WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-grp 
              OR close of THIS-PROCEDURE IN FRAME frm-grp 
              OR 'enter':u OF brw-grp
              OR 'mouse-select-dblclick' OF brw-grp.
      CLOSE QUERY qry-grp.
      HIDE FRAME frm-grp.
      APPLY 'tab' TO StBGrp.
      APPLY 'tab' TO StGrp.
      RETURN. 
END.

ON 'choose':U OF enBgrp IN FRAME frm-main     
DO:
    wslabel = enBgrp:LABEL.
    VIEW FRAME frm-grp.
      open query qry-grp preselect 
                    each tblgroup NO-LOCK.
      ENABLE ALL WITH FRAME frm-Grp.
      WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-grp 
              OR close of THIS-PROCEDURE IN FRAME frm-grp 
              OR 'enter':u OF brw-grp
              OR 'mouse-select-dblclick' OF brw-grp.
      CLOSE QUERY qry-grp.
      HIDE FRAME frm-grp.
      APPLY 'tab' TO EnBGrp.
      APPLY 'tab' TO endGrp.
      RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-grp 
    OR 'enter':u OF brw-grp 
    OR 'mouse-select-dblclick' OF brw-grp
DO: 
   GET CURRENT qry-grp  EXCLUSIVE-LOCK NO-WAIT.
   IF wslabel = "Start Group" THEN DO:
       DISPLAY tblGroup.intgrp @ stGrp WITH FRAME frm-main.
       APPLY 'tab' TO stGrp IN FRAME frm-main.
   END.
   ELSE IF wslabel = "End Group" THEN DO:
       endGrp:SCREEN-VALUE = STRING(tblGroup.intgrp).
       APPLY 'tab' TO endGrp IN FRAME frm-main.
   END.
   RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
   session:set-wait-state("").
   ASSIGN  wsStart = DEC(wsStart:SCREEN-VALUE IN FRAME frm-main)
           wsEnd   = DEC(wsEnd:SCREEN-VALUE)
           stGrp 
           endGrp
           wsAccDate 
           wsDueDate
            wsDueDate1
           wsPeriod
           wsZero =  LOGICAL(wsZero:SCREEN-VALUE).
           wsMessage1 = wsMessage1:SCREEN-VALUE IN FRAME frm-main.
           wsMessage2 = wsMessage2:SCREEN-VALUE IN FRAME frm-main.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"} 
  MESSAGE "Statement Print Completed" VIEW-AS ALERT-BOX.
  APPLY 'close' TO THIS-PROCEDURE.
  HIDE FRAME frm-main.
  RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
IF AVAILABLE simctr THEN
    ASSIGN  varRegNo   = SIMCTR.BPNO.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       stgrp:SCREEN-VALUE = "0"
       endGrp:SCREEN-VALUE = "999999"
       wsZero:SCREEN-VALUE = "NO"
       wsOpt:SCREEN-VALUE = "NO"
       wsPeriod:SCREEN-VALUE = STRING(simctr.curper).
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

 
PROCEDURE Report.ip:
IF wsOpt = YES THEN DO:
    FOR EACH dbcmf WHERE dbcmf.intgrp >= stGrp AND dbcmf.intgrp <= EndGrp NO-LOCK BY dbcmf.intGrp.
        PAUSE 0 BEFORE-HIDE.
        DISPLAY dbcmf.dbacc @ wssTatus WITH  NO-LABEL OVERLAY FRAME frm-main.
        IF  dbcmf.AccBal = 0 AND wsZero = NO  THEN NEXT.
            ASSIGN  varDue  = 0
                    wsAge   = 0
                    wsCr    = 0
                    j       = 0.
        DISPLAY STREAM a varRegNo dbcmf.dbAcc @ wsAcc wsDueDate wsAccDate   /*wsPeriod */ dbcmf.stno dbcmf.street
                dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 WITH FRAME frm-hdr.
        DOWN STREAM a WITH FRAME frm-hdr.
        wsage = 0.
        REPEAT:
            FIND NEXT dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE dbblf THEN DO:
                DO X = 1 TO 15:
                    ASSIGN wsage[1] = wsage[1] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] < 0
                           wsage[2] = wsage[2] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] > 0
                           wsage[3] = wsage[3] + dbblf.amt[X] WHEN X = 2
                           wsage[4] = wsage[4] + dbblf.amt[X] WHEN X = 3
                           wsage[5] = wsage[5] + dbblf.amt[X] WHEN X >= 4
                           wsage[6] = wsage[6] + dbblf.amt[X]. 
                END. 
            END.  
            IF NOT AVAILABLE dbblf THEN LEAVE.
        END.
        wsmeter = "".
        FIND FIRST dbmmf WHERE dbmmf.dbacc = dbcmf.dbacc AND dbmmf.mstat = 1  AND dbmmf.tarif <> 0 NO-ERROR.
        IF AVAILABLE dbmmf THEN DO:
           FIND FIRST dbtmf WHERE dbmmf.tarif = dbtmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
           IF AVAILABLE dbtmf THEN DO:
              FIND FIRST dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc AND dbmtf.ref  = dbmmf.serial NO-LOCK NO-ERROR. 
              IF AVAILABLE dbmtf THEN DO:
                 wsMeter = dbmtf.descrip + " " + STRING(dbmmf.READ[12]) + " - " + STRING(dbmmf.READ[11]).
              END.
           END.
        END.
        /*DISPLAY STREAM a  wsline WITH FRAME frm-hdr1.*/
        DOWN STREAM a WITH FRAME frm-hdr.
        DISPLAY STREAM a "BALANCE B/F " @ varDesc dbcmf.BalBf @ varAmt
                         WITH FRAME frm-trans.
        DOWN STREAM a WITH FRAME frm-trans.
        j = j + 1.
        varVat = 0.
        RUN tmpwork-ip.
        RUN rep-ip.
        j = 17 - j.   
        DOWN j STREAM a WITH FRAME frm-trans.
        DISPLAY STREAM a "VAT ON '*' ITEMS: " + string(varVat)  WHEN varVat <> 0 @ varDesc WITH FRAME frm-trans.
        DISPLAY STREAM a WITH FRAME frm-trans.
        DOWN STREAM a WITH FRAME frm-trans.
        DISPLAY STREAM a wsage[1] wsage[2] wsage[3] wsage[4] wsage[5] wsage[6] wsDueDate1
                      wsMessage1 wsMessage2  WITH FRAME frm-footer.
    
        
        FIND FIRST dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE dbblf THEN DO:
             varAmt = 0 /* dbblf.INT */.
              j = 8.
             DO X = 1 TO 15:
                  varAmt = varAmt + dbblf.amt[X].
             END.
             DOWN 1 STREAM a WITH FRAME frm-stub.
             DISPLAY STREAM a  /*dbblf.sgrp @ varSgrp varAmt*/ WITH FRAME frm-stub.
             DOWN STREAM a WITH FRAME frm-stub.
             DISPLAY STREAM a  dbcmf.dbacc @ varAcc WITH FRAME frm-stub.
            j = j - 1.
        END.
        REPEAT:
            FIND NEXT dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE dbblf THEN DO:
                varAmt = 0 /* dbblf.INT */.
                DO X = 1 TO 15:
                    varAmt = varAmt + dbblf.amt[X].
                END.
                IF varAmt <> 0 AND j > 1 THEN DO:
                    DISPLAY STREAM a /*dbblf.sgrp @ varSgrp varAmt*/ WITH FRAME frm-stub.
                    DOWN STREAM a WITH FRAME frm-stub.
                    j = j - 1.
                END. 
            END.
            IF NOT AVAILABLE dbblf THEN DO:
                IF j < 0 THEN
                    j = 1.
                DOWN j STREAM a WITH FRAME frm-stub.
                DISPLAY STREAM a wsAge[6] @ varAmt WITH FRAME frm-stub.
                DOWN 4 STREAM a WITH FRAME frm-stub.
                LEAVE.
            END.   
        END.
    END. /* eo--dbcmf */
END.
ELSE DO:
    FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd NO-LOCK.
        PAUSE 0 BEFORE-HIDE.
        DISPLAY dbcmf.dbacc @ wssTatus WITH  NO-LABEL OVERLAY FRAME frm-main.
        IF  dbcmf.AccBal = 0 /*AND wsZero = NO */ THEN NEXT.
            ASSIGN  varDue  = 0
                    wsAge   = 0
                    wsCr    = 0
                    j       = 0.
        DISPLAY STREAM a varRegNo dbcmf.dbAcc @ wsAcc wsDueDate wsAccDate   /*wsPeriod */ dbcmf.stno dbcmf.street
                dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 WITH FRAME frm-hdr.
        DOWN STREAM a WITH FRAME frm-hdr.
        wsage = 0.
        REPEAT:
            FIND NEXT dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE dbblf THEN DO:
                DO X = 1 TO 15:
                    ASSIGN wsage[1] = wsage[1] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] < 0
                           wsage[2] = wsage[2] + dbblf.amt[1] WHEN X = 1 AND dbblf.amt[1] > 0
                           wsage[3] = wsage[3] + dbblf.amt[X] WHEN X = 2
                           wsage[4] = wsage[4] + dbblf.amt[X] WHEN X = 3
                           wsage[5] = wsage[5] + dbblf.amt[X] WHEN X >= 4
                           wsage[6] = wsage[6] + dbblf.amt[X]. 
                END. 
            END.  
            IF NOT AVAILABLE dbblf THEN LEAVE.
        END.
        wsmeter = "".
        FIND FIRST dbmmf WHERE dbmmf.dbacc = dbcmf.dbacc AND dbmmf.mstat = 1  AND dbmmf.tarif <> 0 NO-ERROR.
        IF AVAILABLE dbmmf THEN DO:
           FIND FIRST dbtmf WHERE dbmmf.tarif = dbtmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
           IF AVAILABLE dbtmf THEN DO:
              FIND FIRST dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc AND dbmtf.ref  = dbmmf.serial NO-LOCK NO-ERROR. 
              IF AVAILABLE dbmtf THEN DO:
                 wsMeter = dbmtf.descrip + " " + STRING(dbmmf.READ[12]) + " - " + STRING(dbmmf.READ[11]).
              END.
           END.
        END.
        /*DISPLAY STREAM a  wsline WITH FRAME frm-hdr1.*/
        DOWN STREAM a WITH FRAME frm-hdr.
        DISPLAY STREAM a "BALANCE B/F " @ varDesc dbcmf.BalBf @ varAmt
                         WITH FRAME frm-trans.
        DOWN STREAM a WITH FRAME frm-trans.
        j = j + 1.
        varVat = 0.
        RUN tmpwork-ip.
        RUN rep-ip.
        j = 17 - j.   
        DOWN j STREAM a WITH FRAME frm-trans.
        DISPLAY STREAM a "VAT ON '*' ITEMS: " + string(varVat) WHEN varVat <> 0   @ varDesc WITH FRAME frm-trans.
        DISPLAY STREAM a WITH FRAME frm-trans.
        DOWN STREAM a WITH FRAME frm-trans.
        DISPLAY STREAM a wsage[1] wsage[2] wsage[3] wsage[4] wsage[5] wsage[6] wsDueDate1
                      wsMessage1 wsMessage2  WITH FRAME frm-footer.
    
        
        FIND FIRST dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE dbblf THEN DO:
             varAmt = 0 /* dbblf.INT */.
              j = 8.
             DO X = 1 TO 15:
                  varAmt = varAmt + dbblf.amt[X].
             END.
             DOWN 1 STREAM a WITH FRAME frm-stub.
             DISPLAY STREAM a  /*dbblf.sgrp @ varSgrp varAmt*/ WITH FRAME frm-stub.
             DOWN STREAM a WITH FRAME frm-stub.
             DISPLAY STREAM a  dbcmf.dbacc @ varAcc WITH FRAME frm-stub.
            j = j - 1.
        END.
        REPEAT:
            FIND NEXT dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE dbblf THEN DO:
                varAmt = 0 /* dbblf.INT */.
                DO X = 1 TO 15:
                    varAmt = varAmt + dbblf.amt[X].
                END.
                IF varAmt <> 0 AND j > 1 THEN DO:
                    DISPLAY STREAM a /*dbblf.sgrp @ varSgrp varAmt*/ WITH FRAME frm-stub.
                    DOWN STREAM a WITH FRAME frm-stub.
                    j = j - 1.
                END. 
            END.
            IF NOT AVAILABLE dbblf THEN DO:
                IF j < 0 THEN
                    j = 1.
                DOWN j STREAM a WITH FRAME frm-stub.
                DISPLAY STREAM a wsAge[6] @ varAmt WITH FRAME frm-stub.
                DOWN 4 STREAM a WITH FRAME frm-stub.
                LEAVE.
            END.   
        END.
    END. /* eo--dbcmf */
END.
RETURN.
END PROCEDURE.

PROCEDURE rep-ip:
    FIND FIRST tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
    IF AVAILABLE tmpwork THEN DO:
       varAmt = tmpwork.Amt.
       varVat = varVat + tmpwork.Vat.
       DISPLAY STREAM a tmpwork.trDate @ varDate tmpwork.ref @ varRef 
              upper(tmpwork.DESCRIP) @ varDesc varAmt varAmt  "*" WHEN tmpwork.vat <> 0 @ varVats  WITH FRAME frm-trans.
       DOWN STREAM a WITH FRAME frm-trans.
       j = j + 1.
    END.
    REPEAT:
        FIND NEXT tmpwork WHERE tmpwork.dbacc = dbcmf.dbacc NO-LOCK NO-ERROR.
        IF AVAILABLE tmpwork THEN DO:
            varAmt = tmpwork.Amt.
            varVat = varVat + tmpwork.Vat.
            DISPLAY STREAM a tmpwork.trDate @ varDate tmpwork.ref @ varRef 
                upper(tmpwork.DESCRIP) @ varDesc varAmt varAmt  "*" WHEN tmpwork.vat <> 0 @ varVats  WITH FRAME frm-trans.
           DOWN STREAM a WITH FRAME frm-trans.
           j = j + 1.
        END.   
        IF NOT AVAILABLE tmpwork THEN LEAVE.
    END.
END PROCEDURE.

PROCEDURE tmpwork-ip: /*consolidate Receipts and Journals */
    FOR EACH tmpwork.
      DELETE tmpwork.
    END.
    FOR EACH dbmtf WHERE dbmtf.dbacc = dbcmf.dbacc NO-LOCK:
        IF dbmtf.prog = "dbint.p" THEN DO: /* Interest */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbint.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Interest".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "dbjnl02.p" THEN DO: /* Journals */
           FIND FIRST tmpwork WHERE tmpwork.prog = "dbjnl02.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Adjusting Journals".
           END.
           ELSE tmpwork.amt = tmpwork.amt + dbmtf.amt.
        END.
        ELSE IF dbmtf.prog = "recupd.p" THEN DO: /* Receipts */
           FIND FIRST tmpwork WHERE tmpwork.prog = "recupd.p" NO-ERROR.
           IF NOT AVAILABLE tmpwork THEN DO:
               CREATE tmpwork.
               BUFFER-COPY dbmtf TO tmpwork.
               ASSIGN tmpwork.ref = ""
                      tmpwork.descrip = "Receipts".
           END.
           ELSE ASSIGN tmpwork.amt = tmpwork.amt + dbmtf.amt
                       /* tmpwork.descrip = wsmeter WHEN dbmtf.ref  = dbmmf.serial*/.
        END.
        ELSE DO:
           CREATE tmpwork.
           BUFFER-COPY dbmtf TO tmpwork.
        END.
    END.
END PROCEDURE
