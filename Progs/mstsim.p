DEFINE VARIABLE objOutlook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookMsg AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookAttach AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookRecip AS COM-HANDLE NO-UNDO.
DEFINE STREAM b.
DEF VAR wsCharge AS DEC.
DEF VAR wsAmt AS DEC EXTENT 2.
DEF VAR ws AS CHAR EXTENT 2.
DEF VAR X AS INT.
DEF VAR wsrec AS CHAR FORM "x(80)".
DEF VAR wsa LIKE dbcmf.dbacc.
DEF BUFFER tt FOR dbcmf.
DEF VAR wsa1 AS CHAR FORM "x(10)".
DEF VAR rfil AS CHAR FORM "x(8)".
DEF VAR i AS INT.
DEF VAR wstype AS INT.
DEF VAR wsvalue AS CHAR FORM "X(80)".
DEF VAR wsStatus LIKE dbcmf.dbacc.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wsstatus   LABEL "Processing......." COLON 30 VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 23.5
    TITLE "PRE-PAID METER DATA TRANSFER" VIEW-AS DIALOG-BOX.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.

ON 'CHOOSE':U OF btn-ok IN FRAME frm-main 
DO:
    RUN Exctract-ip.
    RETURN.
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE  Exctract-ip:
OUTPUT STREAM b TO z:\REPORTS\MSTSim01.txt.
PUT STREAM b 'ACCOUNT NUMBER|ARREARS BALANCE|CURRENT BALANCE' SKIP.
FOR EACH dbcmf WHERE SUBSTR(deedno,1,2) = "PM":
   DISPLAY dbcmf.dbacc @ wsstatus WITH FRAM frm-main.
   PAUSE 0.
   wsa1 = SUBSTR(dbcmf.deedno,4,6).
   wsamt = 0.
   i = LENGTH(dbcmf.deedno) - 1.
   rfil = SUBSTR(dbcmf.deedno,i,-1).
   IF rfil = "/2" THEN DO:
      i = i - 4.
      wstype = 2.
      wsa = DEC(SUBSTR(dbcmf.deedno,4,i)).
   END.
   ELSE 
       ASSIGN wsa    = DEC(SUBSTR(dbcmf.deedno,4))
              wstype = 0.
    /* services Data for main account */
    DO X = 1 TO 14: /* Services */
       IF dbcmf.tarif[X] <> 0 AND dbcmf.units[X] <> 0 THEN DO:       
          FIND FIRST dbtmf WHERE dbtmf.TYPE = 3 AND dbtmf.tarif = dbcmf.tarif[X] NO-LOCK NO-ERROR.
          IF AVAILABLE dbtmf THEN DO:
             IF dbtmf.VCharge = NO THEN
                wsamt[1] = wsamt[1] + (dbtmf.Charge * dbcmf.units[X])
                        + (dbtmf.Charge * dbcmf.units[X]) * (dbtmf.vat% / 100). 
             ELSE IF VCharge = YES THEN DO:
                  FIND FIRST dbvTar WHERE  dbvTar.dbAcc = dbcmf.dbacc AND dbvTar.Tarif = dbcmf.tarif[x] AND dbvTar.type = 3 NO-LOCK NO-ERROR.
                 IF AVAILABLE dbvTar THEN
                     wsamt[1] = wsamt[1] + (dbvTar.Charge * dbcmf.units[X])
                             + ROUND ((dbvTar.Charge * dbcmf.units[X]) * (dbtmf.vat% / 100),2 ).
             END. 
          END.
       END. /*1*/
    END. /* do x.... */     
   /* Rates main account */
    IF dbcmf.Rate-tarif > 0 THEN DO:
       wsCharge = 0.
       IF dbcmf.BldValue > 0 or dbcmf.SiteValue > 0 then do:
          FIND LAST dbtmf WHERE dbtmf.type = 2
                            AND dbtmf.tarif = dbcmf.rate-tarif NO-LOCK no-error.
          ASSIGN wsCharge = round(dbtmf.BValue * dbcmf.BldValue,2)
                          + round(dbtmf.LValue * dbcmf.SiteValue,2). 
       END.
       wsAmt[1] = wsAmt[1] + wsCharge.
    END.
    /* Get aged values */
    FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.sgrp <> wstype:
       ASSIGN wsAmt[2] = wsAmt[2] + dbblf.dbamt[1].
    END.
    IF wsa1 <> "" THEN /* additional Account */
        RUN add-account.
    IF wsAmt[2] < 0 THEN
       ASSIGN wsAmt[1] = ROUND((wsAmt[1] + wsAmt[2]),2)
              wsAmt[2] = 0.00.
    IF wsAmt[1] < 0 THEN
        wsAmt[1] = 0.00.
    DO X = 1 TO 2:
       ws[X] = STRING(substring(string(wsAmt[X]),index(string(wsAmt[X]),".") + 1,2)).
       IF index(string(wsAmt[X]),".") = 0 THEN
          ws[X] = "00".
       IF LENGTH(ws[X]) < 2 THEN
          ws[X] = ws[X] + "0".
    END.
    wsrec = STRING(dbcmf.dbacc) + "|"
          + string(substring(string(wsAmt[2]),1,index(string(wsAmt[2]),".") - 1))
          + "," + ws[2] + "|"
          + string(substring(string(wsAmt[1]),1,index(string(wsAmt[1]),".") - 1))
          + "," + ws[1].
    PUT STREAM b wsrec SKIP.
END.
OUTPUT STREAM b CLOSE.
    /* send email to entries in incparm.incvalue for MST */
INPUT FROM z:\bin\mstsim.d.
 REPEAT:
        IMPORT wsvalue.
        CREATE "Outlook.Application" objOutlook.
        objoutlookMsg = objOutlook:CreateItem(0).
        objOutlookRecip = objOutlookMsg:Recipients:Add(wsvalue).
        objOutlookRecip:Type = 1.
        objOutlookMsg:Subject = "Kariba File Extract".
        objOutlookMsg:Body = "Please find attached the file extract for Kariba".
        
        objOutlookMsg:Attachments:Add("z:\results\MSTSim01.TXT").
        objOutlookRecip:Resolve.
        objOutlookMsg:Send.
        objoutlook:Quit().
        
        RELEASE OBJECT objOutlook.
        RELEASE OBJECT objOutlookMsg.
        RELEASE OBJECT objOutlookRecip. 
END.
MESSAGE "Procedure run completed....." VIEW-AS ALERT-BOX.
APPLY 'close' TO THIS-PROCEDURE.
RETURN.
END PROCEDURE.

PROCEDURE Add-Account:
   FIND FIRST tt WHERE tt.dbacc =  wsa NO-LOCK NO-ERROR.
   IF AVAILABLE tt THEN DO:
      DO X = 1 TO 14: /* Services */
         IF tt.tarif[X] <> 0 AND tt.units[X] <> 0 THEN DO:       
            FIND FIRST dbtmf WHERE dbtmf.TYPE = 3 AND dbtmf.tarif = tt.tarif[X] NO-LOCK NO-ERROR.
            IF AVAILABLE dbtmf THEN DO:
               IF dbtmf.VCharge = NO THEN
                  wsamt[1] = wsamt[1] + (dbtmf.Charge * tt.units[X])
                           + (dbtmf.Charge * tt.units[X]) * (dbtmf.vat% / 100). 
               ELSE IF VCharge = YES THEN DO:
                    FIND FIRST dbvTar WHERE  dbvTar.dbAcc = tt.dbacc AND dbvTar.Tarif = tt.tarif[x] AND dbvTar.type = 3 NO-LOCK NO-ERROR.
                    IF AVAILABLE dbvTar THEN
                       wsamt[1] = wsamt[1] + (dbvTar.Charge * tt.units[X])
                               + ROUND ((dbvTar.Charge * tt.units[X]) * (dbtmf.vat% / 100),2 ).
               END. 
            END.
         END. /*1*/
      END. /*2*/
      /* Rates */
      IF tt.Rate-tarif > 0 THEN DO:
         wsCharge = 0.
         IF tt.BldValue > 0 or tt.SiteValue > 0 THEN DO:
            FIND LAST dbtmf WHERE dbtmf.type = 2
                              AND dbtmf.tarif = tt.rate-tarif NO-LOCK NO-ERROR.
            ASSIGN wsCharge = round(dbtmf.BValue * tt.BldValue,2)
                            + round(dbtmf.LValue * tt.SiteValue,2). 
         END.
         wsAmt[1] = wsAmt[1] + wsCharge.
      END.
      FOR EACH dbblf WHERE dbblf.dbacc = wsa NO-LOCK:
          ASSIGN wsAmt[2] = wsAmt[2] + dbblf.dbamt[1].
      END.
   END.
END PROCEDURE.


