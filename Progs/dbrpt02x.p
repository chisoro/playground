/* Program.................dbrpt02x.p
   Notes:................. Property Breakdown Debit Raising Summary Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR st-per  LIKE dbhtf.Accper.
DEF VAR end-per  LIKE dbhtf.Accper.
DEF VAR X AS INT.
DEF  VAR wsFilter LIKE dbblf.sgrp.
DEF VAR wsStatus AS CHAR.
DEF VAR wsDesc AS CHAR FORM "x(35)".
DEF VAR wsServ AS CHAR FORM "x(35)".
DEF VAR wsTar  AS CHAR FORM "x(35)".
DEF VAR wsLedger LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsVari LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsAmt  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsBal  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsRec LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR balcd LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsColl LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTAmt LIKE wsAmt.
DEF VAR wsTLAmt LIKE wsAmt.
DEF VAR wsVat  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTVat LIKE wsAmt.
DEF VAR wsCnt  AS INT.
DEF VAR wsTCnt LIKE wsCnt.
DEF VAR wsHeader AS CHAR FORM "X(100)".
DEF VAR varDate LIKE gltdf.trDate.
DEF VAR  varCur LIKE simctr.dbCur.
DEF VAR rdbRate LIKE tblForex.decRate.
DEF VAR wsLAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTBalcd LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsBalbd  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-". 
DEF VAR wsTBalbd LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-". 
DEF VAR tdeduct LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-". 
DEF VAR deduct LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsBalcd LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsRecAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsjnlAmt  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTjnlAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR  wsTRecAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".

DEF TEMP-TABLE tbAna
    FIELD tbward LIKE dbcmf.ward
    FIELD tbcons LIKE dbcmf.cons
    FIELD tbsgrp LIKE dbhtf.sgrp
    FIELD tbtarif LIKE dbhtf.tarif
    FIELD tbcnt  AS INT
    FIELD tbRate LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-"
    FIELD tbVat% LIKE dbtmf.vat%
    FIELD tbAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-"
    FIELD tbVat LIKE dbhtf.vat FORM "zzz,zzz,zz9.99-"
    FIELD tbRecAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-"
    FIELD tbRecVat LIKE dbhtf.vat FORM "zzz,zzz,zz9.99-"
    FIELD tbjnlAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-"
    FIELD tbjnlVat LIKE dbhtf.vat FORM "zzz,zzz,zz9.99-".





DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

FORM SPACE(10)
    wsDesc NO-LABEL FORM "x(20)"
    wsLedger LABEL "AS PER LEDGER"
    tbCnt  LABEL "NUMBER OF ACCOUNTS"
    tbRate LABEL "RATE PER UNIT"
    tbAmt  LABEL "TOTAL AMOUNT"
    tbVat  LABEL "TOTAL VAT"
    wsVari LABEL "VARIANCE"
    HEADER SKIP(2) wsTitle AT 30
           SKIP(2) wsHeader AT 15
    "Page:" AT 75 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frmrpt.

FORM SPACE(10)
    wsDesc NO-LABEL FORM "x(20)"
    wsBal LABEL "BALANCE b/d"
    tbAmt  LABEL "BILLING TOTAL"
    tbJnlAmt  LABEL "JOURNAL TOTAL"
    wsRec LABEL "RECEIPTS"
    balcd LABEL "BALANCE c/d"
    wsColl LABEL "% COLLECTION"
    HEADER SKIP(2) wsTitle AT 30
           SKIP(2) wsHeader AT 15
    "Page:" AT 75 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frmrpt1.

DEF FRAME frm-main
    SKIP(2.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(1.5)
    st-per      LABEL "PERIOD" COLON 30 
    /*end-per     LABEL "END Period"  SKIP(0.5)*/
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "DEBIT RAISING SUMMARY REPORT" VIEW-AS DIALOG-BOX.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   FIND FIRST simctr NO-LOCK NO-ERROR.
   varCur = simctr.dbCur.
   ASSIGN wsStart wsEnd st-per.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
MESSAGE "This will not produce good results for periods not billed with this system" VIEW-AS ALERT-BOX.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       st-Per:SCREEN-VALUE = STRING(simctr.curper)
       /*st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-Per:SCREEN-VALUE = string(SIMCTR.CURPER)*/
       wsTitle = SIMCTR.CONAME.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
   

     FOR EACH dbhtf WHERE  accper = INT(st-per:SCREEN-VALUE IN FRAME frm-main)  NO-LOCK:
             FIND FIRST dbcmf WHERE dbcmf.dbacc = dbhtf.dbacc NO-LOCK NO-ERROR.
            FIND FIRST tbAna WHERE  tbCons = dbcmf.Cons AND tbSgrp = dbhtf.sgrp AND tbTarif = dbhtf.tarif AND tbWard = dbcmf.ward NO-ERROR.
            
                IF NOT AVAILABLE tbAna THEN DO:
                    CREATE tbAna.
                    ASSIGN tbWard = dbcmf.ward 
                           tbCons = dbcmf.Cons
                           tbSgrp = dbhtf.sgrp
                           tbTarif = dbhtf.tarif.
                END.
                IF dbhtf.prog = "dbsg07.p" THEN DO:
                        FIND FIRST dbtmf WHERE dbtmf.sgrp = dbhtf.sgrp AND dbtmf.Tarif = dbhtf.tarif NO-LOCK NO-ERROR.
                        IF AVAILABLE dbtmf THEN   
                            ASSIGN tbRate =  dbtmf.charge
                                   tbVat% = dbtmf.vat%.
                    
                        ASSIGN tbCnt = tbCnt + 1
                               tbAmt = tbAmt + dbhtf.amt
                               tbVat = tbVat + dbhtf.vat.
                    END.
                ELSE IF dbhtf.prog = "recupd.p" THEN DO:
                        ASSIGN 
                               tbRecAmt = tbRecAmt + (dbhtf.amt * -1)
                               tbRecVat = tbRecVat + (dbhtf.vat * -1). 
                END.
                ELSE DO:
                
                    ASSIGN tbjnlAmt = tbjnlAmt + dbhtf.amt
                           tbjnlVat = tbjnlVat + dbhtf.vat.
                 END.
     END.
   

   


    /*Analysis by Service by Tarif*/
    assign wsHeader = varCur + " PROPERTY TARIFF BILLING ANALYSIS BY SERIVICE FOR PERIOD " + STRING(st-per) 
             wsTAmt = 0
             wsTCnt = 0
             wsTVat = 0
            wsTLamt = 0.
    FOR EACH dbsgr NO-LOCK:
        ASSIGN wsAmt = 0
               wsCnt = 0
               wsVat = 0.
        IF CAN-FIND (FIRST tbAna WHERE tbSgrp = dbsgr.sgrp AND tbTarif <> 0) THEN DO:
            wsdesc = dbsgr.Descrip.
            DISPLAY STREAM a wsDesc WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        FOR EACH tbAna WHERE tbAna.tbSgrp = dbsgr.sgrp  AND tbTarif <> 0 NO-LOCK BREAK BY tbTarif :
            ACCUMULATE tbAmt (SUB-TOTAL BY tbTarif).
            ACCUMULATE tbVat (SUB-TOTAL BY tbTarif).
            ACCUMULATE tbCnt (SUB-TOTAL BY tbTarif).
            ASSIGN wsAmt = wsAmt + tbAmt
                   wsCnt = wsCnt + tbCnt
                   wsTAmt = wsTAmt + tbAmt
                   wsTCnt = wsTCnt + tbCnt
                   wsVat = wsVat + tbVat
                   wsTVat = wsTVat + tbVat.
            IF LAST-OF(tbTarif) THEN DO:
               FIND FIRST dbtmf WHERE dbtmf.Tarif = tbTarif AND dbtmf.Sgrp = tbAna.tbSgrp
                   NO-LOCK NO-ERROR.
               wsdesc = "     " + dbtmf.Descrip.
               DISPLAY STREAM a wsdesc dbtmf.charge @ tbRate 
                                       ACCUM SUB-TOTAL BY tbTarif tbAmt @ tbAmt
                                       ACCUM SUB-TOTAL BY tbTarif tbVat @ tbVat
                                       ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
               DOWN STREAM a WITH FRAME frmRpt.
           END.
        END.

        
       
         wsLAmt = 0.
          FOR EACH  gltdf WHERE  gltdf.acct = dbsgr.ctrLedger AND  gltdf.period = st-per AND  gltdf.Ref = "dbsg07.p" NO-LOCK.
                ASSIGN
                    varDate = gltdf.trDate.
               
               FIND FIRST tblForex WHERE tblForex.txtCur = "USD" AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
                IF AVAILABLE tblForex THEN
                      ASSIGN rdbRate = tblForex.decRate.
                ELSE IF NOT AVAILABLE tblForex  THEN DO:
                   FIND LAST tblForexH WHERE tblForexH.txtCur = "USD" AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
                   IF AVAILABLE tblForexH THEN
                       ASSIGN rdbRate = tblForexH.decRate.
                    ELSE IF NOT AVAILABLE tblForexH  THEN DO:
                        FIND FIRST tblForexH WHERE  tblForexH.txtCur = "USD" AND tblForexH.dtRate >= varDate NO-LOCK NO-ERROR.
                        IF AVAILABLE tblForexH THEN 
                        ASSIGN rdbRate = tblForexH.decRate.
                   
                   END.
                END.
            
          wsLAmt = wsLAmt  + (gltdf.amt / rdbRate).
         
        END. 
          wsTLAmt = wsTLAmt + wsLAmt.

                IF wsAmt <> 0  AND wsLAmt <> 0 THEN DO:
                    wsdesc = "Service Total".
                    UNDERLINE STREAM a  wsLedger wsVari wsVari tbAmt tbVat WITH FRAME frmRpt.
                    DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsVat @ tbVat wsLAmt @ wsLedger (wsLAmt - wsAmt) @ wsVari   WITH FRAME frmRpt.
                    DOWN 1 STREAM a WITH FRAME frmRpt.
                END.  
        END.
        IF wsTAmt <> 0  AND wsTLAmt <> 0 THEN DO:
            wsdesc = "Grand Total".
            UNDERLINE STREAM a wsLedger tbAmt tbVat  WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTVat @ tbVat wsTLAmt @ wsLedger (wsTLAmt - wsTAmt) @ wsVari  WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        PAGE STREAM a.


       /*Analysis by Service by Ward  */
    assign wsHeader =  varCur + " BILLING ANALYSIS BY WARD FOR PERIOD " + STRING(st-per) 
             wsTAmt = 0
             wsTjnlAmt = 0
             wsTRecAmt = 0
             wsTBalbd = 0
             wsTBalcd = 0.
    FOR EACH dbsgr NO-LOCK:
        ASSIGN wsAmt = 0
               wsRecAmt = 0
               wsjnlAmt = 0
               wsBalbd = 0
               wsBalcd = 0.
        IF CAN-FIND (FIRST tbAna WHERE tbSgrp = dbsgr.sgrp) THEN DO:
            wsdesc = dbsgr.Descrip.
            DISPLAY STREAM a wsDesc WITH FRAME frmRpt1.
            DOWN STREAM a WITH FRAME frmRpt1.
        END.
        FOR EACH tbAna WHERE tbAna.tbSgrp = dbsgr.sgrp NO-LOCK BREAK BY tbAna.tbWard /*BY tbAna.tbSgrp*/ :
            ACCUMULATE tbRecAmt (SUB-TOTAL BY tbAna.tbWard).
            ACCUMULATE tbjnlAmt (SUB-TOTAL BY tbAna.tbWard).
            ACCUMULATE tbAmt (SUB-TOTAL BY tbAna.tbWard).
            ASSIGN wsAmt = wsAmt + tbAmt
                   wsTAmt = wsTAmt + tbAmt
                   wsjnlAmt = wsjnlAmt + tbjnlAmt
                   wsTjnlAmt = wsTjnlAmt + tbjnlAmt
                   wsRecAmt = wsRecAmt + tbRecAmt
                   wsTRecAmt = wsTRecAmt + tbRecAmt.
            IF LAST-OF(tbAna.tbWard) THEN DO:
           
                FIND FIRST dbwrd WHERE dbwrd.ward = tbAna.tbWard
                   NO-LOCK NO-ERROR.
               IF AVAILABLE dbWrd THEN DO:
                    wsdesc = "     " + dbwrd.Descrip.
               END.
                
               IF NOT AVAILABLE dbWrd THEN DO:
                   wsdesc = "     WARD 0 (UNSPECIFIED)".
               END.
                 
               DISPLAY STREAM a wsdesc  
                                       ACCUM SUB-TOTAL BY tbAna.tbWard tbAmt @ tbAmt
                                       ACCUM SUB-TOTAL BY tbAna.tbWard tbjnlAmt @ tbjnlAmt
                                       ACCUM SUB-TOTAL BY tbAna.tbWard tbRecAmt @ wsRec 
                                        WITH FRAME frmRpt1.
               DOWN STREAM a WITH FRAME frmRpt1.
           END.
       
        END.


       FOR EACH  dbblf WHERE dbblf.sgrp = dbsgr.sgrp NO-LOCK.
            wsBalbd = wsBalbd + dbblf.dbamt[1].
            
        END.
        
        FOR EACH  dbhtf WHERE dbhtf.sgrp = dbsgr.sgrp AND dbhtf.accper > INT(st-per:SCREEN-VALUE IN FRAME frm-main)  NO-LOCK.
                 deduct = deduct + dbhtf.amt.
                 
        END.


        IF (wsAmt <> 0 OR wsjnlAmt <> 0 OR wsRecAmt <> 0) THEN DO: 
            tdeduct = tdeduct + deduct.
            wsBalbd = wsBalbd - deduct - wsAmt - wsjnlAmt + wsRecAmt.
            wsBalcd = wsBalbd + wsAmt + wsjnlAmt - wsRecAmt.
            wsTBalbd = wsTBalbd + wsBalbd.
            wsTBalcd = wsTBalcd + wsBalcd.
            wsColl = (wsRecAmt / (wsBalbd + wsAmt + wsjnlAmt)) * 100.
            wsdesc = "Service Total".
            UNDERLINE STREAM a wsBal tbAmt tbjnlAmt wsRec balcd  wsColl WITH FRAME frmRpt1.
            DISPLAY STREAM a wsdesc wsBalbd @ wsBal wsAmt @ tbAmt wsjnlAmt @ tbjnlAmt  wsRecAmt @ wsRec wsBalcd @ balcd wsColl WITH FRAME frmRpt1.
            DOWN 1 STREAM a WITH FRAME frmRpt1.
        END. 
        END.
        IF (wsTAmt <> 0 OR wsTjnlAmt <> 0 OR wsTRecAmt <> 0) THEN DO:
            wsColl = (wsTRecAmt / (wsTBalbd + wsTAmt + wsTjnlAmt)) * 100.
            wsdesc = "Grand Total".
            UNDERLINE STREAM a wsBal tbAmt tbjnlAmt wsRec balcd wsColl WITH FRAME frmRpt1.
            DISPLAY STREAM a wsdesc  wsTBalbd @ wsBal wsTAmt @ tbAmt wsTjnlAmt @ tbjnlAmt  wsTRecAmt @ wsRec wsTBalcd @ balcd wsColl   WITH FRAME frmRpt1.
            DOWN STREAM a WITH FRAME frmRpt1.
        END.
        PAGE STREAM a.

        ASSIGN 
            wsBalbd = 0
            wsTBalbd = 0
            wsAmt = 0
            wsTAmt = 0
            wsTCnt = 0
            wsCnt = 0
            wsVat = 0
            wsTVat = 0
            wsTLamt = 0
            wsLAmt = 0
            tdeduct = 0
            deduct = 0
            wsjnlAmt = 0
            wsTjnlAmt = 0
            wsRecAmt = 0 
            wsTRecAmt = 0 
            wsBalcd = 0
            wsTBalcd = 0.
    
END.
   
