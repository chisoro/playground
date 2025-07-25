DEF STREAM a.
OUTPUT STREAM a TO g:\simacc\test.txt.

DEF VAR wsDesc AS CHAR FORM "x(35)".
DEF VAR wsServ AS CHAR FORM "x(35)".
DEF VAR wsTar  AS CHAR FORM "x(35)".
DEF VAR wsAmt  LIKE dbhtf.amt.
DEF VAR wsTAmt LIKE wsAmt.
DEF VAR wsCnt  AS INT.
DEF VAR wsTCnt LIKE wsCnt.
DEF VAR wsHeader AS CHAR FORM "X(60)".

DEF TEMP-TABLE tbAna
    FIELD tbward LIKE dbcmf.ward
    FIELD tbcons LIKE dbcmf.cons
    FIELD tbsgrp LIKE dbhtf.sgrp
    FIELD tbtarif LIKE dbhtf.tarif
    FIELD tbcnt  AS INT
    FIELD tbAmt LIKE dbhtf.amt.

FORM SPACE(10)
    wsDesc NO-LABEL
    /*wsServ LABEL "Service"
    wsTar  LABEL "Tariff" */
    tbAmt  LABEL "Amount"
    tbCnt  LABEL "Accounts"
    HEADER skip(4) wsHeader AT 15
    "Page:" AT 65 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frmrpt.


FOR EACH dbhtf WHERE Accper >= 201912 AND Accper <= 201912 AND prog = "dbsg07.p"
    NO-LOCK:
    FIND FIRST dbcmf WHERE dbcmf.dbacc = dbhtf.dbacc NO-LOCK NO-ERROR.
    FIND FIRST tbAna WHERE tbWard = dbcmf.ward AND tbCons = dbcmf.Cons
        AND tbSgrp = sgrp AND tbTarif = dbhtf.tarif NO-ERROR.
    IF NOT AVAILABLE tbAna THEN DO:
        CREATE tbAna.
        ASSIGN tbWard = dbcmf.ward 
               tbCons = dbcmf.Cons
               tbSgrp = sgrp 
               tbTarif = dbhtf.tarif.
    END.
    ASSIGN tbCnt = tbCnt + 1
           tbAmt = tbAmt + dbhtf.amt.
END.
/*
/*Analysis by Ward by Service */
assign wsHeader =  "TARIFF BILLING ANALYSIS BY WARD BY SERVICE:" 
         wsTAmt = 0
         wsTCnt = 0.
FOR EACH dbwrd NO-LOCK:
    ASSIGN wsAmt = 0
           wsCnt = 0.
    IF CAN-FIND (FIRST tbAna WHERE tbWard = dbwrd.ward) THEN DO:
        wsdesc = dbwrd.Descrip.
        DISPLAY STREAM a wsDesc WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    FOR EACH tbAna WHERE tbward = dbwrd.ward NO-LOCK BREAK BY tbSgrp BY tbTarif :
        ACCUMULATE tbAmt (SUB-TOTAL BY tbTarif).
        ACCUMULATE tbCnt (SUB-TOTAL BY tbTarif).
        ACCUMULATE tbAmt (SUB-TOTAL BY tbSgrp).
        ACCUMULATE tbCnt (SUB-TOTAL BY tbSgrp).
        ASSIGN wsAmt = wsAmt + tbAmt
               wsCnt = wsCnt + tbCnt
               wsTAmt = wsTAmt + tbAmt
               wsTCnt = wsTCnt + tbCnt.
        IF FIRST-OF(tbSgrp) THEN DO:
           FIND FIRST dbsgr WHERE dbsgr.Sgrp = tbSgrp NO-LOCK NO-ERROR.
           wsdesc = "  " + dbsgr.Descrip.
           DISPLAY STREAM a wsdesc WITH FRAME frmRpt.
           DOWN STREAM a WITH FRAME frmRpt.
        END.
        IF LAST-OF(tbTarif) THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.Tarif = tbTarif AND dbtmf.Sgrp = tbSgrp
               NO-LOCK NO-ERROR.
           wsdesc = "     " + dbtmf.Descrip.
           DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbTarif tbAmt @ tbAmt 
                                   ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
           DOWN STREAM a WITH FRAME frmRpt.
       END.
       IF LAST-OF(tbSgrp) THEN DO:
            wsdesc = "  Service Total".
            UNDERLINE STREAM A tbAmt tbCnt WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbSgrp tbAmt @ tbAmt 
                                   ACCUM SUB-TOTAL BY tbSgrp tbCnt @ tbCnt WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
       END.
    END.
    IF wsAmt <> 0 THEN DO:
        wsdesc = "Ward Total".
        UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
        DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsCnt @ tbCnt WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END.  
END.
IF wsTAmt <> 0 THEN DO:
    wsdesc = "Grand Total".
    UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
    DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTCnt @ tbCnt WITH FRAME frmRpt.
    PAGE STREAM a.
END.

/*Analysis by Consumer by Service */
ASSIGN wsHeader =  "BILLING ANALYSIS BY CONSUMER BY SERVICE:"
       wsTAmt = 0
       wsTCnt = 0 .
FOR EACH dbctf NO-LOCK:
    ASSIGN wsAmt = 0
           wsCnt = 0.
    IF CAN-FIND (FIRST tbAna WHERE tbCons = dbctf.Cons) THEN DO:
        wsdesc = dbctf.Descrip.
        DISPLAY STREAM a wsDesc WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    FOR EACH tbAna WHERE tbCons = dbctf.Cons NO-LOCK BREAK BY tbSgrp BY tbTarif :
        ACCUMULATE tbAmt (SUB-TOTAL BY tbTarif).
        ACCUMULATE tbCnt (SUB-TOTAL BY tbTarif).
        ACCUMULATE tbAmt (SUB-TOTAL BY tbSgrp).
        ACCUMULATE tbCnt (SUB-TOTAL BY tbSgrp).
        ASSIGN wsAmt = wsAmt + tbAmt
               wsCnt = wsCnt + tbCnt
               wsTAmt = wsTAmt + tbAmt
               wsTCnt = wsTCnt + tbCnt.
        IF FIRST-OF(tbSgrp) THEN DO:
           FIND FIRST dbsgr WHERE dbsgr.Sgrp = tbSgrp NO-LOCK NO-ERROR.
           wsdesc = "  " + dbsgr.Descrip.
           DISPLAY STREAM a wsdesc WITH FRAME frmRpt.
           DOWN STREAM a WITH FRAME frmRpt.
        END.
        IF LAST-OF(tbTarif) THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.Tarif = tbTarif AND dbtmf.Sgrp = tbSgrp NO-LOCK NO-ERROR.
           wsdesc = "     " + dbtmf.Descrip.
           DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbTarif tbAmt @ tbAmt 
                                   ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
           DOWN STREAM a WITH FRAME frmRpt.
       END.
       IF LAST-OF(tbSgrp) THEN DO:
            wsdesc = "  Service Total".
            UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbSgrp tbAmt @ tbAmt 
                                   ACCUM SUB-TOTAL BY tbSgrp tbCnt @ tbCnt WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
       END.
    END.
    IF wsAmt <> 0 THEN DO:
        wsdesc = "Consumer Total".
        UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
        DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsCnt @ tbCnt WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END.  
END.
IF wsTAmt <> 0 THEN DO:
    wsdesc = "Grand Total".
    UNDERLINE  STREAM a tbAmt tbCnt WITH FRAME frmRpt.
    DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTCnt @ tbCnt WITH FRAME frmRpt.
    PAGE STREAM a.
END.
*/

/*Analysis by Ward by Service */
assign wsHeader =  "TARIFF BILLING ANALYSIS BY WARD" 
         wsTAmt = 0
         wsTCnt = 0.
FOR EACH dbwrd NO-LOCK:
    ASSIGN wsAmt = 0
           wsCnt = 0.
    IF CAN-FIND (FIRST tbAna WHERE tbWard = dbwrd.ward) THEN DO:
        wsdesc = dbwrd.Descrip.
        DISPLAY STREAM a wsDesc WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    FOR EACH tbAna WHERE tbward = dbwrd.ward NO-LOCK BREAK BY tbTarif :
        ACCUMULATE tbAmt (SUB-TOTAL BY tbTarif).
        ACCUMULATE tbCnt (SUB-TOTAL BY tbTarif).
        ASSIGN wsAmt = wsAmt + tbAmt
               wsCnt = wsCnt + tbCnt
               wsTAmt = wsTAmt + tbAmt
               wsTCnt = wsTCnt + tbCnt.
        IF LAST-OF(tbTarif) THEN DO:
           FIND FIRST dbtmf WHERE dbtmf.Tarif = tbTarif AND dbtmf.Sgrp = tbSgrp
               NO-LOCK NO-ERROR.
           wsdesc = "     " + dbtmf.Descrip.
           DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbTarif tbAmt @ tbAmt 
                                   ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
           DOWN STREAM a WITH FRAME frmRpt.
       END.
    END.
    IF wsAmt <> 0 THEN DO:
        wsdesc = "Ward Total".
        UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
        DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsCnt @ tbCnt WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END.  
END.
IF wsTAmt <> 0 THEN DO:
    wsdesc = "Grand Total".
    UNDERLINE STREAM a tbAmt tbCnt WITH FRAME frmRpt.
    DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTCnt @ tbCnt WITH FRAME frmRpt.
    PAGE STREAM a.
END.
