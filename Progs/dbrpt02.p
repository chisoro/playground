/* Program.................dbrpt02.p
   Notes:................. Debit Raising Summary Report
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
DEF VAR wsAmt  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTAmt LIKE wsAmt.
DEF VAR wsVat  LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTVat LIKE wsAmt.
DEF VAR wsCnt  AS INT.
DEF VAR wsTCnt LIKE wsCnt.
DEF VAR wsHeader AS CHAR FORM "X(60)".

DEF TEMP-TABLE tbAna
    FIELD tbward LIKE dbcmf.ward
    FIELD tbcons LIKE dbcmf.cons
    FIELD tbsgrp LIKE dbhtf.sgrp
    FIELD tbtarif LIKE dbhtf.tarif
    FIELD tbcnt  AS INT
    FIELD tbAmt LIKE dbhtf.amt FORM "zzz,zzz,zz9.99-"
    FIELD tbVat LIKE dbhtf.vat FORM "zzz,zzz,zz9.99-".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

FORM SPACE(10)
    wsDesc NO-LABEL
    tbAmt  LABEL "AMOUNT"
    tbVat  LABEL "VAT"
    tbCnt  LABEL "ACCOUNTS"
    HEADER SKIP(2) wsTitle AT 30
           SKIP(2) wsHeader AT 15
    "Page:" AT 75 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frmrpt.

DEF FRAME frm-main
    SKIP(2.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(1.5)
    st-per      LABEL "START Period" COLON 30 SPACE(1)
    end-per     LABEL "END Period"  SKIP(0.5)
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
   ASSIGN wsStart wsEnd  st-per end-per.
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
       st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-Per:SCREEN-VALUE = string(SIMCTR.CURPER)
       wsTitle = SIMCTR.CONAME.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
    /* Create Summary Data */
    FOR EACH dbhtf WHERE Accper >= st-per AND Accper <= end-per AND prog = "dbsg07.p" NO-LOCK:
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
               tbAmt = tbAmt + dbhtf.amt
               tbVat = tbVat + dbhtf.vat.
    END.

    /*Analysis by Ward by Service */
    assign wsHeader =  "TARIFF BILLING ANALYSIS BY WARD" 
             wsTAmt = 0
             wsTCnt = 0
             wsTVat = 0.
    FOR EACH dbwrd NO-LOCK:
        ASSIGN wsAmt = 0
               wsCnt = 0
               wsVat = 0.
        IF CAN-FIND (FIRST tbAna WHERE tbWard = dbwrd.ward) THEN DO:
            wsdesc = dbwrd.Descrip.
            DISPLAY STREAM a wsDesc WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        FOR EACH tbAna WHERE tbward = dbwrd.ward NO-LOCK BREAK BY tbTarif :
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
               FIND FIRST dbtmf WHERE dbtmf.Tarif = tbTarif AND dbtmf.Sgrp = tbSgrp
                   NO-LOCK NO-ERROR.
               wsdesc = "     " + dbtmf.Descrip.
               DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbTarif tbAmt @ tbAmt
                                       ACCUM SUB-TOTAL BY tbTarif tbVat @ tbVat
                                       ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
               DOWN STREAM a WITH FRAME frmRpt.
           END.
        END.
        IF wsAmt <> 0 THEN DO:
            wsdesc = "Ward Total".
            UNDERLINE STREAM a tbAmt tbVat WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsVat @ tbVat /*wsCnt @ tbCnt  */ WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.  
        END.
        IF wsTAmt <> 0 THEN DO:
            wsdesc = "Grand Total".
            UNDERLINE STREAM a tbAmt tbVat  WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTVat @ tbVat /*wsTCnt @ tbCnt */ WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        PAGE STREAM a.

    /*Analysis by Ward by Service */
    assign wsHeader =  "TARIFF BILLING ANALYSIS BY WARD BY SERVICE:" 
             wsTAmt = 0
             wsTCnt = 0
             wsTVat = 0.
    FOR EACH dbwrd NO-LOCK:
        ASSIGN wsAmt = 0
               wsCnt = 0
               wsVat = 0.
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
            ACCUMULATE tbVat (SUB-TOTAL BY tbTarif).
            ACCUMULATE tbVat (SUB-TOTAL BY tbSgrp).
            ASSIGN wsVat = wsVat + tbVat
                   wsAmt = wsAmt + tbAmt
                   wsCnt = wsCnt + tbCnt
                   wsTAmt = wsTAmt + tbAmt
                   wsTCnt = wsTCnt + tbCnt
                   wsTvat = wsTVat + tbVat.
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
                                       ACCUM SUB-TOTAL BY tbTarif tbVat @ tbVat
                                       ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt WITH FRAME frmRpt.
               DOWN STREAM a WITH FRAME frmRpt.
           END.
           IF LAST-OF(tbSgrp) THEN DO:
                wsdesc = "  Service Total".
                UNDERLINE STREAM A tbAmt tbVat WITH FRAME frmRpt.
                DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbSgrp tbAmt @ tbAmt 
                                       ACCUM SUB-TOTAL BY tbSgrp tbVat @ tbVat
                                       /*ACCUM SUB-TOTAL BY tbSgrp tbCnt @ tbCnt */ WITH FRAME frmRpt.
                DOWN 2 STREAM a WITH FRAME frmRpt.
           END.
        END.
        IF wsAmt <> 0 THEN DO:
            wsdesc = "Ward Total".
            UNDERLINE STREAM a tbAmt tbVat WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsVat @ tbVat /*wsCnt @ tbCnt */ WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.  
    END.
    IF wsTAmt <> 0 THEN DO:
        wsdesc = "Grand Total".
        UNDERLINE STREAM a tbAmt tbVat WITH FRAME frmRpt.
        DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTVat @ tbVat /*wsTCnt @ tbCnt */ WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    PAGE STREAM a.
    
    /*Analysis by Consumer by Service */
    ASSIGN wsHeader =  "BILLING ANALYSIS BY CONSUMER BY SERVICE:"
           wsTAmt = 0
           wsTCnt = 0
           wsTVat = 0 .
    FOR EACH dbctf NO-LOCK:
        ASSIGN wsAmt = 0
               wsCnt = 0
               wsVat = 0.
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
            ACCUMULATE tbVat (SUB-TOTAL BY tbTarif).
            ACCUMULATE tbVat (SUB-TOTAL BY tbSgrp).
            ASSIGN wsAmt = wsAmt + tbAmt
                   wsCnt = wsCnt + tbCnt
                   wsTAmt = wsTAmt + tbAmt
                   wsTCnt = wsTCnt + tbCnt
                   wsVat = wsVat + tbVat
                   wsTVat = wsTVat + tbVat.
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
                                       ACCUM SUB-TOTAL BY tbTarif tbVat @ tbVat
                                       ACCUM SUB-TOTAL BY tbTarif tbCnt @ tbCnt  WITH FRAME frmRpt.
               DOWN STREAM a WITH FRAME frmRpt.
           END.
           IF LAST-OF(tbSgrp) THEN DO:
                wsdesc = "  Service Total".
                UNDERLINE STREAM a tbAmt tbVat WITH FRAME frmRpt.
                DISPLAY STREAM a wsdesc ACCUM SUB-TOTAL BY tbSgrp tbAmt @ tbAmt
                                       ACCUM SUB-TOTAL BY tbSgrp tbVat @ tbVat
                                       /*ACCUM SUB-TOTAL BY tbSgrp tbCnt @ tbCnt */ WITH FRAME frmRpt.
                DOWN 2 STREAM a WITH FRAME frmRpt.
           END.
        END.
        IF wsAmt <> 0 THEN DO:
            wsdesc = "Consumer Total".
            UNDERLINE STREAM a tbAmt tbVat WITH FRAME frmRpt.
            DISPLAY STREAM a wsdesc wsAmt @ tbAmt wsVat @ tbVat /*wsCnt @ tbCnt */ WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.  
    END.
    IF wsTAmt <> 0 THEN DO:
        wsdesc = "Grand Total".
        UNDERLINE  STREAM a tbAmt tbVat WITH FRAME frmRpt.
        DISPLAY STREAM a wsdesc wsTAmt @ tbAmt wsTVat @ tbVat /*wsTCnt @ tbCnt  */WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    PAGE STREAM a.
END.
   
