/* Program.................glrpt04.p
   Notes:...... Project Based Collection and expenditure Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsAmt LIKE glbal.AMT.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsStatus LIKE glbal.acct.
DEF VAR wsYear   LIKE GLBAL.YEAR.
DEF VAR wsMonth AS INT FORM "99".
DEF VAR st-proj LIKE glmf.fund.
DEF VAR end-proj LIKE glmf.fund.
DEF VAR st-Acc LIKE glmf.Acct.
DEF VAR end-Acc LIKE glmf.Acct.
DEF VAR varFund LIKE GLFUND.DESCRIP.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEF TEMP-TABLE tmptdf
    FIELD wacct LIKE glmf.acct
    FIELD wproj LIKE glmf.proj
    FIELD wclas LIKE glmf.clas
    FIELD wcat  LIKE glmf.cat
    FIELD wsub  LIKE glmf.subcat
    FIELD wAmt  AS DEC FORM "zzz,zzz,zz9.99-".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 8.5.

DEFINE FRAME frm-main
    SKIP(2)
    st-per    COLON 30 LABEL "Accounting Period" SKIP(.5)
    st-proj   COLON 30 LABEL "START Project" SKIP(.5)
    end-proj  COLON 30 LABEL "END Project" SKIP(.5)
    wsStatus  COLON 20 LABEL "Processing......" view-as text no-tab-stop
    skip(2.5)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PROJECT BASEED COLLECTION AND EXPENSE REPORT".

FORM
    glmf.DESCRIPTION AT 5 
    tmptdf.wAmt FORM "zzz,zzz,zz9.99-"
    HEADER skip(1) ""  AT 5 "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP
     "PROJECT BASEED COLLECTION AND EXPENSE REPORT FOR THE PERIOD" AT 5 
      STRING(st-Per) SKIP(1) WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmRpt.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN st-per   = INT(st-per:SCREEN-VALUE)
            st-proj  = INT(st-proj:SCREEN-VALUE)
            end-proj = INT(end-proj:SCREEN-VALUE)
            wsYear   = INT(SUBSTR(STRING(st-per),1,4)).
            wsMonth   = INT(SUBSTR(STRING(st-per),5,2)).
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged}
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER).
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip:
FOR EACH gltdf WHERE gltdf.period = 202001 AND 
    (gltdf.SOURCE = "RE" OR gltdf.SOURCE = "CB" OR gltdf.SOURCE = "CR"):
    IF CAN-FIND(FIRST cbkmf WHERE cbkmf.Ledger = gltdf.acct) THEN NEXT.
    IF (gltdf.acct = simctr.vat[1] OR gltdf.acct = simctr.vat[2]) /*exclude vat movements */ 
        AND SOURCE = "RE" THEN NEXT.
    IF (gltdf.acct = simctr.vat[3] OR gltdf.acct = simctr.vat[4]) /*exclude vat movements */
        AND SOURCE = "CR" THEN NEXT.
    FIND FIRST tmptdf WHERE tmptdf.wacct = gltdf.acct AND tmptdf.wproj = gltdf.proj
        NO-ERROR.
    IF NOT AVAILABLE tmptdf THEN DO:
        FIND FIRST glmf WHERE glmf.acct = gltdf.acct NO-ERROR.
        /* IF glmf.ACCTYPE <> "IE" THEN NEXT. */
        CREATE tmptdf.
        ASSIGN tmptdf.wacct = gltdf.acct
               tmptdf.wproj = gltdf.proj
               tmptdf.wclas = 1 WHEN SOURCE = "RE" AND gltdf.amt < 0
               tmptdf.wclas = 2 WHEN SOURCE <> "RE"
               tmptdf.wcat  = glmf.cat
               tmptdf.wsub  = glmf.subcat.
    END.
    tmptdf.wAmt = tmptdf.wAmt + gltdf.amt.
END.
FOR EACH tmptdf NO-LOCK BREAK BY wproj BY wclas BY wcat:
        IF FIRST-OF(wproj) THEN DO:
            FIND FIRST glproj WHERE glproj.proj = wproj NO-LOCK NO-ERROR.
            DISPLAY STREAM a glproj.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        IF FIRST-OF(wclas) THEN DO:
            FIND FIRST glclass WHERE glclass.clas = wclas NO-LOCK NO-ERROR.
            DISPLAY STREAM a "   " + glclass.DESCRIPTION @ glmf.DESCRIPTION WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        IF FIRST-OF(wcat) THEN DO:
            FIND FIRST glcat WHERE glcat.cat = wcat NO-LOCK NO-ERROR.
            DISPLAY STREAM a "      " + glcat.descrip @ glmf.DESCRIPTION WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
        END.
        FIND FIRST glmf WHERE glmf.acct = wacct NO-LOCK NO-ERROR.
        ACCUMULATE wAmt (SUB-TOTAL BY wcat).
        ACCUMULATE wAmt (SUB-TOTAL BY wclas).
        ACCUMULATE wAmt (TOTAL BY wproj).
        DISPLAY STREAM a "          " + glmf.DESCRIPTION @ glmf.DESCRIPTION wamt WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
        IF LAST-OF(wCat) THEN DO:
            UNDERLINE STREAM a wAmt WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
            DISPLAY STREAM a "      SUB-TOTAL" @ glmf.DESCRIPTION ACCUM SUB-TOTAL BY wcat (wAmt) @ wAmt WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.
        IF LAST-OF(wClas) THEN DO:
            FIND FIRST glclass WHERE glclass.clas = wclas NO-LOCK NO-ERROR.
            DOWN STREAM a WITH FRAME frmRpt.
            DISPLAY STREAM a "   TOTAL: " + glclass.DESCRIPTION @ glmf.DESCRIPTION ACCUM SUB-TOTAL BY wclas (wAmt) @ wAmt WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
        END.
        IF LAST-OF(wproj) THEN DO:
            FIND FIRST glproj WHERE glproj.proj = wproj NO-LOCK NO-ERROR.
            DOWN STREAM a WITH FRAME frmRpt.
            DISPLAY STREAM a "NET OF PROJECT" @ glmf.DESCRIPTION ACCUM TOTAL BY wproj (wAmt) @ wAmt WITH FRAME frmRpt.
             DOWN STREAM a WITH FRAME frmRpt.
             PAGE STREAM a.
        END.
END.
END PROCEDURE.

