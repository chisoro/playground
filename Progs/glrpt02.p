/* Program.................glrpt02.p
   Notes:...... Income & Expenditure Management Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsAmt LIKE gltdf.AMT.
DEF VAR wsTAmt LIKE gltdf.AMT.
DEF VAR wsBudg LIKE gltdf.AMT.
DEF VAR wsBTotal LIKE gltdf.AMT EXTENT 4.
DEF VAR wsVar AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsVar% AS DEC FORM "zzz,zz9.99-".
DEF VAR wsCom AS CHAR .
DEF VAR X AS INT.
DEF VAR st-per LIKE gltdf.period.
DEF VAR END-per LIKE gltdf.period.
DEF VAR wsStatus LIKE gltdf.acct.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 8.5.

DEFINE FRAME frm-main
    SKIP(2)
    st-per    COLON 30 LABEL "START Accounting Period" SKIP(.5)
    end-per   COLON 30 LABEL "END Accounting Period" SKIP(1.5)
    /*wsStatus  COLON 20 LABEL "Processing......" view-as text no-tab-stop */
    skip(3.5)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 14
    TITLE "INCOME AND EXPENDITURE MANAGEMENT REPORT" VIEW-AS DIALOG-BOX.

FORM
    glmf.acct  LABEL "ACCOUNT"
    glmf.DESCRIPTION  LABEL "DESCRIPTION" 
    wsAmt LABEL "AMOUNT"
    wsBudg LABEL "BUDGET"
    wsVar  LABEL "VARIANCE"
    wsVar%  LABEL "VARIANCE*"
    wsCom   NO-LABEL
    HEADER skip(4) "INCOME AND EXPENDITURE MANAGEMENT REPORT FOR THE PERIOD:" AT 5
    TRIM(STRING(st-Per)) "TO" TRIM(STRING(end-Per)) "Page:" AT 82 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frm-rpt.

ON CHOOSE OF btn-oK  DO:
     session:set-wait-state("").
     ASSIGN st-per end-per .
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged}
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       end-per:SCREEN-VALUE = STRING(SIMCTR.CURPER).
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip:
    ASSIGN wsTAmt = 0
           wsBtotal = 0.
FOR EACH glmf WHERE glmf.ACCTYPE = "IE" NO-LOCK , 
    EACH gltdf OF glmf  WHERE gltdf.period >= st-per AND gltdf.period <= end-per no-lock 
                                BREAK BY glmf.dept  BY glmf.clas BY glmf.acct:
    wsTAmt = wsTAmt + gltdf.Amt.
    /* DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main. */
    ACCUMULATE gltdf.AMT (TOTAL BY glmf.dept BY glmf.clas BY glmf.acct ).
    IF FIRST-OF(glmf.dept) THEN DO:
       FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-ERROR.
       /*DOWN 1 STREAM a WITH FRAME frm-rpt.*/
       DISPLAY STREAM a gldept.descrip @ glmf.DESCRIPTION WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
    END.
    IF FIRST-OF(glmf.clas) THEN DO:
         wsBTotal[1] = 0.
        DOWN STREAM a WITH FRAME frm-rpt.
       FIND FIRST glclass WHERE glclass.clas = glmf.clas NO-LOCK NO-ERROR.
       DISPLAY STREAM a " " + GLclass.DESCRIP @ glmf.DESCRIPTION WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
    END.
    IF LAST-OF(glmf.acct) THEN DO:
        IF (ACCUM TOTAL BY glmf.acct gltdf.Amt) <> 0 THEN DO:
            ASSIGN wsAmt = (ACCUM TOTAL BY glmf.acct gltdf.Amt)
                   wsBudg = 0
                   wsCom = ""
                   wsVar  = 0
                   wsvar%   = 0.
            FIND FIRST glDbal WHERE glDbal.acct = glmf.acct AND gldbal.Dept = glmf.dept
                AND GLDBal.YEAR = INT(SUBSTR(STRING(end-per),1,4)) NO-ERROR.
            DO X = INT(SUBSTR(STRING(st-per),5,2)) TO INT(SUBSTR(STRING(end-per),5,2)):
                wsBudg = wsBudg + glDbal.budget[X]. 
            END.

            IF  wsBudg <> 0 THEN DO:
                ASSIGN wsVar = wsBudg - wsAmt
                       wsVar% = ROUND(((wsVar / wsBudg) * 100),2)
                       wsBTotal[1] = wsBtotal[1] + wsBudg
                       wsBTotal[2] = wsBtotal[2] + wsBudg 
                       wsBTotal[3] = wsBtotal[3] + wsBudg.
                IF wsAmt < 0 AND wsBudg < 0 THEN
                    ASSIGN wsCom  = "A" WHEN wsAmt > wsBudg.
                ELSE IF wsAmt > 0 AND wsBudg > 0 THEN
                    ASSIGN wscom = "A" WHEN wsAmt > WsBudg.
            END. 
            ELSE wsVar% = 0.
            DISPLAY STREAM a glmf.acct "   " + glmf.DESCRIPTION  @ glmf.DESCRIPTION 
             wsAmt wsBudg wsVar wsVar% wsCom WITH FRAME frm-rpt.
        END.
    END.
        
    DOWN STREAM a WITH FRAME frm-rpt.  
    IF LAST-OF(glmf.clas) THEN DO:
        /*wsAmt = (ACCUM TOTAL BY glmf.clas gltdf.Amt) */
        IF  wsBTotal[1] <> 0 THEN DO:
                ASSIGN wsAmt = (ACCUM TOTAL BY glmf.Clas gltdf.Amt)
                       wsVar = wsBTotal[1] - wsAmt 
                       wsVar% = ROUND(((wsVar / wsBTotal[1]) * 100),2).
                IF wsAmt < 0 AND wsBTotal[1] < 0 THEN
                    ASSIGN wsCom  = "A" WHEN wsAmt > wsBTotal[1].
                ELSE IF wsAmt > 0 AND wsBTotal[1] > 0 THEN
                    ASSIGN wscom = "A" WHEN wsAmt > wsBTotal[1].
            END.
       FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-LOCK NO-ERROR.
       UNDERLINE STREAM a wsAmt WITH FRAME frm-rpt.
       DISPLAY STREAM a " SUB_TOTAL"  @ glmf.DESCRIPTION 
              wsAmt wsBtotal[1] @ wsBudg wsVar wsVar% wsCom WITH FRAME frm-rpt.
       DOWN 2 STREAM a  WITH FRAME frm-rpt.
    END.     
    IF LAST-OF(glmf.dept) THEN DO:
        /*wsAmt = (ACCUM TOTAL BY glmf.clas gltdf.Amt) */
        IF  wsBTotal[2] <> 0 THEN DO:
                ASSIGN wsAmt = (ACCUM TOTAL BY glmf.dept gltdf.Amt)
                       wsVar = wsBTotal[2] - wsAmt .
                       wsVar% = ROUND(((wsVar / wsBTotal[2]) * 100),2).
                IF wsAmt < 0 AND wsBTotal[2] < 0 THEN
                    ASSIGN wsCom  = "A" WHEN wsAmt > wsBTotal[2].
                ELSE IF wsAmt > 0 AND wsBTotal[2] > 0 THEN
                    ASSIGN wscom = "A" WHEN wsAmt > wsBTotal[2].
            END.
        UNDERLINE STREAM a wsAmt WITH FRAME frm-rpt.
        DISPLAY STREAM a "DEPARTMENT SURPLUS)/DEFICIT" @ glmf.DESCRIPTION
            wsAmt wsBTotal[2] @ wsBudg wsVar wsVar% wsCom WITH FRAME frm-rpt.
        UNDERLINE STREAM a wsAmt WITH FRAME frm-rpt.
        wsBTotal[2] = 0.
       /* PAGE STREAM a.*/
    END.
END.
DOWN /*1 */ STREAM a  WITH FRAME frm-rpt.
DISPLAY STREAM a "(GRAND SURPLUS)/DEFICIT" @ glmf.DESCRIPTION wsTAmt @ wsAmt wsBTotal[3] @ wsBudg WITH FRAME frm-rpt.
APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
