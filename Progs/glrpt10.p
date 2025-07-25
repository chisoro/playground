/* Program.................glrpt10.p
   Notes:...... Statement of Financial Performance
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
DEF VAR st-fund LIKE glmf.fund.
DEF VAR end-fund LIKE glmf.fund.
DEF VAR st-Acc LIKE glmf.Acct.
DEF VAR end-Acc LIKE glmf.Acct.
DEF VAR varFund LIKE GLFUND.DESCRIP.

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
    st-per    COLON 30 LABEL "Accounting Period" SKIP(.5)
    st-fund   COLON 30 LABEL "START Fund" SKIP(.5)
    end-fund  COLON 30 LABEL "END Fund" SKIP(.5)
    st-acc    COLON 30 LABEL "START Account" SKIP(.5)
    end-acc   COLON 30 LABEL "END Account" SKIP(1.5)
    wsStatus  COLON 20 LABEL "Processing......" view-as text no-tab-stop
    skip(3.0)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STATEMENT OF FINANCIAL PERFORMANCE".
FORM
    glmf.DESCRIPTION AT 5 glbal.Amt[1] FORM "zzz,zzz,zz9.99-"
    HEADER skip(1) varFund AT 5 "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP
     "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED" AT 5 
      STRING(st-Per) SKIP(1) WITH DOWN STREAM-IO 
                         FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME a.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN st-per   = INT(st-per:SCREEN-VALUE)
            st-fund  = INT(st-fund:SCREEN-VALUE)
            end-fund = INT(end-fund:SCREEN-VALUE)
            st-Acc   = INT(st-Acc:SCREEN-VALUE)
            end-Acc  = INT(end-Acc:SCREEN-VALUE).
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
FOR EACH glmf /* WHERE glmf.cat <= 9 */ NO-LOCK , 
    EACH glbal OF glmf  WHERE glbal.YEAR = wsYear no-lock 
                                BREAK BY glmf.CAT BY glmf.subcat:
    DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main. PAUSE 0.
    ACCUMULATE glbal.AMT[1] (TOTAL BY glmf.CAT BY glmf.subcat ) FORM "zzz,zzz,zz9.99-".
    IF FIRST-OF(glmf.cat) THEN DO:
       FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
       DISPLAY STREAM a glcat.descrip @ glmf.DESCRIPTION WITH FRAME a.
       DOWN 1 STREAM a WITH FRAME a.
    END.
    IF LAST-OF(glmf.subcat) THEN DO:
        DOWN STREAM a WITH FRAME a.
       FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-LOCK NO-ERROR.
       DISPLAY STREAM a "   " + GLSUBCAT.DESCRIP @ glmf.DESCRIPTION 
           (ACCUM TOTAL BY glmf.subcat glbal.Amt[1])FORM "zzz,zzz,zz9.99-" WITH FRAME a.
       DOWN STREAM a WITH FRAME a.
    END.   
    IF LAST-OF(glmf.cat) THEN DO:
        UNDERLINE STREAM a glbal.Amt[1]  WITH FRAME a.
        DISPLAY STREAM a "TOTAL" @ glmf.DESCRIPTION
            (ACCUM TOTAL BY glmf.cat glbal.Amt[1])FORM "zzz,zzz,zz9.99-" WITH FRAME a.
        UNDERLINE glbal.Amt[1] WITH FRAME a.
        /*PAGE STREAM a.*/
    END.
END.
APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
