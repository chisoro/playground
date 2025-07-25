session:DATA-ENTRY-RETURN = TRUE.
/* Program.................GLMF02.p
   Notes:.................Chart of Accounts Listing
   Author:.................S. Mawire
*/

&SCOPED-DEFINE pgorientation "PORTRAIT"

SESSION:DATA-ENTRY-RETURN = TRUE.
DEF BUFFER bfrglmf FOR glmf.

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsStatus LIKE gltdf.acct.
DEF VAR wsTitle0  AS CHAR FORM "X(80)".
DEF VAR st-proj LIKE glmf.proj.
DEF VAR end-proj LIKE glmf.proj.
DEF VAR st-fund LIKE glmf.fund.
DEF VAR end-fund LIKE glmf.fund.
DEF VAR wsDescrip AS CHAR FORM "X(80)".
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Consolidated ", 1, "By Project", 2, "By Fund", 3.
DEF VAR X AS INT.
DEF VAR prnCat AS LOGICAL.
DEF VAR wsProg AS CHAR.
DEF VAR wsTitle  AS CHAR FORM "X(70)" INITIAL "CHART OF ACCOUNT MASTERFILE LISTING".

DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btnSubCat  LABEL "Surplus/Deficit SubCat".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 16.5.

DEF    QUERY qry-Picksubcat FOR glsubcat SCROLLING.
DEF BROWSE brw-Picksubcat QUERY qry-Picksubcat
    DISPLAY GLsubCAT.subCAT GLsubCAT.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Picksubcat 
    brw-Picksubcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btnclose colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Sub-Category Selection".

DEFINE FRAME frm-main
    SKIP(2)
    wsOpt    LABEL  "Selection Report Option" COLON 30 AUTO-RETURN SKIP(1)
    st-proj   COLON 30 LABEL  "FROM Project" SKIP(0.5)
    end-proj  COLON 30 LABEL  "TO Project" SKIP(0.5)
    st-fund   COLON 30 LABEL  "FROM Fund" SKIP(0.5)
    end-fund   COLON 30 LABEL  "TO Fund" SKIP(3.5)
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(2.2)
    btn-ok AT ROW 18.6 COL 15
    btnclose colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         SIDE-LABELS no-underline three-d SCROLLABLE CENTERED
    TITLE "CHART OF ACCOUNT LISTING REPORT".

FORM
    glmf.DESCRIPTION LABEL "DESCRIPTION"
    glmf.acct        LABEL "ACCOUNT"
    glmf.CAT         LABEL "CAT"
    glmf.subcat      LABEL "SUB-CAT"
    glmf.Dept        LABEL "DEPT"
    HEADER skip(2) wsTitle0 AT 20 SKIP(3) wsTitle AT 5 SPACE(2) "Page: " AT 65 TRIM(STRING(PAGE-NUMBER(a)))
    SKIP(2)
     WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132  FRAME frm-rpt.

ON 'mouse-select-click' OF wsOpt IN FRAME frm-main
DO:
    ASSIGN wsopt.
    IF wsOpt = 1 THEN DO:
        DISABLE st-proj end-proj st-fund end-fund WITH FRAME frm-main.
        wsProg = "Rep1.ip".
    END.
    IF wsOpt = 2 THEN DO:
        ENABLE st-proj end-proj WITH FRAME frm-main.
         DISABLE st-fund end-fund WITH FRAME frm-main.
         wsProg = "Rep2.ip".
    END.
    IF wsOpt = 3 THEN DO:
       DISABLE st-proj end-proj WITH FRAME frm-main.
        ENABLE st-fund end-fund  WITH FRAME frm-main.
         wsProg = "Rep3.ip".
    END.
    APPLY 'tab' TO SELF.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsopt.
    IF wsOpt = 1 THEN DO:
        DISABLE st-proj end-proj st-fund end-fund WITH FRAME frm-main.
        wsProg = "Rep1.ip".
    END.
     ASSIGN st-proj = INT(st-proj:SCREEN-VALUE)
            end-proj = INT(end-proj:SCREEN-VALUE)
            st-fund = INT(st-fund:SCREEN-VALUE)
            end-fund = INT(end-fund:SCREEN-VALUE).

    {PrintOpt.i &stream-name="stream a"
                    &print-prog=VALUE(wsProg)
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN end-proj:SCREEN-VALUE = "99"
       st-proj:SCREEN-VALUE = "00"
       end-fund:SCREEN-VALUE = "99"
       st-fund :SCREEN-VALUE = "00"
       wsTitle0 = simctr.CONAME.    
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Rep1.ip:
    wsTitle = "CONSOLIDATED CHART OF ACCOUNT MASTERFILE LISTING".
    FOR EACH glmf NO-LOCK BREAK BY CLAS BY cat BY subcat:
        IF FIRST-OF(clas) THEN DO:
            FIND FIRST glclass WHERE glclass.clas = glmf.clas NO-LOCK NO-ERROR.
            DISPLAY STREAM a glclass.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(cat) THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-LOCK NO-ERROR.
            IF NOT AVAILABLE glcat THEN
                MESSAGE "Category " glmf.cat " not available" VIEW-AS ALERT-BOX.
            DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(subcat) THEN DO:
            FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-LOCK NO-ERROR.
            DISPLAY STREAM a  "      " + glsubcat.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a "        " + glmf.DESCRIPTION @ glmf.DESCRIPTION glmf.acct glmf.CAT glmf.subca glmf.Dept WITH FRAM frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        IF LAST-OF(glmf.clas) THEN
            DOWN 2 STREAM a WITH FRAME frm-rpt.
    END.
END.

PROCEDURE Rep2.ip:
    wsTitle = "CHART OF ACCOUNT MASTERFILE LISTING BY PROJECT".
    FOR EACH glmf WHERE glmf.proj >= st-proj AND glmf.proj <= end-proj NO-LOCK BREAK BY proj BY Clas BY cat BY dept :
        IF FIRST-OF(proj) THEN DO:
            FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
            DISPLAY STREAM a glproj.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(clas) THEN DO:
            FIND FIRST glclass WHERE glclass.clas = glmf.clas NO-LOCK NO-ERROR.
            DISPLAY STREAM a glclass.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(cat) THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-LOCK NO-ERROR.
            IF NOT AVAILABLE glcat THEN
                MESSAGE "Category " glmf.cat " not available" VIEW-AS ALERT-BOX.
            DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a "        " + glmf.DESCRIPTION @ glmf.DESCRIPTION glmf.acct glmf.CAT glmf.subca glmf.Dept WITH FRAM frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
         IF LAST-OF(glmf.clas) THEN
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        IF LAST-OF(glmf.proj) THEN
            PAGE STREAM a.
    END.
END.

PROCEDURE Rep3.ip:
    wsTitle = "CHART OF ACCOUNT MASTERFILE LISTING BY SEGMENT/FUND".
    FOR EACH glmf WHERE glmf.fund >= st-fund AND glmf.fund <= end-fund NO-LOCK BREAK BY fund BY Clas BY cat BY dept :
        IF FIRST-OF(fund) THEN DO:
            FIND FIRST glfund WHERE glfund.fund = glmf.fund NO-LOCK NO-ERROR.
            DISPLAY STREAM a glfund.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(clas) THEN DO:
            FIND FIRST glclass WHERE glclass.clas = glmf.clas NO-LOCK NO-ERROR.
            DISPLAY STREAM a glclass.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF FIRST-OF(cat) THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-LOCK NO-ERROR.
            IF NOT AVAILABLE glcat THEN
                MESSAGE "Category " glmf.cat " not available" VIEW-AS ALERT-BOX.
            DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAM frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        DISPLAY STREAM a "        " + glmf.DESCRIPTION @ glmf.DESCRIPTION glmf.acct glmf.CAT glmf.subca glmf.Dept WITH FRAM frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        
        IF LAST-OF(glmf.fund) THEN
            PAGE STREAM a.
    END.
END.
