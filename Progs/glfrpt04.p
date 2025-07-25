/* Program.................glfrpt04.p
   Notes:...... Statement of Changes in Net Asset/Equity Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsStatus LIKE gltdf.acct.
DEF VAR ws AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 3.
DEF VAR wst LIKE ws EXTENT 3.
DEF VAR wsTitle  AS CHAR FORM "X(80)".
DEF VAR wsTitle0  AS CHAR FORM "X(80)".
DEF VAR  wsc     LIKE glmf.cat.
DEF VAR wsYear AS INT FORM "9999" .
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsper LIKE SIMCTR.CURPER.
DEF VAR wssurp  LIKE glmf.acct.
DEF VAR wsDescrip AS CHAR FORM "X(60)".
DEF VAR X AS INT.
DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btnSubCat  LABEL "Surplus/Deficit SubCat".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 10.5.

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

FORM glmf.DESCRIPTION NO-LABEL 
     ws[1] LABEL "RESERVES"
     ws[2] LABEL "SURPLUS/DEFICIT"
     ws[3] LABEL "TOTAL"
    HEADER SKIP(1) wsTitle0 AT 20 SKIP(1) wsTitle AT 20 skip(2) "STATEMENT OF CHANGES IN NET ASSET/EQUITY FOR THE YEAR ENDED:" AT 5
    TRIM(STRING(wsPer)) "Page:" AT 89 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frm-rpt.

DEFINE FRAME frm-main
    SKIP(2)
    wsper     COLON 30 LABEL "Reporting Accounting Period" SKIP(.5)
    btnSubCat COLON 8 no-tab-stop wssurp NO-LABEL glsubCat.descrip NO-LABEL SKIP(1.5) 
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(5.0)
    btn-ok colon 15
    btnclose colon 70
    rect-2 AT ROW 1.4 COL 2
    rect-1 AT ROW 12 COL 2
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STATEMENT OF CHANGES IN NET ASSET/EQUITY REPORT".

ON CHOOSE OF btnSubCat IN FRAME frm-main
DO:
  VIEW FRAME frm-picksubCat.
  OPEN QUERY qry-picksubCat FOR EACH glSubCat NO-LOCK.
  ENABLE ALL WITH FRAME frm-picksubCat.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-picksubCat 
          OR close of THIS-PROCEDURE IN FRAME frm-picksubCat
          OR CHOOSE OF btn-ok IN FRAME frm-picksubCat 
          OR 'enter':u OF brw-picksubCat
          OR 'mouse-select-dblclick' OF brw-picksubCat.
  CLOSE QUERY qry-picksubCat.
  HIDE FRAME frm-picksubCat.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-picksubCat 
    OR 'enter':u OF brw-picksubCat
    OR 'mouse-select-dblclick' OF brw-picksubCat
DO: 
   GET CURRENT qry-picksubCat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glsubCat.SubCat @ wsSurp WITH FRAME frm-main.
   ASSIGN wsSurp.
   APPLY 'tab' TO btnSubCat IN FRAME frm-main.
   RETURN.
END.

ON 'enter':U OF  wsSurp IN FRAME frm-main 
    OR 'tab':U OF  wsSurp IN FRAME frm-main 
DO:
    FIND FIRST glsubCat WHERE glsubCat.SubCat = int( wsSurp:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glsubCat THEN DO:
        DISPLAY glsubCat.descrip WITH FRAME frm-main.
      APPLY 'tab' TO SELF.
      ASSIGN wsSurp.
    END.
    ELSE IF NOT AVAILABLE glsubCat THEN DO:
        MESSAGE "Invalid sub-Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     IF  INT(wsper:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
         MESSAGE "ENTERED period cannot be greater than CURRENT period" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     ASSIGN wsper = INT(wsper:SCREEN-VALUE)
            wsYear = INT(SUBSTR(wsper:SCREEN-VALUE,1,4))
            wsMonth = INT(SUBSTR(wsper:SCREEN-VALUE,5,2)).
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsper:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsTitle0 = simctr.coname.  
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip:
    FOR EACH glmf WHERE clas = 5 BY subcat:
        FOR EACH glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = wsYear:
            ASSIGN ws[1] = ws[1] + glbal.bfbal WHEN glmf.subcat <> wsSurp
                   ws[2] = ws[2] + glbal.bfbal WHEN glmf.subcat = wsSurp
                   ws[3] = ws[3] + glbal.bfbal
                   wst[1] = wst[1] + glbal.bfbal WHEN glmf.subcat <> wsSurp
                   wst[2] = wst[2] + glbal.bfbal WHEN glmf.subcat = wsSurp
                   wst[3] = wst[3] + glbal.bfbal.
        END.
    END.
    DISPLAY STREAM a "     Balance b/f" @ glmf.DESCRIPTION ws[1] ws[2] ws[3] WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    ASSIGN ws = 0.
    FOR EACH glmf WHERE clas = 5 BREAK BY glmf.subcat BY glmf.acct:
        FOR EACH glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = wsYear:
            DO X = 1 TO wsMonth:
               ASSIGN ws[1] = ws[1] + amt[X] WHEN glmf.subcat <> wsSurp
                      ws[2] = ws[2] + amt[X] WHEN glmf.subcat = wsSurp
                      ws[3] = ws[3] + amt[X]
                      wst[1] = wst[1] + amt[X] WHEN glmf.subcat <> wsSurp
                      wst[2] = wst[2] + amt[X] WHEN glmf.subcat = wsSurp
                      wst[3] = wst[3] + amt[X].
            END.
        END.
        IF LAST-OF(glmf.subcat) AND ws[1] <> 0 OR ws[2] <> 0 OR ws[3] <> 0 THEN DO:
            FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
            DISPLAY STREAM a "     " + glsubcat.descrip @ glmf.DESCRIPTION ws[1] ws[2] ws[3] WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
            ASSIGN ws = 0.
        END.
    END.
    ASSIGN ws = 0.
    FOR EACH glmf WHERE acctype = "I" OR acctype = "E":
        FOR EACH glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = wsYear:
            DO X = 1 TO wsMonth:
               ASSIGN ws[2] = ws[2] + amt[X]
                      ws[3] = ws[3] + amt[X]
                      wst[2] = wst[2] + amt[X]
                      wst[3] = wst[3] + amt[X].
            END.
        END.
    END.
    DISPLAY STREAM a "     Surplus/Deficit for the Period" @ glmf.DESCRIPTION ws[1] ws[2] ws[3] WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
    UNDERLINE STREAM a  ws[1] ws[2] ws[3] WITH FRAME frm-rpt.
    DISPLAY STREAM a "     NET ASSET/EQUITY " @ glmf.DESCRIPTION 
        wst[1] @ ws[1] wst[2] @ ws[2] wst[3] @ ws[3] WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
END.
