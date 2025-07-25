/* Program.................asrpt03.p
    Notes:...... ..........Asset ACQUISITION Report
    Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsTotal LIKE wsAmt EXTENT 2.
DEF VAR subTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR st-date AS DATE.
DEF VAR end-date AS DATE.
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR wsItem LIKE payitem.itemcode.
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsTitle1 AS CHAR FORM "x(90)".
DEF VAR st-cat LIKE acat.cat.
DEF VAR en-cat LIKE acat.cat.
DEF VAR st-subcat LIKE asubcat.subcat.
DEF VAR en-subcat LIKE asubcat.subcat.
DEF VAR st-dept LIKE gldept.dept.
DEF VAR en-dept LIKE gldept.dept.
DEF VAR wsNbv   LIKE assmf.cost.
DEF VAR tCost LIKE wsNbv.
DEF VAR tDep  LIKE wsNbv.
DEF VAR wsDep LIKE assmf.alife.
DEF VAR tNbv  LIKE wsNbv.
DEF VAR wsCnt AS INT.
DEF VAR Des LIKE acat.descrip EXTENT 3.

DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "PRINT".
DEF BUTTON btnDept LABEL "Start Dept"
    SIZE 15 BY 1.
DEF BUTTON btnDept1 LABEL "End Dept"
    SIZE 15 BY 1.
DEF BUTTON btnCat LABEL "Start Category"
    SIZE 15 BY 1.
DEF BUTTON btnCat1 LABEL "End Category"
    SIZE 15 BY 1.
DEF BUTTON btnSubCat LABEL "Start SubCategory"
    SIZE 17 BY 1.
DEF BUTTON btnSubCat1 LABEL "End SubCategory"
    SIZE 17 BY 1.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 10.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

DEF    QUERY qry-pickDept FOR glDept SCROLLING.
DEF BROWSE brw-pickDept QUERY qry-pickDept
    DISPLAY GLDept.Dept GLDept.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickDept 
    brw-pickDept AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Department Selection".


DEF    QUERY qry-pickCat FOR aCat SCROLLING.
DEF BROWSE brw-pickCat QUERY qry-pickCat
    DISPLAY aCat.Cat aCat.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickCat 
    brw-pickCat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         
    side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF    QUERY qry-picksubCat FOR asubCat SCROLLING.
DEF BROWSE brw-picksubCat QUERY qry-picksubCat
    DISPLAY asubCat.subCat asubCat.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-picksubCat 
    brw-picksubCat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Sub-Category Selection".

DEFINE FRAME frm-Main
     SKIP(.5)
     st-date      COLON 31 LABEL "Start Date" SKIP(0.3)
     end-date      COLON 31 LABEL "End Date"
     BtnsubCat    AT ROW 4 COL 15 NO-TAB-STOP AUTO-RETURN 
                       st-SubCat    NO-LABEL AUTO-RETURN des[1] NO-LABEL VIEW-AS TEXT
     BtnsubCat1    AT ROW 5.2 COL 15 NO-TAB-STOP AUTO-RETURN 
                    en-SubCat  NO-LABEL AUTO-RETURN asubcat.descrip NO-LABEL VIEW-AS TEXT
     BtnCat    AT ROW 6.4 COL 17 NO-TAB-STOP AUTO-RETURN 
                       st-Cat    NO-LABEL AUTO-RETURN des[2] NO-LABEL VIEW-AS TEXT 
     BtnCat1    AT ROW 7.6 COL 17 NO-TAB-STOP AUTO-RETURN 
                    en-Cat  NO-LABEL AUTO-RETURN acat.descrip NO-LABEL VIEW-AS TEXT 
     BtnDept    AT ROW 8.8 COL 17 NO-TAB-STOP AUTO-RETURN 
                       st-dept    NO-LABEL  des[3] NO-LABEL VIEW-AS TEXT
     BtnDept1    AT ROW 10 COL 17 NO-TAB-STOP AUTO-RETURN 
                    en-dept  NO-LABEL gldept.descrip NO-LABEL VIEW-AS TEXT 
     btn-ok AT ROW 12.5 COL 10 SPACE(50) btn-close
     rect-1 AT ROW 1 COL 3
     rect-2 AT ROW 12.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 15
     TITLE "ASSET ACQUISITION REPORTS" VIEW-AS DIALOG-BOX.

FORM SPACE(5)
       Assmf.ACode    COLUMN-LABEL "ASSET CODE"
       Assmf.descrip  COLUMN-LABEL "DESCRIPTION" FORM "!(40)"
       Assmf.PDate    COLUMN-LABEL "PURCHASE!DATE"
       Assmf.Cost     COLUMN-LABEL "COST"
       Assmf.AccumDep COLUMN-LABEL "ACCUMULATED!DEPRECIATION"
       Assmf.CurDep   COLUMN-LABEL "CURRENT!DEPRECIATION"
       wsNbv          COLUMN-LABEL "NBV"
       HEADER SKIP(1) wsTitle AT 20 skip(2) wsTitle1 AT 10
            "Page:" AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
            WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX WIDTH 132 FRAME frm-rpt.
 /* Triggers  */

ON CHOOSE OF btnDept IN FRAME frm-main
DO:
   RUN Dept.ip.
   st-dept:SCREEN-VALUE = STRING(gldept.dept).
  APPLY 'tab' TO st-dept.
END.

ON 'tab':U OF st-dept IN FRAME frm-Main 
    OR 'enter':U OF st-dept IN FRAME frm-Main 
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(st-dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE gldept THEN 
        DISPLAY gldept.descrip @ des[3] WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid Department entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.


ON CHOOSE OF btnDept1 IN FRAME frm-main
DO:
   RUN Dept.ip.
    en-dept:SCREEN-VALUE = STRING(gldept.dept).
  APPLY 'tab' TO en-dept.
END.
ON 'tab':U OF en-dept IN FRAME frm-Main 
    OR 'enter':U OF en-dept IN FRAME frm-Main
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(en-dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE gldept THEN 
        DISPLAY gldept.descrip WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid Department entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.

ON CHOOSE OF btnCat IN FRAME frm-main
DO:
   RUN Cat.ip.
   st-cat:SCREEN-VALUE = STRING(acat.cat).
  APPLY 'tab' TO st-Cat.
END.
ON 'tab':U OF st-cat IN FRAME frm-Main
    OR 'enter':U OF st-cat IN FRAME frm-Main
DO:
    FIND FIRST acat WHERE acat.cat = INT(st-cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE acat THEN 
        DISPLAY acat.descrip @ des[2] WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid Category entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.

ON CHOOSE OF btnCat1 IN FRAME frm-main
DO:
   RUN Cat.ip.
   en-cat:SCREEN-VALUE = STRING(acat.cat).
  APPLY 'tab' TO en-Cat.
END.
ON 'tab':U OF en-cat IN FRAME frm-Main
    OR 'enter':U OF en-cat IN FRAME frm-Main
DO:
    FIND FIRST acat WHERE acat.cat = INT(en-cat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE acat THEN 
        DISPLAY acat.descrip WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid Category entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.

ON CHOOSE OF btnsubCat IN FRAME frm-main
DO:
   RUN subcat.ip.
   st-subcat:SCREEN-VALUE = STRING(asubcat.subcat).
  APPLY 'tab' TO st-subcat.
END.
ON 'tab':U OF st-subcat IN FRAME frm-Main 
    OR 'enter':U OF st-subcat IN FRAME frm-Main 
DO:
    FIND FIRST asubcat WHERE asubcat.subcat = INT(st-subcat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE asubcat THEN 
        DISPLAY asubcat.descrip @ des[1] WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid sub-Category entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.

ON CHOOSE OF btnsubCat1 IN FRAME frm-main
DO:
   RUN SubCat.ip.
   en-subcat:SCREEN-VALUE = STRING(asubcat.subcat).
  APPLY 'tab' TO en-subcat.
END.
ON 'tab':U OF en-subcat IN FRAME frm-Main 
    OR 'enter':U OF en-subcat IN FRAME frm-Main 
DO:
    FIND FIRST asubcat WHERE asubcat.subcat = INT(en-subcat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE asubcat THEN 
        DISPLAY asubcat.descrip WITH FRAME frm-main.
    ELSE DO:
        MESSAGE "Invalid sub-Category entered...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Main 
DO:
    ASSIGN st-cat st-subcat st-dept en-cat en-subcat en-dept
          st-date end-date.
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN   wsCo = SIMCTR.CONAME
         wsTitle = simctr.coname + "   - REPORT DATE " + STRING(TODAY)
         st-cat:SCREEN-VALUE = "0"
         st-subcat:SCREEN-VALUE = "0"
         st-dept:SCREEN-VALUE = "0".
VIEW FRAME frm-Main IN WINDOW CURRENT-WINDOW.
FIND LAST asubcat NO-LOCK NO-ERROR.
IF AVAILABLE asubcat THEN DO:
    ASSIGN en-subcat:SCREEN-VALUE = STRING(asubcat.subcat).
    DISPLAY asubcat.descrip WITH FRAME frm-main.
END.
ELSE ASSIGN en-cat:SCREEN-VALUE = "9999".
FIND LAST acat NO-LOCK NO-ERROR.
IF AVAILABLE acat THEN DO:
    ASSIGN en-cat:SCREEN-VALUE = STRING(acat.cat).
    DISPLAY acat.descrip WITH FRAME frm-main.
END.
ELSE ASSIGN en-cat:SCREEN-VALUE = "9999".
FIND LAST gldept  NO-LOCK NO-ERROR.
IF AVAILABLE gldept THEN DO:
    ASSIGN en-dept:SCREEN-VALUE = STRING(gldept.dept).
    DISPLAY gldept.descrip WITH FRAME frm-main.
END.
ELSE ASSIGN en-dept:SCREEN-VALUE = "9999".
ENABLE ALL  WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Main.
HIDE FRAME frm-Main.
RETURN.

PROCEDURE Dept.ip:
    VIEW FRAME frm-PickDept.
   OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK BY gldept.dept .
   ENABLE ALL WITH FRAME frm-PickDept.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickDept 
           OR close of THIS-PROCEDURE IN FRAME frm-PickDept
           OR CHOOSE OF btn-ok IN FRAME frm-PickDept 
           OR 'enter':u OF brw-PickDept
           OR 'mouse-select-dblclick' OF brw-PickDept.
   CLOSE QUERY qry-PickDept.
   HIDE FRAME frm-PickDept.
   APPLY 'tab' TO SELF.
   APPLY 'tab'.
   RETURN. 
END.

PROCEDURE Cat.ip:
    VIEW FRAME frm-PickCat.
   OPEN QUERY qry-PickCat FOR EACH aCat NO-LOCK.
   ENABLE ALL WITH FRAME frm-PickCat.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickCat 
           OR close of THIS-PROCEDURE IN FRAME frm-PickCat
           OR CHOOSE OF btn-ok IN FRAME frm-PickCat 
           OR 'enter':u OF brw-PickCat
           OR 'mouse-select-dblclick' OF brw-PickCat.
   CLOSE QUERY qry-PickCat.
   HIDE FRAME frm-PickCat.
   APPLY 'tab' TO SELF.  
   APPLY 'tab'.
   RETURN. 
END.


PROCEDURE subcat.ip:
    VIEW FRAME frm-Picksubcat.
   OPEN QUERY qry-Picksubcat FOR EACH asubcat NO-LOCK.
   ENABLE ALL WITH FRAME frm-Picksubcat.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Picksubcat 
           OR close of THIS-PROCEDURE IN FRAME frm-Picksubcat
           OR CHOOSE OF btn-ok IN FRAME frm-Picksubcat 
           OR 'enter':u OF brw-Picksubcat
           OR 'mouse-select-dblclick' OF brw-Picksubcat.
   CLOSE QUERY qry-Picksubcat.
   HIDE FRAME frm-Picksubcat.
   APPLY 'tab' TO SELF.
   APPLY 'tab'.
   RETURN. 
END.

PROCEDURE Report.ip:
    ASSIGN wsTitle1 =  "ASSET ACQUISITION REPORT BY CATEGORY BY SUB-CATEGORY FOR THE PERIOD "
                        + STRING(st-date) + " TO " + STRING(end-date)
           wsCnt = 0
           tCost = 0
           tNbv  = 0
           tDep  = 0.
    FOR EACH assmf WHERE assmf.cat >= st-cat AND assmf.cat <= en-cat
                    AND assmf.subcat >= st-subcat AND assmf.subcat <= en-subcat
                    AND assmf.dept >= st-dept AND assmf.dept <= en-dept 
                    AND assmf.PDate >= st-date AND assmf.PDate <= end-date  NO-LOCK
                    BREAK BY assmf.cat BY assmf.subcat :
        IF FIRST-OF(assmf.cat) THEN DO:
            FIND FIRST acat WHERE acat.cat = assmf.cat NO-LOCK NO-ERROR.
            IF AVAILABLE acat THEN DO:
                DISPLAY STREAM a "CAT:" @ assmf.acode acat.descrip @ assmf.Descrip WITH FRAME frm-rpt.
                DOWN STREAM a  WITH FRAME frm-rpt.
            END.
        END.
        IF FIRST-OF(assmf.subcat) THEN DO:
            FIND FIRST asubcat WHERE asubcat.subcat = assmf.subcat NO-LOCK NO-ERROR.
            IF AVAILABLE asubcat THEN DO:
                DISPLAY STREAM a "SUB-CAT:" @ assmf.acode "   " + asubcat.descrip @ assmf.Descrip WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
            END.
        END.    
        ASSIGN wsNbv = Assmf.Cost - Assmf.AccumDep
               tCost = tCost + assmf.cost
               tDep  = tDep + assmf.AccumDep
               tNbv  = tNbv + wsNbv
               wscnt = wscnt + 1.
        ACCUMULATE assmf.cost (TOTAL BY assmf.cat BY assmf.subcat).
        ACCUMULATE assmf.cost (COUNT BY assmf.cat BY assmf.subcat ).
        ACCUMULATE assmf.AccumDep (TOTAL BY assmf.cat BY assmf.subcat).
        ACCUMULATE Assmf.Cost - Assmf.AccumDep (TOTAL BY assmf.cat BY assmf.subcat).
        DISPLAY STREAM a Assmf.ACode "     " + Assmf.descrip @ Assmf.descrip Assmf.PDate Assmf.Cost
                assmf.curDep Assmf.AccumDep  wsNbv WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        IF LAST-OF(assmf.subcat) THEN DO:
            wsDes = "SUB-CAT TOTAL = " + STRING(ACCUM COUNT BY assmf.subcat Assmf.Cost).
            UNDERLINE STREAM a assmf.descrip assmf.cost assmf.AccumDep wsNbv WITH FRAME frm-rpt.
            DISPLAY STREAM a  wsDes @ assmf.descrip 
                  ACCUM TOTAL BY assmf.subcat assmf.cost  @ assmf.cost 
                  ACCUM TOTAL BY assmf.subcat assmf.AccumDep @ assmf.AccumDep 
                  ACCUM TOTAL BY assmf.subcat Assmf.Cost - Assmf.AccumDep @ wsNbv WITH FRAME frm-rpt.
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        END. 
        IF LAST-OF(assmf.cat) THEN DO:
            FIND FIRST acat WHERE acat.cat = assmf.cat NO-LOCK NO-ERROR.
            wsDes = acat.descrip + ":= " + STRING(ACCUM COUNT BY assmf.cat Assmf.Cost).
            UNDERLINE STREAM a assmf.descrip assmf.cost assmf.AccumDep wsNbv WITH FRAME frm-rpt.
            DISPLAY STREAM a  wsDes @ assmf.descrip 
                  ACCUM TOTAL BY assmf.cat assmf.cost  @ assmf.cost 
                  ACCUM TOTAL BY assmf.cat assmf.AccumDep @ assmf.AccumDep 
                  ACCUM TOTAL BY assmf.cat Assmf.Cost - Assmf.AccumDep @ wsNbv WITH FRAME frm-rpt.
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        END. 
    END. 
END.
