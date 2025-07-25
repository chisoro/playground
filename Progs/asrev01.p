session:DATA-ENTRY-RETURN = TRUE.
/* Program.................asrev01.p
   Notes:...... ....... Asset Revaluation
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsPer  LIKE SIMCTR.NDep.
DEF VAR wsLoc LIKE aloc.descrip.
DEF VAR wsDMethod LIKE aloc.descrip.
DEF VAR wsAmt    LIKE assmf.AValue.
DEF VAR wsBank   AS INT.
DEF VAR wsCb     AS INT.
DEF VAR wsDrLedger LIKE glmf.acct.
DEF VAR wsCrLedger LIKE glmf.acct.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR wsproj   LIKE dbgl.Proj. 
DEF VAR wsDes LIKE dbgl.descrip.
DEF VAR wsSource LIKE dbgl.source.
DEF VAR wsDr      AS DEC FORM "zz,zzz,zz9.99-".
DEF VAR wsCr      AS DEC FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsDr.
DEF VAR wsTCr    LIKE wsDr.
DEF VAR wsDate AS DATE.
DEF VAR yy AS INT.
DEF VAR mm AS INT.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsRem  LIKE Assmf.RemPer.
DEF VAR wsRCost LIKE Assmf.RCost.

DEF BUTTON btnAss LABEL "ASSET" SIZE 12.5 BY 1.0.
DEF BUTTON btn-ok LABEL "COMMIT" SIZE 16.5 BY 1.5.
DEF BUTTON btn-close LABEL "CLOSE" SIZE 16.5 BY 1.5.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 178 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 104 by 13.5.

DEFINE RECTANGLE rect-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 72 by 13.

DEFINE RECTANGLE rect-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 104 by 10.5.

DEFINE RECTANGLE rect-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 104 by 2.3.

DEF    QUERY qry-PickAsset FOR assmf SCROLLING.
DEF BROWSE brw-PickAsset QUERY qry-PickAsset
    DISPLAY assmf.ACode assmf.descrip assmf.Cost assmf.Pdate 
    WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickAsset 
    brw-PickAsset AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Asset Selection".

DEF    QUERY qry-PickImp FOR asadf SCROLLING.
DEF BROWSE brw-PickImp QUERY qry-PickImp
    DISPLAY asadf.descrip asadf.PDate asadf.Cost 
    WITH 15 DOWN SEPARATORS.

FORM 
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "PAYMENTS UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" wsPer
      "Page:" AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

define frame frm-main
    SKIP(0.5)
    btnAss           COLON 10 NO-LABEL NO-TAB-STOP
    Assmf.ACode               NO-LABEL 
    assmf.descrip             NO-LABEL SKIP(.5)
    assmf.serial     COLON 20 LABEL "Serial Number" SKIP(.5)
    acat.descrip     COLON 20 LABEL "Category" SKIP(0.5)
    asubcat.descrip  COLON 20 LABEL "Sub-category" 
    wsDMethod                 LABEL "Depreciation Method"  SKIP(0.5)
    assmf.Alife    AT ROW 7 COL 55 LABEL "Useful life in Months"
    assmf.DPer     AT ROW 7 COL 55 LABEL "Depreciation %/year" SKIP(0.5) 
    assmf.Cost    COLON 20  LABEL "Original Cost"
    assmf.AValue            LABEL "Asset Value" 
    assmf.RCost             LABEL "Residual Value" SKIP(0.5)
    assmf.PDate    COLON 20 LABEL "Purchase Date"
    assmf.DepPer            LABEL "First Depreciation period" 
    assmf.RemPer              LABEL "Period Left" SKIP(.5)
    gldept.descrip COLON 20 LABEL "Department"
    glfund.descrip          LABEL "Fund"  SKIP(.5)
    aloc.descrip   COLON 20 LABEL "Asset Location" 
    Area.descrip            LABEL "Area"  SKIP(1.5)
    "ASSET REVALUATION DETAILS" AT ROW 1 COL 110 SKIP(1)
    wsPer          COLON 125 LABEL "Accounting Period" SPACE(10) wsDate LABEL "Transaction Date" SKIP(0.5)
    wsAmt           COLON 135 LABEL "Revaluation Amount" SKIP(1)
    wsLedger        COLON 135 LABEL "Revaluation Reserve Ledger" SKIP(1)
    wsRem           COLON 135 LABEL "New Asset Life" SKIP(1)
    wsrCost      COLON 135 LABEL "New Residual Value" 
    RECT-2 AT ROW 1 COL 2
    RECT-3 AT ROW 1.5 COL 107
    RECT-1 AT ROW 14.8 COL 2
    btn-ok AT ROW 15.2 COL 30 SPACE(80) btn-close  SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         SIDE-LABELS no-underline three-d SCROLLABLE BGCOLOR 11 FGCOLOR 1 CENTERED
    TITLE "ASSET REVALUATION".


/* ***** Triggers for the main frame **** */
ON 'enter':U OF  Assmf.ACode IN FRAME frm-main
    OR 'tab':U OF  Assmf.ACode IN FRAME frm-main
DO:
    FIND FIRST assmf WHERE assmf.aCode =  Assmf.ACode:SCREEN-VALUE AND Astatus = "A" NO-ERROR.
    IF NOT AVAILABLE assmf THEN DO:
        MESSAGE "No Asset with such a code or has been disposed, please try again..." VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-main.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST asubcat WHERE asubcat.Cat = assmf.Cat NO-LOCK NO-ERROR.
        FIND FIRST acat WHERE acat.Cat =assmf.Cat NO-LOCK NO-ERROR.
        FIND FIRST aloc WHERE aloc.lCode =assmf.lCode NO-LOCK NO-ERROR.
        FIND FIRST area WHERE area.area =assmf.area NO-LOCK NO-ERROR.
        FIND FIRST glDept WHERE glDept.Dept =assmf.Dept NO-LOCK NO-ERROR.
        FIND FIRST glfund WHERE glfund.fund = gldept.fund NO-LOCK NO-ERROR.
        IF  asubcat.DMethod = 1 THEN
            wsDMethod = "Straight Line".
        ELSE IF  asubcat.DMethod = 2 THEN
             wsDMethod = "Reducing Method". 
        ASSIGN wsRem = assmf.RemPer
               wsRcost = assmf.RCost. 
        DISPLAY assmf.Descrip asubcat.descrip assmf.Alife assmf.Dper wsDMethod
                assmf.AValue assmf.Cost assmf.RCost acat.descrip assmf.PDate 
                Area.descrip aloc.descrip assmf.DepPer assmf.serial assmf.RemPer
                GlDept.DESCRIP glfund.descrip wsrCost wsRem WITH FRAME frm-main.
        IF  asubcat.DMethod = 1 THEN DO:
            assmf.Alife:VISIBLE = TRUE.
            assmf.Dper:VISIBLE = FALSE.
        END.
        ELSE IF  asubcat.DMethod = 2 THEN DO:
             assmf.Alife:VISIBLE = FALSE.
             assmf.Dper:VISIBLE = TRUE.
        END.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main
DO:
    ASSIGN wsamt 
           wsDate 
           wsPer
           wsLedger
           wsRem
           wsRcost.
     wsDes = "Revaluation".
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="Revalue.ip"
                     &paged} 
    CLEAR FRAME frm-main.
    APPLY 'entry' TO assmf.acode.
    RETURN.
END.


ON CHOOSE OF BtnAss IN FRAME frm-main
    OR CHOOSE OF BtnAss IN FRAME frm-main
DO:
  VIEW FRAME frm-PickAsset.
  OPEN QUERY qry-PickAsset FOR EACH Assmf WHERE Assmf.Astatus = "A" NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickAsset.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickAsset 
          OR close of THIS-PROCEDURE IN FRAME frm-PickAsset
          OR CHOOSE OF btn-ok IN FRAME frm-PickAsset 
          OR 'enter':u OF brw-PickAsset
          OR 'mouse-select-dblclick' OF brw-PickAsset.
  CLOSE QUERY qry-PickAsset.
  HIDE FRAME frm-PickAsset.
  APPLY 'tab' TO btnASS.
  RETURN. 
END.

ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <> INT(wsper:SCREEN-VALUE)
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickAsset 
    OR 'enter':u OF brw-PickAsset
    OR 'mouse-select-dblclick' OF brw-PickAsset
DO: 
   GET CURRENT qry-PickAsset NO-LOCK NO-WAIT.
   assmf.Acode:SCREEN-VALUE IN FRAME frm-main = assmf.Acode.
   APPLY 'tab' TO btnASS.
   APPLY 'tab' TO assmf.ACode IN FRAME frm-main.
   RETURN. 
END.


/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
CLEAR FRAME frm-main.
ASSIGN wsper:SCREEN-VALUE = STRING(simctr.CURPER) 
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsSource = "AS"
       wsdes    = "Revaluation".
VIEW FRAME frm-main.
ENABLE  btnAss Assmf.ACode btn-ok btn-close wsamt wsLedger wsDate wsPer wsRCost wsRem WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Revalue.ip:
    ASSIGN wsTime    = STRING(TIME,"HH:MM:SS")
           wsTransID = DEC (string(YEAR(TODAY),"9999")
                     + string(MONTH(TODAY),"99" )
                     + string(DAY(TODAY),"99")
                     + SUBSTR(STRING(wsTIME),1,2) 
                     + SUBSTR(STRING(wsTIME),4,2) 
                     + SUBSTR(STRING(wsTIME),7,2) ).
    FIND FIRST gldept WHERE gldept.dept = assmf.dept NO-LOCK NO-ERROR.
    ASSIGN wsDrLedger = dec(string(GlDept.FUND) + STRING(GlDept.Dept) + STRING(asubcat.Lsuffix[2]))
                                WHEN simctr.LForm = 1
              wsCrLedger = dec(string(GlDept.FUND) + STRING(asubcat.Lsuffix[1],"99999999")) WHEN simctr.LForm = 1
              wsDrLedger = asubcat.Lsuffix[2] WHEN simctr.LForm = 2
              wsCrLedger = asubcat.Lsuffix[1] WHEN simctr.LForm = 2.
    CREATE asstf.
    ASSIGN asstf.acct    = wsLedger
           asstf.ACode   = assmf.acode
           asstf.amt     = wsAmt
           asstf.Descrip = wsdes
           asstf.Per     = wsPer
           asstf.ref     = "Rev\" + STRING(wsPer)
           asstf.TrDate  = wsDate.
    /* create Cr Revaluation with full amount */
    FIND FIRST dbgl WHERE dbgl.acct = wsCrLedger AND dbgl.SOURCE   = "AS" 
                      AND dbgl.REF      = "Rev/" + STRING(wsPer) no-error. 
    IF NOT AVAILABLE dbgl THEN DO:
       CREATE dbgl.
       ASSIGN  dbgl.CREDATE = TODAY
               dbgl.DESCRIP  = wsDes 
               dbgl.period   = wsPer
               dbgl.REF      = "REV/:" + STRING(wsPer)
               dbgl.TransID  = wsTransId
               dbgl.trDATE   = wsDate
               dbgl.UID      = varUser
               dbgl.acct     = wsLedger 
               dbgl.Dept     = Assmf.Dept
               dbgl.Proj     = wsproj
               dbgl.SOURCE   = "AS"
               dbgl.UID2    = varUser.
    END.
    ASSIGN dbgl.AMT     = dbgl.AMT + ((wsAmt - Assmf.NBV)* -1).
         /* create Dr Accumlated Depreciation ledger */
    FIND FIRST dbgl WHERE dbgl.acct = wsDrLedger AND dbgl.SOURCE   = "AS"  
                      AND dbgl.REF      = "Rev/" + STRING(wsPer) no-error.
    IF NOT AVAILABLE dbgl THEN DO:
       CREATE dbgl.
       ASSIGN  dbgl.CREDATE = TODAY
               dbgl.DESCRIP  = wsDes 
               dbgl.period   = wsPer
               dbgl.REF      = "Rev/" + STRING(wsPer)
               dbgl.TransID  = wsTransId
               dbgl.trDATE   = wsDate
               dbgl.UID      = varUser
               dbgl.acct     = wsDrLedger
               dbgl.Dept     = Assmf.Dept
               dbgl.Proj     = wsproj
               dbgl.SOURCE   = "AS"
               dbgl.UID2    = varUser.
    END.
    ASSIGN dbgl.AMT     = dbgl.AMT + Assmf.AccumDep .
        /* Dr Asset with revaluation less NBV (gain/loss) */
    FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.SOURCE   = "AS" 
                      AND dbgl.REF      = "Rev/" + STRING(wsPer) no-error.
    IF NOT AVAILABLE dbgl THEN DO:
       CREATE dbgl.
       ASSIGN  dbgl.CREDATE = TODAY
               dbgl.DESCRIP  = wsDes 
               dbgl.period   = wsPer
               dbgl.REF      = "Rev/" + STRING(wsPer)
               dbgl.TransID  = wsTransId
               dbgl.trDATE   = wsDate
               dbgl.UID      = varUser
               dbgl.acct     = wscrLedger
               dbgl.Dept     = Assmf.Dept
               dbgl.Proj     = wsproj
               dbgl.SOURCE   = "AS"
               dbgl.UID2    = varUser.
    END.
    ASSIGN dbgl.AMT      = dbgl.AMT + (wsAmt - Assmf.NBV).
    ASSIGN assmf.Avalue  = wsAmt
           Assmf.AVDate  = wsDate
           Assmf.VCost   = wsAmt
           Assmf.NBV     = wsAmt
           Assmf.RemPer  = wsRem  WHEN wsRem <> 0
           assmf.Alife   = wsRem  WHEN wsRem <> 0
           assmf.RCost   = wsRCost
           Assmf.AccumDep = 0.  
    {glcon.i}
END.
