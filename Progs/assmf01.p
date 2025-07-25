session:DATA-ENTRY-RETURN = TRUE.
/* Program.................asEnq.p
   Notes:...... ....... Asset Enquiries
   Author:.................S. Mawire
*/
DEF VAR wsLoc LIKE aloc.descrip.
DEF VAR wsDMethod LIKE aloc.descrip.

DEF BUTTON btnAss LABEL "ASSET" SIZE 12.5 BY 1.0.
DEF BUTTON btn-ok LABEL "OK" SIZE 16.5 BY 1.5.
DEF BUTTON btn-close LABEL "CLOSE" SIZE 16.5 BY 1.5.
DEF BUTTON BtnAdd  LABEL "ADD" SIZE 16.5 BY 1.5.
DEF BUTTON BtnEdit  LABEL "EDIT" SIZE 16.5 BY 1.5.
DEF BUTTON BtnDel   LABEL "DELETE" SIZE 16.5 BY 1.5.

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
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Asset Selection".

DEF    QUERY qry-PickImp FOR asadf SCROLLING.
DEF BROWSE brw-PickImp QUERY qry-PickImp
    DISPLAY asadf.descrip asadf.PDate asadf.Cost 
    WITH 6 DOWN SEPARATORS.

DEF    QUERY qry-PickImpTran FOR asstf SCROLLING.
DEF BROWSE brw-PickImpTran QUERY qry-PickImpTran
    DISPLAY asstf.Per asstf.TrDate asstf.Descrip LABEL "Description" 
    asstf.amt  asstf.ref
    WITH 6 DOWN SEPARATORS.

DEF    QUERY qry-PickTrans FOR asstf SCROLLING.
DEF BROWSE brw-PickTrans QUERY qry-PickTrans
    DISPLAY asstf.Per asstf.TrDate asstf.Descrip LABEL "Description" 
    asstf.amt  asstf.ref asstf.acct COLUMN-LABEL "Ledger"
    WITH 15 DOWN SEPARATORS.

DEFINE FRAME frm-Trans
    SKIP(0.5)
    brw-PickTrans AT ROW 1 COL 1.5
    btn-close AT ROW 13.5 COL 40   SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         SIDE-LABELS no-underline three-d SCROLLABLE CENTERED
    TITLE "ASSET TRANSACTION DETAILS".

define frame frm-Main
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
    assmf.RemPer             LABEL "Period Left" SKIP(.5)
    gldept.descrip COLON 20 LABEL "Department"
    glfund.descrip          LABEL "Fund"  SKIP(.5)
    aloc.descrip   COLON 20 LABEL "Asset Location" 
    Area.descrip            LABEL "Area"  SKIP(1.5)
    "ACCESSARIES AND IMPROVEMENTS/ADDITIONS" AT ROW 1 COL 110
    RECT-2 AT ROW 1 COL 2
    RECT-3 AT ROW 1.5 COL 107
    brw-PickImp AT ROW 1.8 COL 108
    brw-PickImpTran AT ROW 8.8 COL 108
    RECT-1 AT ROW 14.8 COL 2
    BtnAdd AT ROW 15.2 COL 10 SPACE(18) 
    BtnEdit SPACE(18) BtnDel SPACE(18) btn-close  SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         SIDE-LABELS no-underline three-d SCROLLABLE BGCOLOR 11 FGCOLOR 1 CENTERED
    TITLE "ACCESSARIES AND IMPROVEMENTS/ADDITIONS MAINTENANCE".

define frame frm-Input
    SKIP(0.5)
    Asadf.ACode      COLON 20 LABEL "Asset" 
    Asadf.descrip             NO-LABEL SKIP(.5)
    Asadf.serial     COLON 20 LABEL "Serial Number" SKIP(.5)
    Asadf.Alife    AT ROW 4 COL 55 LABEL "Useful life in Months"
    Asadf.DPer     AT ROW 4 COL 55 LABEL "Depreciation %/year" SKIP(0.5) 
    Asadf.Cost    COLON 20  LABEL "Original Cost"
    Asadf.RCost             LABEL "Residual Value" SKIP(0.5)
    Asadf.PDate    COLON 20 LABEL "Purchase Date"
    Asadf.DepPer            LABEL "First Depreciation period" 
    Asadf.RemPer              LABEL "Period Left" SKIP(.5)
    asadf.AccumDep COLON 54 LABEL "Accumulated Depreciation" SKIP(0.5)
    asadf.VCost     COLON 20 LABEL "Valuation Cost"
    asadf.AVDate             LABEL "Valuation Date"
    asadf.Astatus
    RECT-4 AT ROW 1.2 COL 2
    RECT-5 AT ROW 12 COL 2
    Btn-Ok AT ROW 12.5 COL 20  SPACE(38) 
    btn-close  SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         SIDE-LABELS no-underline three-d SCROLLABLE BGCOLOR 11 FGCOLOR 1 CENTERED
    TITLE "ACCESSARIES AND IMPROVEMENTS/ADDITIONS MAINTENANCE".

/* ***** Triggers for the main frame **** */
ON 'enter':U OF  Assmf.ACode IN FRAME frm-Main
    OR 'tab':U OF  Assmf.ACode IN FRAME frm-Main
DO:
    ENABLE brw-PickImp BtnAdd BtnEdit BtnDel WITH FRAME frm-main.
    FIND FIRST assmf WHERE assmf.aCode =  Assmf.ACode:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE assmf THEN DO:
        MESSAGE "No Asset with such a code, please try again..." VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-Main.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST asubcat WHERE asubcat.Cat =assmf.Cat NO-LOCK NO-ERROR.
        FIND FIRST acat WHERE acat.Cat =assmf.Cat NO-LOCK NO-ERROR.
        FIND FIRST aloc WHERE aloc.lCode =assmf.lCode NO-LOCK NO-ERROR.
        FIND FIRST area WHERE area.area =assmf.area NO-LOCK NO-ERROR.
        FIND FIRST glDept WHERE glDept.Dept =assmf.Dept NO-LOCK NO-ERROR.
        FIND FIRST glfund WHERE glfund.fund = gldept.fund NO-LOCK NO-ERROR.
        IF  asubcat.DMethod = 1 THEN
            wsDMethod = "Straight Line".
        ELSE IF  asubcat.DMethod = 2 THEN
             wsDMethod = "Reducing Method".  
        DISPLAY assmf.Descrip asubcat.descrip assmf.Alife assmf.Dper wsDMethod
                assmf.AValue assmf.Cost assmf.RCost acat.descrip assmf.PDate 
                Area.descrip aloc.descrip assmf.DepPer assmf.serial assmf.remPer
                GlDept.DESCRIP glfund.descrip WITH FRAME frm-Main.
        IF  asubcat.DMethod = 1 THEN DO:
            assmf.Alife:VISIBLE = TRUE.
            assmf.Dper:VISIBLE = FALSE.
        END.
        ELSE IF  asubcat.DMethod = 2 THEN DO:
             assmf.Alife:VISIBLE = FALSE.
             assmf.Dper:VISIBLE = TRUE.
        END.
        OPEN QUERY qry-PickImp FOR EACH asadf WHERE asadf.Acode = assmf.Acode:SCREEN-VALUE.
    END.
    RETURN.
END.


ON CHOOSE OF BtnAdd IN FRAME frm-Main
DO:  
   VIEW FRAME frm-Input.
   IF  asubcat.DMethod = 1 THEN DO:
            asadf.Alife:VISIBLE = TRUE.
            asadf.Dper:VISIBLE = FALSE.
        END.
        ELSE IF  asubcat.DMethod = 2 THEN DO:
             asadf.Alife:VISIBLE = FALSE.
             asadf.Dper:VISIBLE = TRUE.
        END.
        ASSIGN asadf.ACODE:SCREEN-VALUE = assmf.Acode
               asadf.Alife:SCREEN-VALUE = STRING(asadf.Alife)
               asadf.Dper:SCREEN-VALUE = STRING(asadf.DPer).   
    ENABLE ALL EXCEPT asadf.Acode WITH FRAME frm-Input.
    APPLY 'entry' TO asadf.descrip IN FRAME frm-Input.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Input.
    HIDE FRAME frm-Input.
    RETURN.
END.

ON 'mouse-select-dblclick':U OF  brw-PickImp IN FRAME frm-Main
    OR CHOOSE OF BtnEdit IN FRAME frm-Main
    OR 'enter':u OF  brw-PickImp IN FRAME frm-Main
DO:
   GET CURRENT qry-PickImp  NO-LOCK NO-WAIT.
   VIEW FRAME frm-Input.
   IF asadf.AccumDep = 0  THEN
      ENABLE ALL EXCEPT asadf.ACode asadf.AVDate asadf.VCost WITH FRAME frm-Input.
   ELSE
      ENABLE btn-Ok btn-Close asadf.descrip asadf.serial asadf.Alife asadf.DepPer
          WITH FRAME frm-Input.
   DISPLAY asadf WITH FRAME frm-Input.
   IF  asubcat.DMethod = 1 THEN DO:
            asadf.Alife:VISIBLE = TRUE.
            asadf.Dper:VISIBLE = FALSE.
        END.
        ELSE IF  asubcat.DMethod = 2 THEN DO:
             asadf.Alife:VISIBLE = FALSE.
             asadf.Dper:VISIBLE = TRUE.
        END.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Input.
    HIDE FRAME frm-Input.
    RETURN.
END.


ON CHOOSE OF BtnAss IN FRAME frm-Main
    OR CHOOSE OF BtnAss IN FRAME frm-Main
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

ON CHOOSE OF btn-ok IN FRAME frm-PickAsset 
    OR 'enter':u OF brw-PickAsset
    OR 'mouse-select-dblclick' OF brw-PickAsset
DO: 
   GET CURRENT qry-PickAsset NO-LOCK NO-WAIT.
   assmf.Acode:SCREEN-VALUE IN FRAME frm-Main = assmf.Acode.
   APPLY 'tab' TO btnASS.
   APPLY 'tab' TO assmf.ACode IN FRAME frm-Main.
   RETURN. 
END.
ON 'choose':U OF BtnDel IN FRAME frm-Main
DO:
    VIEW FRAME frm-Trans.
    ENABLE ALL WITH FRAME frm-Trans.
    OPEN QUERY qry-PickTrans FOR EACH asstf WHERE asstf.Acode = assmf.Acode:SCREEN-VALUE.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Trans.
    HIDE FRAME frm-Trans.
    RETURN.
END.


/********** MAIN LOGIC **********/
VIEW FRAME frm-Main.
ENABLE  btnAss Assmf.ACode btn-close WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Main.
HIDE FRAME frm-main.
