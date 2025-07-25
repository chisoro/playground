session:DATA-ENTRY-RETURN = TRUE.
/* Program.................assmf.p
   Notes:...... Capture Asset Masterfile
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Asset file already exist"
&SCOPED-DEFINE wsTitle           "Asset file Maintenance"
&SCOPED-DEFINE tmptable             Assmf
&SCOPED-DEFINE skey                 Assmf.syscode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.SysCode ~
                                        COLUMN-LABEL ' System Code ':C ~
                              bfr{&tmptable}.ACode ~
                                        COLUMN-LABEL ' Asset Code ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                          WIDTH 40 COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.Cost ~
                                          COLUMN-LABEL ' Cost ':C ~
                              bfr{&tmptable}.Serial ~
                                          COLUMN-LABEL ' Serial Number ':C


{varlibrary.i}
DEF VAR wsDMethod AS CHAR FORM "x(20)".
DEF BUTTON btnLoc LABEL "Location".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 18.5.

DEF    QUERY qry-Picksubcat FOR asubcat SCROLLING.
DEF BROWSE brw-Picksubcat QUERY qry-Picksubcat
    DISPLAY asubcat.subcat asubcat.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Picksubcat 
    brw-Picksubcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF    QUERY qry-Pickloc FOR aloc SCROLLING.
DEF BROWSE brw-Pickloc QUERY qry-Pickloc
    DISPLAY aloc.LCode aloc.descrip COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Pickloc 
    brw-Pickloc AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Category Selection".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 4
    btn-add AT ROW 20.7 COL 10
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 110 BY 23.5
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                   COLON 35 LABEL "System Asset Code" SKIP(.5)
    bfr{&tmptable}.Acode      COLON 35 LABEL "Asset Code" SKIP(.5)
    bfr{&tmptable}.descrip    COLON 35 LABEL "Description" SKIP(.5)
     bfr{&tmptable}.serial    COLON 35 LABEL "Serial Number" SKIP(0.5)
    Btnsubcat                 COLON 17 NO-TAB-STOP
    bfr{&tmptable}.subcat               NO-LABEL
    asubcat.descrip                     NO-LABEL VIEW-AS TEXT 
    wsDMethod                     LABEL "Depreciation Method"  VIEW-AS TEXT SKIP(0.5)
    asubcat.Alife      COLON 35   LABEL "Useful life in Months"
    asubcat.DPer                  LABEL "Depreciation %/year" Skip(0.5) 
    acat.descrip        COLON 35  LABEL "Asset category"  VIEW-AS TEXT SKIP(1)
    bfr{&tmptable}.Cost COLON 35  LABEL "Original Cost"
    bfr{&tmptable}.AValue         LABEL "Asset Value" VIEW-AS TEXT SKIP(0.5)
    bfr{&tmptable}.RCost COLON 35 LABEL "Residual Value" SKIP(0.5)
    bfr{&tmptable}.PDate COLON 35 LABEL "Purchase Date"
    bfr{&tmptable}.DepPer         LABEL "First Depreciation period" 
    bfr{&tmptable}.RemPer           LABEL "Period Left" VIEW-AS TEXT SKIP(1)
    btnDept               COLON 12 NO-LABEL NO-TAB-STOP
    bfr{&tmptable}.dept   NO-LABEL
    gldept.descrip                   NO-LABEL VIEW-AS TEXT
    glfund.descrip                   LABEL "Fund" VIEW-AS TEXT SKIP(.5)
    btnLoc               COLON 25 NO-LABEL NO-TAB-STOP
    bfr{&tmptable}.LCode   NO-LABEL
    aloc.descrip                   NO-LABEL VIEW-AS TEXT
    Area.descrip                   LABEL "Area" VIEW-AS TEXT SKIP(1.5)
    btn-ok colon 20
    btn-close colon 80 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}

ON CHOOSE OF Btnsubcat IN FRAME frm-input
    OR CHOOSE OF Btnsubcat IN FRAME frm-input
DO:
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
   APPLY 'tab' TO bfr{&tmptable}.subCat.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Picksubcat 
    OR 'enter':u OF brw-Picksubcat
    OR 'mouse-select-dblclick' OF brw-Picksubcat
DO: 
   GET CURRENT qry-Picksubcat NO-LOCK NO-WAIT.
   DISPLAY asubcat.subCat @ bfr{&tmptable}.subCat asubcat.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.subCat IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.subCat IN FRAME frm-input
DO:
    FIND FIRST asubcat WHERE asubcat.subCat = DEC(bfr{&tmptable}.subCat:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE asubcat THEN DO:
        IF asubcat.DMethod = 1 THEN
           wsDMethod = "Straight Line".
        IF asubcat.DMethod = 2 THEN
           wsDMethod = "Reducing Method".
        FIND FIRST acat WHERE acat.cat = asubcat.cat NO-LOCK NO-ERROR.
        DISPLAY asubcat.DESCRIP wsdMethod acat.Descrip WITH FRAM frm-input.
        IF asubcat.DMethod = 1 THEN DO:
            asubcat.Alife:VISIBLE = TRUE.
            asubcat.Dper:VISIBLE = FALSE.
            DISPLAY  asubcat.Alife WITH FRAM frm-input.
        END.  
        ELSE IF asubcat.DMethod  = 2 THEN DO:
            asubcat.Alife:VISIBLE = FALSE.
            asubcat.Dper:VISIBLE = TRUE.
            DISPLAY asubcat.DPer WITH FRAM frm-input.
        END.
    END.
    ELSE DO:
        MESSAGE "Invalid sub-category...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'tab':U OF bfr{&tmptable}.Cost 
    OR 'enter':U OF bfr{&tmptable}.Cost
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN
        bfr{&tmptable}.AValue:SCREEN-VALUE = STRING(bfr{&tmptable}.Cost:SCREEN-VALUE).
    ELSE bfr{&tmptable}.AValue:SCREEN-VALUE = STRING(bfr{&tmptable}.Cost + bfr{&tmptable}.ACost).
    RETURN.
END.

ON CHOOSE OF Btnloc IN FRAME frm-input
    OR CHOOSE OF Btnloc IN FRAME frm-input
DO:
  VIEW FRAME frm-Pickloc.
  OPEN QUERY qry-Pickloc FOR EACH aloc NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pickloc.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Pickloc 
          OR close of THIS-PROCEDURE IN FRAME frm-Pickloc
          OR CHOOSE OF btn-ok IN FRAME frm-Pickloc 
          OR 'enter':u OF brw-Pickloc
          OR 'mouse-select-dblclick' OF brw-Pickloc.
  CLOSE QUERY qry-Pickloc.
  HIDE FRAME frm-Pickloc.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.lCode.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Pickloc 
    OR 'enter':u OF brw-Pickloc
    OR 'mouse-select-dblclick' OF brw-Pickloc
DO: 
   GET CURRENT qry-Pickloc NO-LOCK NO-WAIT.
   DISPLAY aloc.lCode @ bfr{&tmptable}.lcod aloc.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.lcode IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.lcode IN FRAME frm-input
DO:
    FIND FIRST aloc WHERE aloc.lcode = bfr{&tmptable}.lcode:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE aloc THEN DO:
        FIND FIRST Area WHERE Area.area = aloc.area NO-LOCK NO-ERROR.
        DISPLAY aloc.DESCRIP Area.descrip WITH FRAME frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid sub-category...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.


ON CHOOSE OF BtnDept IN FRAME frm-input
    OR CHOOSE OF BtnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-PickDept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickDept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickDept 
          OR close of THIS-PROCEDURE IN FRAME frm-PickDept
          OR CHOOSE OF btn-ok IN FRAME frm-PickDept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-PickDept.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Dept.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickDept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept NO-LOCK NO-WAIT.
   DISPLAY glDept.Dept @ bfr{&tmptable}.Dept glDept.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.Dept IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Dept IN FRAME frm-input
DO:
    FIND FIRST glDept WHERE glDept.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glDept THEN DO:
        FIND FIRST glfund WHERE glfund.fund = gldept.fund NO-LOCK NO-ERROR.
        DISPLAY gldept.descrip glfund.descrip WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Department...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Cost IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Cost IN FRAME frm-input
DO:
     ASSIGN bfr{&tmptable}.AValue:SCREEN-VALUE = bfr{&tmptable}.Cost:SCREEN-VALUE.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.aCode  = bfr{&tmptable}.aCode:SCREEN-VALUE NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Asset already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN  bfr{&tmptable}.SysCode = DEC({&skey}:SCREEN-VALUE)
                    bfr{&tmptable}.aCode    = bfr{&tmptable}.aCode:SCREEN-VALUE
                    bfr{&tmptable}.Descrip  = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                    bfr{&tmptable}.subCat   = INT(bfr{&tmptable}.subCat:SCREEN-VALUE)
                    bfr{&tmptable}.Cat      = acat.Cat
                    bfr{&tmptable}.Alife    = INT(asubcat.Alife:SCREEN-VALUE)
                    bfr{&tmptable}.DPer     = DEC(asubcat.DPer:SCREEN-VALUE)
                    bfr{&tmptable}.area     = Area.area
                    bfr{&tmptable}.AValue   = DEC(bfr{&tmptable}.AValue:SCREEN-VALUE)
                    bfr{&tmptable}.Cost     = DEC(bfr{&tmptable}.Cost:SCREEN-VALUE)
                    bfr{&tmptable}.DepPer   = DEC(bfr{&tmptable}.DepPer:SCREEN-VALUE)
                    bfr{&tmptable}.lcode    = bfr{&tmptable}.lcode:SCREEN-VALUE
                    bfr{&tmptable}.PDate    = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE)
                    bfr{&tmptable}.RCost    = DEC(bfr{&tmptable}.RCost:SCREEN-VALUE)
                    bfr{&tmptable}.Dept     = DEC(bfr{&tmptable}.Dept:SCREEN-VALUE)
                    bfr{&tmptable}.fund     = gldept.fund
                    bfr{&tmptable}.serial   = bfr{&tmptable}.serial:SCREEN-VALUE
                    bfr{&tmptable}.RemPer     = INT(asubcat.Alife:SCREEN-VALUE)
                    bfr{&tmptable}.astatus  = "A"
                    bfr{&tmptable}.NBV      = DEC(bfr{&tmptable}.AValue:SCREEN-VALUE).
            IF asubcat.DMethod = 1 THEN
                bfr{&tmptable}.RemPer = INT(asubcat.Alife:SCREEN-VALUE).
            ELSE IF asubcat.DMethod = 2 OR asubcat.DMethod = 3 THEN
                 bfr{&tmptable}.RemPer = (100 / INT(asubcat.DPer:SCREEN-VALUE)) * 12.
           ASSIGN wsid = DEC({&skey}:SCREEN-VALUE).
           OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} No-lock.
           CLEAR FRAME frm-input ALL.
           {&skey}:SCREEN-VALUE IN FRAME frm-input = STRING(wsid + 1).
           APPLY 'entry' TO bfr{&tmptable}.aCode IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                /*bfr{&tmptable}.aCode    = bfr{&tmptable}.aCode:SCREEN-VALUE */
                bfr{&tmptable}.Descrip  = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.subCat   = INT(bfr{&tmptable}.subCat:SCREEN-VALUE)
                bfr{&tmptable}.Cat      = acat.Cat
                bfr{&tmptable}.Alife    = INT(asubcat.Alife:SCREEN-VALUE)
                bfr{&tmptable}.DPer     = DEC(asubcat.DPer:SCREEN-VALUE)
                bfr{&tmptable}.area     = Area.area
                bfr{&tmptable}.AValue   = DEC(bfr{&tmptable}.AValue:SCREEN-VALUE)
                bfr{&tmptable}.Cost     = DEC(bfr{&tmptable}.Cost:SCREEN-VALUE)
                bfr{&tmptable}.DepPer   = DEC(bfr{&tmptable}.DepPer:SCREEN-VALUE)
                bfr{&tmptable}.lCode    = bfr{&tmptable}.lCode:SCREEN-VALUE
                bfr{&tmptable}.PDate    = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE)
                bfr{&tmptable}.RCost    = DEC(bfr{&tmptable}.RCost:SCREEN-VALUE)
                bfr{&tmptable}.Dept     = DEC(bfr{&tmptable}.Dept:SCREEN-VALUE)
                bfr{&tmptable}.fund     = gldept.fund
                bfr{&tmptable}.serial   = bfr{&tmptable}.serial:SCREEN-VALUE
                bfr{&tmptable}.RemPer     = INT(asubcat.Alife:SCREEN-VALUE)
                bfr{&tmptable}.NBV      = DEC(bfr{&tmptable}.AValue:SCREEN-VALUE).
          IF asubcat.DMethod = 1 THEN
                bfr{&tmptable}.RemPer = INT(asubcat.Alife:SCREEN-VALUE).
            ELSE IF asubcat.DMethod = 2 OR asubcat.DMethod = 3 THEN
                 bfr{&tmptable}.RemPer = (100 / INT(asubcat.DPer:SCREEN-VALUE)) * 12.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.SysCode.
    FIND FIRST asstf WHERE asstf.ACode  = bfr{&tmptable}.ACode NO-ERROR.
    IF AVAILABLE asstf THEN DO:
        MESSAGE "Asset has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE asstf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK WHERE bfr{&tmptable}.astatus  = "A" .
ENABLE ALL  WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.SysCode).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.SysCode  = wsid EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST asubcat WHERE asubcat.Cat = bfr{&tmptable}.Cat NO-LOCK NO-ERROR.
    FIND FIRST acat WHERE acat.Cat = bfr{&tmptable}.Cat NO-LOCK NO-ERROR.
    FIND FIRST aloc WHERE aloc.lCode = bfr{&tmptable}.lCode NO-LOCK NO-ERROR.
    FIND FIRST area WHERE area.area = bfr{&tmptable}.area NO-LOCK NO-ERROR.
    IF asubcat.DMethod = 1 THEN
           wsDMethod = "Straight Line".
    IF asubcat.DMethod = 2 THEN
           wsDMethod = "Reducing Method".
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.subCat asubcat.descrip 
            bfr{&tmptable}.Alife @ asubcat.Alife bfr{&tmptable}.Dper @ asubcat.Dper wsDMethod
            bfr{&tmptable}.ACode bfr{&tmptable}.AValue bfr{&tmptable}.Cost bfr{&tmptable}.RCost
            acat.descrip  bfr{&tmptable}.PDate bfr{&tmptable}.lCode Area.descrip aloc.descrip 
           bfr{&tmptable}.DepPer bfr{&tmptable}.serial bfr{&tmptable}.RemPer WITH FRAME frm-input.
    FIND FIRST glDept WHERE glDept.Dept = bfr{&tmptable}.Dept NO-LOCK NO-ERROR.
    IF AVAILABLE glDept THEN DO:
        FIND FIRST glfund WHERE glfund.fund = gldept.fund NO-LOCK NO-ERROR.
        DISPLAY bfr{&tmptable}.Dept GlDept.DESCRIP glfund.descrip WITH FRAM frm-input.
    END.
    FIND FIRST asstf WHERE asstf.ACode = bfr{&tmptable}.ACode  NO-LOCK NO-ERROR.
    IF  AVAILABLE asstf THEN
        ENABLE ALL EXCEPT {&skey} bfr{&tmptable}.Cost bfr{&tmptable}.RCost WITH FRAME frm-input. 
    ELSE  
        ENABLE ALL EXCEPT {&skey} bfr{&tmptable}.Acode WITH FRAME frm-input.
    IF  asubcat.DMethod = 1 THEN DO:
        asubcat.Alife:VISIBLE = TRUE.
        asubcat.Dper:VISIBLE = FALSE.
    END.
    IF  asubcat.DMethod = 2 THEN DO:
        asubcat.Alife:VISIBLE = FALSE.
        asubcat.Dper:VISIBLE = TRUE.
    END.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} No-lock.
   HIDE FRAME frm-input.
 END.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK WHERE bfr{&tmptable}.astatus  = "A".
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT {&skey} WITH FRAME frm-input.
   asubcat.Alife:VISIBLE = FALSE.
   asubcat.Dper:VISIBLE = FALSE.
   FIND LAST assmf NO-LOCK NO-ERROR.
   IF AVAILABLE assmf THEN
       ASSIGN wsid = assmf.syscode
       {&skey}:SCREEN-VALUE IN FRAME frm-input = STRING(wsid + 1).
   ELSE {&skey}:SCREEN-VALUE IN FRAME frm-input = "1".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK WHERE bfr{&tmptable}.astatus  = "A".
   HIDE FRAME frm-input.
END.
