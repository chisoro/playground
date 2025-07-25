/* Program.................hse02c.p.p
   Notes:................. Rename/transfer Account to another account code
   Author:.................S. Mawire
*/

session:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE tmptable           hsecmf
&SCOPED-DEFINE skey               hsecmf.dbacc

{varlib.i}

DEF VAR wsnew     LIKE hsecmf.dbacc.   
DEF VAR wsold     LIKE hsecmf.dbacc.   
DEF VAR wsStatus  LIKE hsecmf.dbacc.
DEF VAR wsopt     AS   CHAR.
DEF VAR wsSize    LIKE hsecmf.SIZE.
DEF VAR wsStand   LIKE hsecmf.standno.
DEF VAR wsPrice   LIKE hsecmf.Pprice.
DEF VAR wsSite    LIKE hsecmf.siteValue.

DEF BUTTON btn-schem     LABEL "SCHEME".

DEF BUFFER temphse FOR hsecmf.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF    QUERY qry-pickScheme FOR hsesch SCROLLING.
DEF BROWSE brw-pickScheme QUERY qry-pickScheme
        DISPLAY hsesch.scheme hsesch.Descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickScheme 
        brw-pickScheme AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-exit colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Scheme Selection".

DEF FRAME frm-main
    SKIP(1.5)
    wsnew     LABEL "Enter New Account" COLON 30 SKIP(0.5)
    wsold     LABEL "Enter Old Account" COLON 30 bfr{&tmpTable}.NAME NO-LABEL VIEW-AS TEXT  SKIP(0.5)
    btn-schem                           COLON 20 hsesch.Scheme NO-LABEL hsesch.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    bfr{&tmpTable}.StandNo LABEL "Stand number" COLON 30 SKIP(0.5)
    bfr{&tmpTable}.Name LABEL "Account Name" COLON 30 SKIP(0.5)
    bfr{&tmpTable}.Add1 LABEL "Address"      COLON 30 SKIP(0.2)
    bfr{&tmpTable}.add2 NO-LABEL  COLON 30 SKIP(0.2)
    bfr{&tmpTable}.Add3 NO-LABEL  COLON 30 SKIP(0.2)
    bfr{&tmpTable}.PDate  LABEL "Purchase DATE" COLON 30 SKIP(0.2)
    bfr{&tmpTable}.PPrice LABEL "Purchase Price" COLON 30  SKIP(0.2)
    bfr{&tmpTable}.Bal    LABEL "Capital Balance" COLON 30 SKIP(0.2)
    bfr{&tmpTable}.amtdue[1]    LABEL "Revenue Balance" COLON 30 SKIP(0.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 10 space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "LAND SALES ACCOUNT MIGRATION" VIEW-AS DIALOG-BOX.


ON 'enter':U OF wsnew IN FRAME frm-Main 
DO:
    ASSIGN wsnew.
    FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = wsnew  NO-LOCK NO-ERROR.
    IF AVAILABLE bfr{&tmpTable} AND bfr{&tmpTable}.Accstat = 0 THEN DO:
        MESSAGE "Account already exist ...Stand already allocated" VIEW-AS ALERT-BOX.
            wsnew:SCREEN-VALUE = "0".
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE bfr{&tmpTable} AND bfr{&tmpTable}.Accstat <> 0 THEN
        wsopt = "trf".
     ELSE IF NOT AVAILABLE bfr{&tmpTable} THEN
        wsopt = "recode".
        ENABLE btn-schem hsesch.scheme bfr{&tmpTable}.StandNo WITH FRAME frm-main.
    RETURN.
END.

ON 'enter':U OF wsold IN FRAME frm-Main 
DO:
    ASSIGN wsold.
    FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = wsold AND bfr{&tmpTable}.Accstat = 0 NO-ERROR.
    IF NOT AVAILABLE bfr{&tmpTable} THEN DO:
       MESSAGE "Account does not exist ...pleas try again" VIEW-AS ALERT-BOX.
        wsold:SCREEN-VALUE = "0".
        RETURN NO-APPLY.
    END.
    ELSE DO:
         DISPLAY bfr{&tmpTable}.NAME bfr{&tmpTable}.StandNo bfr{&tmpTable}.Add1 bfr{&tmpTable}.add2 bfr{&tmpTable}.Add3 bfr{&tmpTable}.PDate bfr{&tmpTable}.PPrice 
                        bfr{&tmpTable}.bal bfr{&tmpTable}.Scheme @ hsesch.scheme bfr{&tmpTable}.amtdue[1] WITH FRAME frm-main.
         FIND FIRST hsesch WHERE hsesch.Scheme = bfr{&tmpTable}.scheme NO-LOCK NO-ERROR.
         IF AVAILABLE hsesch THEN
            DISPLAY hsesch.descrip WITH FRAME frm-main.
    END.    
    RETURN.                             
END.

ON CHOOSE OF btn-Schem IN FRAME frm-main
DO:
  VIEW FRAME frm-pickScheme.
  OPEN QUERY qry-pickScheme FOR EACH hsesch NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickScheme.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickScheme 
          OR close of THIS-PROCEDURE IN FRAME frm-pickScheme 
          OR CHOOSE OF btn-ok IN FRAME frm-pickScheme 
          OR 'enter':u OF brw-pickScheme
          OR 'mouse-select-dblclick' OF brw-pickScheme.
  CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO  hsesch.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickScheme 
    OR 'enter':u OF brw-PickScheme
    OR 'mouse-select-dblclick' OF brw-PickScheme
DO: 
   GET CURRENT qry-PickScheme NO-LOCK NO-WAIT.
   DISPLAY hsesch.Scheme hsesch.descrip WITH FRAME frm-main.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO hsesch.Scheme.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmpTable}.StandNo IN FRAME frm-main
    OR 'leave':U OF bfr{&tmpTable}.StandNo IN FRAME frm-main
DO:
    FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.standNo = bfr{&tmpTable}.standNo:SCREEN-VALUE
                        AND bfr{&tmpTable}.scheme  = INT(hsesch.scheme:SCREEN-VALUE) 
                        AND bfr{&tmpTable}.dbacc   <> wsOld  NO-ERROR.
    IF AVAILABLE bfr{&tmpTable}  THEN DO:
        MESSAGE "Stand Number " bfr{&tmpTable}.standNo:SCREEN-VALUE "is already allocated, try again"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Main
DO:
    wsbtn = "EDIT".
   FOR EACH hseblf WHERE hseblf.dbacc = wsold:
       DISPLAY hseblf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       hseblf.dbacc = wsnew.
   END.
   FOR EACH hsemtf WHERE hsemtf.dbacc = wsold:
       DISPLAY hsemtf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       hsemtf.dbacc = wsnew.
   END.
   FOR EACH hsehtf WHERE hsehtf.dbacc = wsold:
       DISPLAY hsehtf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       hsehtf.dbacc = wsnew.
   END.
   CASE wsopt:
    WHEN "trf" THEN DO:
       FIND FIRST temphse WHERE temphse.dbacc = wsnew AND temphse.Accstat <> 0 NO-ERROR.
       ASSIGN temphse.dbacc = 999.
       FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = wsold AND bfr{&tmpTable}.Accstat = 0 NO-ERROR.
       ASSIGN  wsSize = bfr{&tmpTable}.SIZE
               wsStand  = bfr{&tmpTable}.StandNo
               wsPrice  =  bfr{&tmpTable}.PPrice
               wsSite   =  bfr{&tmpTable}.sitevalue
               bfr{&tmpTable}.dbacc = wsnew
               bfr{&tmpTable}.comnt =  bfr{&tmpTable}.comnt + ", Account migrated from " + STRING(wsold)
               bfr{&tmpTable}.StandNo = temphse.StandNo
               bfr{&tmpTable}.SIZE    = temphse.Size
               bfr{&tmpTable}.Scheme  = temphse.Scheme
               bfr{&tmpTable}.SiteValue = temphse.sitevalue
               bfr{&tmpTable}.Bal     = temphse.PPrice - (bfr{&tmpTable}.PPrice - bfr{&tmpTable}.Bal)
               bfr{&tmpTable}.PPrice  = temphse.PPrice
               bfr{&tmpTable}.RegId1  = varuser.
      
       ASSIGN temphse.dbacc = wsold
              tempHse.SIZE  = wsSize
              temphse.PPrice = wsPrice
              temphse.standno = wsstand
              temphse.RegId1  = varuser
              temphse.SiteValue = wsSite.
    END.
    WHEN "recode" THEN DO:
          FIND FIRST bfr{&tmpTable} WHERE bfr{&tmpTable}.dbacc = wsold AND bfr{&tmpTable}.Accstat = 0 NO-ERROR.
          IF AVAILABLE bfr{&tmpTable} THEN DO:
              ASSIGN bfr{&tmpTable}.dbacc = wsnew
                     bfr{&tmpTable}.StandNo = bfr{&tmpTable}.StandNo:SCREEN-VALUE
                     bfr{&tmpTable}.Scheme  = INT(hsesch.Scheme:SCREEN-VALUE)
                     bfr{&tmpTable}.RegId1  = varuser
                     bfr{&tmpTable}.comnt =  bfr{&tmpTable}.comnt + ", Account migrated from " + STRING(wsold).

          END.
              
    END.
   END CASE.
   
   MESSAGE "Account migration completed.....Please edit the new account and amend other information." VIEW-AS ALERT-BOX.
    APPLY 'entry' TO btn-exit IN FRAME frm-main.
    RETURN.
END.

{audit.i}

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE wsnew wsOld btn-ok btn-exit WITH FRAME frm-main.
ASSIGN wsOld:SCREEN-VALUE = "0"
       wsNew:SCREEN-VALUE = "0".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
