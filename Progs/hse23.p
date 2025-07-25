
/* Program.................hse23.p
   Notes:................. Customer Listing
   Author:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsfac AS INTEGER.
DEF VAR wsstart LIKE  hsecmf.dbAcc.
DEF VAR wsend LIKE  hsecmf.dbAcc.
DEF VAR wsStatus AS CHAR.
DEF VAR  wsSub LIKE dbsmf.descrip.
DEF VAR  wsWard LIKE dbsmf.descrip.
DEF VAR txtCur LIKE hsesch.txtCur EXTENT 4.
DEF VAR X AS INT.
DEF VAR i AS INT.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "ACCOUNT STATIC DETAILS REPORT" VIEW-AS DIALOG-BOX.

FORM
    SKIP (0.5)
     "-----------------------------------------------ACCOUNT DETAILS ------------------------------------------" COLON 1 SKIP(2)
    "ACCOUNT   :" COLON 5 hsecmf.dbacc   hsecmf.NAME
    "SORT NAME   :"     COLON 77.5 hsecmf.Sortname  SKIP
    "ID/REG NO.: "     COLON 5 hsecmf.RegID  
    "VAT REG. NO.: "    COLON 77.5 hsecmf.VatNo SKIP
    "ADDRESS   : "      colon 5 hsecmf.Add1 
    hsecmf.Add2         colon 17
    hsecmf.Add3         colon 17 SKIP
    "CELL/PHONE:"       COLON 5 hsecmf.cell 
    "eMAIL     :"       COLON 5 hsecmf.emailAdd SKIP(1)
    
    "------------------PROPERTY DETAILS ------------------" COLON 20 SKIP(1)
   "STAND NUMBER   : "   COLON 5 hsecmf.standNo    "DEED NUMBER: "   COLON 65 hsecmf.DeedNo   SKIP
    "SURBUB         : "        COLON 5 dbsmf.Descrip
    "WARD       : "          COLON 65 dbwrd.Descrip SKIP(0.5)
    "STAND SIZE     :" COLON 5 hsecmf.SIZE      SKIP
    "PURCHASE PRICE :" COLON 5 hsesch.txtCur FORM "x(3)" hsecmf.pprice FORM "->>>,>>>,>>9.99" SKIP
    "DEPOSIT CHARGED:" COLON 5 txtcur[1] FORM "x(3)" hsecmf.damt FORM "->>>,>>>,>>9.99" SKIP
    "INSTALLMENT    :" COLON 5 txtcur[2] FORM "x(3)" hsecmf.INSamt FORM "->>>,>>>,>>9.99" SKIP    
    "SITE VALUE     :"  COLON 5 txtcur[3] FORM "x(3)" hsecmf.sitevalue FORM "->>>,>>>,>>9.99"
    "BUILDING VALUE :" COLON 5 txtcur[4] FORM "x(3)" hsecmf.bldvalue  FORM "->>>,>>>,>>9.99"  SKIP(5)
    WITH DOWN STREAM-IO FONT 8 CENTERED  NO-BOX NO-LABELS
    WIDTH 132 FRAME frm-rpt.


ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsStart wsEnd .
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
    FOR EACH hsecmf WHERE dbacc >= wsStart AND dbAcc <= wsEnd:
        FIND FIRST dbsmf WHERE dbsmf.Suburb = hsecmf.Suburb NO-LOCK NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsSub = dbsmf.descrip.
            ELSE   wsSub = "N/A".
        FIND FIRST dbwrd WHERE dbwrd.Ward = hsecmf.Ward NO-LOCK NO-ERROR.
        IF AVAILABLE dbwrd THEN
            wsward = dbwrd.descrip.
            ELSE   wsWard = "N/A".
        FIND FIRST hsesch WHERE hsecmf.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
         txtCur[1] = hsesch.txtCur.
         txtCur[2] = hsesch.txtCur. 
         txtCur[3] = hsesch.txtCur. 
         txtCur[4] = hsesch.txtCur. 
        DISPLAY STREAM a hsecmf.dbAcc hsecmf.Name  hsecmf.RegID 
            hsecmf.SiteValue hsecmf.Size hsecmf.SortName hsecmf.StandNo hsecmf.VatNo txtCur[1] txtCur[2]  txtCur[3]  txtCur[4]
            hsecmf.emailAdd hsecmf.DeedNo hsecmf.Cell hsecmf.BldValue hsecmf.Add3 hsecmf.damt hsecmf.insamt
            hsecmf.add2 hsecmf.Add1 wsward @ dbwrd.descrip wsSub @ dbsmf.descrip hsesch.txtCur hsecmf.pprice
            WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        i = i + 1.
        IF i = 2 THEN DO:
             PAGE STREAM a.
             X = 0.
        END.
       
    END.
END.

