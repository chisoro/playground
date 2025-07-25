session:DATA-ENTRY-RETURN = TRUE.
/* Program.................arloc.p
   Notes:...... Capture Asset locations
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Location file already exist"
&SCOPED-DEFINE wsTitle           "Location file Maintenance"
&SCOPED-DEFINE tmptable             Aloc
&SCOPED-DEFINE skey                 Aloc.loc
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.loc ~
                                        COLUMN-LABEL ' Location ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                          WIDTH 60 COLUMN-LABEL ' Description ':C

{varlibrary.i}
DEF BUTTON btnArea      LABEL "AREA".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 18.5.

DEF    QUERY qry-Area FOR Area SCROLLING.
DEF BROWSE brw-Area QUERY qry-Area
        DISPLAY Area.area Area.descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-AreaPick 
        brw-Area AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Area Selection".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 7
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                   COLON 20 LABEL "Location" VIEW-AS TEXT SKIP(.5)
     bfr{&tmptable}.lCode      COLON 20 LABEL "Location Code" SKIP(.5)
    bfr{&tmptable}.descrip    COLON 20 LABEL "Description" SKIP(.5)
    btnArea                   COLON 10
    bfr{&tmptable}.area NO-LABEL
    Area.descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    skip(0.5)
    btn-ok colon 5
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}

ON CHOOSE OF btnArea IN FRAME frm-input
    OR CHOOSE OF btnArea IN FRAME frm-input
DO:
  VIEW FRAME frm-Areapick.
  OPEN QUERY qry-Area FOR EACH area NO-LOCK.
  ENABLE ALL WITH FRAME frm-Areapick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Areapick 
          OR close of THIS-PROCEDURE IN FRAME frm-Areapick
          OR CHOOSE OF btn-ok IN FRAME frm-Areapick 
          OR 'enter':u OF brw-Area
          OR 'mouse-select-dblclick' OF brw-Area.
  CLOSE QUERY qry-Area.
  HIDE FRAME frm-Areapick.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Area.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Areapick 
    OR 'enter':u OF brw-Area
    OR 'mouse-select-dblclick' OF brw-Area
DO: 
   GET CURRENT qry-Area NO-LOCK NO-WAIT.
   DISPLAY area.area @ bfr{&tmptable}.area area.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.Area IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Area IN FRAME frm-input
DO:
    FIND FIRST area WHERE area.area = DEC(bfr{&tmptable}.Area:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE area THEN
        DISPLAY area.DESCRIP  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Area...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Lcode  = bfr{&tmptable}.Lcode:SCREEN-VALUE NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Location already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.loc = DEC({&skey}:SCREEN-VALUE)
                bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.area    = INT(bfr{&tmptable}.area:SCREEN-VALUE)
                 bfr{&tmptable}.Lcode  = bfr{&tmptable}.Lcode:SCREEN-VALUE.
             ASSIGN wsid = DEC({&skey}:SCREEN-VALUE).
                OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
                CLEAR FRAME frm-input ALL.
                {&skey}:SCREEN-VALUE IN FRAME frm-input = STRING(wsid + 1).
            APPLY 'entry' TO bfr{&tmptable}.Lcode IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.area    = INT(bfr{&tmptable}.area:SCREEN-VALUE)
                 bfr{&tmptable}.Lcode  = bfr{&tmptable}.Lcode:SCREEN-VALUE.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.loc.
    FIND FIRST assmf WHERE Assmf.LCode  = bfr{&tmptable}.lCode NO-ERROR.
    IF AVAILABLE assmf THEN DO:
        MESSAGE "Location has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE assmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.loc).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.loc  = wsid NO-ERROR.
    FIND FIRST area WHERE area.area = bfr{&tmptable}.area NO-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.LCode bfr{&tmptable}.Descrip bfr{&tmptable}.area area.descrip WITH FRAME frm-input.
    ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   FIND LAST aloc NO-LOCK NO-ERROR.
   IF AVAILABLE aloc THEN
       ASSIGN wsid = aloc.loc
       {&skey}:SCREEN-VALUE IN FRAME frm-input = STRING(wsid + 1).
   ELSE {&skey}:SCREEN-VALUE IN FRAME frm-input = "1".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.
