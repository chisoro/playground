/* Program.................simusr01.p
   Notes:................. Create User
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
/* Standard List Definitions                                            */
DEFINE BUTTON btn-discard DEFAULT 
     LABEL "&Cancel/Exit" 
     SIZE 15.4 BY 1.0 TOOLTIP "Click thois button to discard all changes and exit....."
     BGCOLOR 15 FGCOLOR 0 .
DEFINE BUTTON btn-Save DEFAULT 
     LABEL "&Save" 
     SIZE 15.4 BY 1.0 TOOLTIP "Click this button to save your record..."
     BGCOLOR 15 FGCOLOR 0 .
DEFINE VARIABLE w_DateCreated AS DATE FORMAT "99/99/9999":U 
     LABEL "Date Created" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .86
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
DEFINE VARIABLE w_email AS CHARACTER FORMAT "X(20)":U 
     LABEL "E-Mail Address" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .86
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE w_Name AS CHARACTER FORMAT "X(15)":U 
     LABEL "User Name" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .86
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE w_UserNo AS CHARACTER FORMAT "X(25)":U 
     LABEL "User Code" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .86
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     w_UserNo AT ROW 2.62 COL 20 COLON-ALIGNED WIDGET-ID 2
     w_name AT ROW 5.57 COL 20 COLON-ALIGNED WIDGET-ID 30
     btn-Save AT ROW 12.71 COL 10 WIDGET-ID 48
     btn-discard AT ROW 12.71 COL 50 WIDGET-ID 46
     "Login Details" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 40
          FONT 6
    "User Details" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 4.38 COL 9 WIDGET-ID 42
          FONT 6
     RECT-2 AT ROW 1.81 COL 6 WIDGET-ID 34
     /*RECT-3 AT ROW 5.29 COL 6 WIDGET-ID 36 */
     RECT-4 AT ROW 4.62 COL 6 WIDGET-ID 38
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         AT COL 2.2 ROW 1.19
         SIZE 80.8 BY 15.1
         BGCOLOR 6 FGCOLOR 0  WIDGET-ID 100
         TITLE "User Maintenance".

ON CHOOSE OF btn-discard IN FRAME DEFAULT-FRAME /* Discard form and exit */
DO:
  MESSAGE "Are you sure you want to close this window?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
       TITLE "Discard Form?" UPDATE w_response AS LOGICAL.

  IF w_response THEN
      simusr.txtPass    = "sim"
     APPLY "CLOSE":U TO THIS-PROCEDURE.
  ELSE
      RETURN NO-APPLY.
END.

ON CHOOSE OF btn-Save IN FRAME DEFAULT-FRAME /* Save and exit */
DO:
   CREATE simusr.
   ASSIGN simusr.usercode   = w_userno:SCREEN-VALUE 
          simusr.txtPass    = "sim"
          simusr.Name       = w_name:SCREEN-VALUE
          simusr.txtStatus  = "A".
   CLEAR FRAME default-frame.
   APPLY 'entry' TO w_userno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME DEFAULT-FRAME.
WAIT-FOR CHOOSE OF btn-discard OR CLOSE of THIS-PROCEDURE IN FRAME DEFAULT-FRAME.
HIDE FRAME DEFAULT-FRAME.


