DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE  MENU-BAR-C-Win AS HANDLE NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE NO-UNDO.
DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.
DEFINE VARIABLE h1SubMenu AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE varUser AS CHARACTER.
DEF VAR wsprg AS CHAR.
DEF NEW SHARED VAR wsName LIKE simusr.Name.
DEF NEW SHARED VAR wsCo   LIKE SIMCTR.CONAME.
/* Menu Definitions  */

{dynamicmenu.i.}

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = wsCo + ":- " + wsName  + "  - SimAcc Accounting System ver.1.0.0" 
         HEIGHT             = 24.48
         WIDTH              = 250.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 12
         FGCOLOR            = 3
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
/*ELSE {&WINDOW-NAME} = CURRENT-WINDOW.*/

ASSIGN C-Win:MENUBAR =  MENU-BAR-C-Win.

 VIEW C-Win.
/* RUN ENABLE_UI.*/
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
