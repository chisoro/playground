/* reading attribute values into variables */

/********** DEFINE FIELD-LEVEL WIDGETS **********/
DEFINE RECTANGLE Rect1 SIZE-CHARS 10 BY 4 EDGE-CHARS 1.
DEFINE VARIABLE Field1 AS LOGICAL LABEL "Is Rectangle Visible?".
DEFINE BUTTON btn-Rect LABEL "Show Rectangle".
DEFINE BUTTON btn-Exit LABEL "Exit".
/********** DEFINE FRAMES **********/
DEFINE FRAME Frame1
SKIP(1) Rect1 SKIP(1) Field1 SKIP(1) btn-Rect SKIP(2) btn-Exit
WITH SIDE-LABELS NO-BOX CENTERED THREE-D.
/********** DEFINE TRIGGERS **********/
ON CHOOSE OF btn-Rect
DO:
/*3*/ ASSIGN Rect1:VISIBLE = YES
/*4*/ Field1 = Rect1:VISIBLE.
/*5*/ DISPLAY Field1 WITH FRAME Frame1.
DISABLE btn-Rect WITH FRAME Frame1.
END. /* ON CHOOSE OF btn-Rect */
/********** MAIN LOGIC **********/
/*1*/ ASSIGN Rect1:VISIBLE = NO
/*2*/ Field1 = Rect1:VISIBLE.
DISPLAY Field1 WITH FRAME Frame1.
ENABLE Field1 btn-Rect btn-Exit WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
