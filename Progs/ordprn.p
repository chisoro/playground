/* Program.................ordprn.p
   Notes:................. Order print
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsrec AS CHAR FORM "x(60)".
DEF VAR wsTotal AS DEC FORM "zzzzzz9.99".
DEF VAR wsVat   AS DEC FORM "zzzzzz9.99".
DEF VAR wsl     AS INT.
DEF VAR wsfile AS CHAR FORM "x(40)".

FIND FIRST simctr NO-LOCK NO-ERROR.
FIND LAST ordmf NO-LOCK NO-ERROR.
wsfile = "g:\simacc\Ord" + string(ordmf.OrdNo) + ".pdf".
{ pdf_inc.i "THIS-PROCEDURE"}

RUN pdf_new("Spdf",wsfile).
RUN pdf_set_orientation("Spdf","PORTRAIT").
RUN pdf_load_image ("Spdf", "mygif", "logo.png").

/*DO v_Month = 1 TO 12: */
  RUN pdf_new_page("Spdf").
  RUN pdf_set_font("Spdf","Courier-Bold",24.0).
  RUN pdf_text_color("Spdf",0.10,0.0,0.0).
  RUN pdf_text_xy("Spdf",STRING(simCtr.CONAME), 60, 746).
  RUN pdf_place_image ("Spdf", "mygif", 200, 199, ?, ?).
  RUN pdf_set_TextRed("Spdf", 1.0).
  RUN pdf_text_xy("Spdf","OFFICIAL ORDER", 200,720).
  RUN pdf_text_color("Spdf",0.0,0.0,0.0).
  RUN pdf_skipn("Spdf",3).
  RUN pdf_set_font("Spdf","Courier-Bold",14.0).
  RUN pdf_text_at("Spdf","Order #:" + STRING(ordmf.OrdNo),50).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf"," Date #:" + STRING(ordmf.ordDate),50).
  RUN pdf_skipn("Spdf",4).
FIND FIRST crdmf WHERE crdmf.acc = ordmf.acc NO-LOCK NO-ERROR.
  RUN pdf_text_at("Spdf",STRING(Crdmf.Name),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(Crdmf.Add1),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(Crdmf.Add2 ),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(Crdmf.town),5).
  wsRec = "-----------------------------------------------------------------------".
  RUN pdf_text_at("Spdf",wsrec,1).
  RUN pdf_skipn("Spdf",1).
  RUN pdf_text_at("Spdf","SEND INVOICE TO:",4).
  RUN pdf_text_at("Spdf","DELIVERY TO:",50).
  RUN pdf_skip("Spdf").
  RUN pdf_set_font("Spdf","Courier-Bold",10.0).
  RUN pdf_text_color("Spdf",0.0,0.0,0.0).
  RUN pdf_text_at("Spdf",STRING(simCtr.CONAME),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(simCtr.Add1),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(simCtr.Add2),5).
  RUN pdf_skip("Spdf").
  RUN pdf_text_at("Spdf",STRING(simCtr.Add3),5).
  RUN pdf_skipn("Spdf",2).
  wsRec = "LINE  DESCRIPTION                          QUANTITY        VAT         AMOUNT".
  RUN pdf_text_at("Spdf",wsrec,10).
  RUN pdf_skip("Spdf").
  wsRec = "------------------------------------------------------------------------------------".
  RUN pdf_text_at("Spdf",wsrec,10).
  RUN pdf_skip("Spdf").
  wsl = 15.
FOR EACH Ordtmf WHERE Ordtmf.OrdNo = ordmf.OrdNo:
    ASSIGN wsTotal = wsTotal + Ordtmf.Amt
           wsVat   = wsVat   + ordtmf.VAT
           wsl     = wsl  - 1.
    RUN pdf_text_at("Spdf",Ordtmf.LineSeq,10). 
    RUN pdf_text_at("Spdf",Ordtmf.Descrip,16).       
    RUN pdf_text_at("Spdf",Ordtmf.Qty,56). 
    RUN pdf_text_at("Spdf",STRING(Ordtmf.Vat,">>>,>>9.99-"),62). 
    RUN pdf_text_at("Spdf",STRING(Ordtmf.Amt,">>>,>>>,>>9.99-"),76).
    RUN pdf_skipn("Spdf",1).
END.
RUN pdf_skipn("Spdf",wsl).
RUN pdf_text_at("Spdf",wsrec,10).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","TOTAL",16).
RUN pdf_text_at("Spdf",STRING(wsTotal,">>>,>>>,>>9.99-"),76).
RUN pdf_skipn("Spdf",3).
RUN pdf_text_at("Spdf","CONDITION OF CONTRACT",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","1. RISK OF SUPPLIER: The risk remains with the supplier until delivery has",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","   been affected.",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","2. DELIVERY: if the delivery date cannot be adhered to, the supplier should ",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","   immediately contact the procurement department.",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","3. INVOICE: Invoices must be forwarded immediately after despatch of goods or",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","    services to the above address.",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","4. PRICE ESCALATIONS: Escalations in control prices should be authenticated ",11).
RUN pdf_skip("Spdf").
RUN pdf_text_at("Spdf","   by the relevant date.",11).
RUN pdf_skipn("Spdf",5).
RUN pdf_text_at("Spdf","Prepared by: Procurement Officer",10).
RUN pdf_text_at("Spdf","Approved by: Chief Excutive Officer",54).
RUN pdf_skipn("Spdf",3).
RUN pdf_text_at("Spdf","Signature:----------------------------",10).
RUN pdf_text_at("Spdf","Signature:----------------------------",54).
RUN pdf_skipn("Spdf",2).
RUN pdf_text_at("Spdf","Date:----------------------------",10).
RUN pdf_text_at("Spdf","Date:----------------------------",54).
RUN pdf_skipn("Spdf",2).
RUN pdf_close("Spdf").
OS-COMMAND NO-WAIT VALUE(wsfile).

 /* Draw Border */
  /*RUN pdf_stroke_fill  ("Spdf",1.0,1.0,1.0).
  RUN pdf_rect  ("Spdf", 30, 
                         30, 
                         pdf_PageWidth("Spdf") - 50, 
                         pdf_PageHeight("Spdf") - 50, .5).
  RUN pdf_rect  ("Spdf", 33, 
                         33, 
                         pdf_PageWidth("Spdf") - 56, 
                         pdf_PageHeight("Spdf") - 56, .5).

  /* Draw Week Sections - 5 per month */
  DO v_Loop = 1 TO 6:
    RUN pdf_line  ("Spdf", 33, 
                           100 * v_Loop - 67, 
                           pdf_PageWidth("Spdf") - 24, 
                           100 * v_Loop - 67, 
                           .5).

  END. /* week sections */

  /* Draw Day Sections - 7 per week */
  DO v_Loop = 1 TO 7:

    RUN pdf_line  ("Spdf", 105 * v_Loop - (72),
                           33,
                            105 * v_Loop - (72),
                           pdf_PageHeight("Spdf") - 63,
                           .5).

  END. /* week sections */
  
  /* Draw Day Boxes */
  RUN pdf_stroke_fill  ("Spdf",.9,.9,.9).
  DO v_Loop = 1 TO 7:

    /* Need this to ensure that the lines meet correctly */
    IF v_Loop = 7 THEN
      RUN pdf_rect  ("Spdf", 105 * v_Loop - (72), 
                             535 ,
                             106, 
                             20, .5).
    ELSE
      RUN pdf_rect  ("Spdf", 105 * v_Loop - (72), 
                             535 ,
                             105, 
                             20, .5).
  END. /* Day Boxes */

  /* Draw The Month on the Top Left Hand Corner */
  RUN pdf_set_font("Spdf","Courier-Bold",24.0).
  RUN pdf_text_color("Spdf",0.9,0.0,0.0).
  RUN pdf_text_xy("Spdf",ENTRY(v_Month, v_MonthList) 
                         + " " + STRING(YEAR(TODAY)) ,40, 565).

  /* Draw Day Titles*/
  RUN pdf_set_font("Spdf","Courier",10.0).
  RUN pdf_text_color("Spdf",0.0,0.0,0.0).
  DO v_Loop = 1 TO 7:

    RUN pdf_text_xy  ("Spdf", 
                      ENTRY(v_Loop, v_DayList),
                      105 * v_Loop - (65),
                      540).
  END. /* Day Boxes */

  /* Determine how many Days in the Current Year/Month */
  /* Now go through each day in the Month and add to Calendar */
  v_Loop = DAY(DATE((v_Month MODULO 12) + 1, 1,
                    YEAR(TODAY) + INTEGER(TRUNCATE(v_Month / 12, 0))) - 1).
  ASSIGN v_Row = 0
         v_Y   = 520.
  DO v_Day = 1 TO v_Loop:

    v_WeekDay = WEEKDAY(DATE(v_Month,v_Day ,YEAR(TODAY))).

    IF v_WeekDay = 1 AND v_Day <> 1 THEN
      v_Row = v_Row + 1.

    /* This is to accommodate Months (like May 2004) that actually have more
       than 5 rows of days */
    IF v_Row = 5 THEN DO:
      RUN pdf_text_xy("Spdf",
                      STRING(v_Day),
                      105 * v_WeekDay - (65),
                      (v_Y - (v_Row * 100)) + 50).

      RUN pdf_line  ("Spdf", 105 * v_WeekDay - (65),
                             (v_Y - (v_Row * 100)) + 60,
                              105 * v_WeekDay - (65) + 80,
                             (v_Y - (v_Row * 100)) + 60,
                             .5).

    END. /* Row 5 */

    ELSE
      RUN pdf_text_xy("Spdf",
                      STRING(v_Day),
                      105 * v_WeekDay - (65),
                      v_Y - (v_Row * 100)).

  END. /* Show Day Numbers */

END. /* Month */

RUN pdf_close("Spdf"). */

