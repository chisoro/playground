/* Program.................ordprn01.i
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
FIND FIRST ordmf WHERE ordmf.OrdNo = wsid NO-LOCK NO-ERROR.
FIND FIRST crdmf WHERE crdmf.acc = ordmf.Acc NO-LOCK NO-ERROR.
wsfile = repDir + "Ord" + string(ordmf.OrdNo) + ".pdf".

RUN pdf_new("Spdf",wsfile).
RUN pdf_set_orientation("Spdf","PORTRAIT").
RUN pdf_load_image ("Spdf", "mygif", "c:\simacc\bin\logo.png").

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
  RUN pdf_skip("Spdf").
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

 
