DEFINE VARIABLE objOutlook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookMsg AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookAttach AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookRecip AS COM-HANDLE NO-UNDO.
DEFINE STREAM b.
DEF VAR wsCharge AS DEC.
DEF VAR wsAmt AS DEC EXTENT 2.
DEF VAR ws AS CHAR EXTENT 2.
DEF VAR X AS INT.
DEF VAR wsrec AS CHAR FORM "x(80)".
DEF VAR wsa LIKE muncmf.acc.
DEF BUFFER tt FOR muncmf.
DEF VAR wsa1 AS CHAR FORM "x(10)".
DEF VAR rfil AS CHAR FORM "x(8)".
DEF VAR i AS INT.
DEF VAR wstype AS INT.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
FIND FIRST procof NO-ERROR.
IF co-code <> "kar" THEN DO:
    MESSAGE "Module designed specifically for Municipality of Kariba"
    VIEW-AS ALERT-BOX.
    QUIT.
END.
ELSE DO:
    OUTPUT STREAM b TO p:\results\MSTSim01.txt.
    PUT STREAM b 'ACCOUNT NUMBER|ARREARS BALANCE|CURRENT BALANCE' SKIP.
    FOR EACH muncmf WHERE SUBSTR(full-erf,1,2) = "PM":
        wsa1 = SUBSTR(full-erf,4,6).
        wsamt = 0.
        IF wsa1 <> "" THEN DO: /* additional Account */
            i = LENGTH(full-erf) - 1.
            rfil = SUBSTR(full-erf,i,-1).
            IF rfil = "/2" THEN DO:
                i = i - 4.
                wstype = 2.
                wsa = DEC(SUBSTR(full-erf,4,i)).
            END.
            ELSE 
                ASSIGN wsa = DEC(SUBSTR(full-erf,4))
                     wstype = 0.
            FIND FIRST tt WHERE tt.acc =  wsa NO-LOCK NO-ERROR.
            IF AVAILABLE tt THEN DO:
                DO X = 1 TO 6:
                    IF tt.sv-tar[X] <> 0 THEN DO:
                       FIND FIRST muntmf WHERE muntmf.TYPE = 3 AND muntmf.tarif = tt.sv-tar[X] NO-LOCK NO-ERROR.
                        wsamt[1] = wsamt[1] + ((muntmf.rate[1]* tt.sv-pts[X]) * muntmf.vat-pcnt / 100) + (muntmf.rate[1] * tt.sv-pts[X]).
                    END. /*1*/
                    IF tt.av-tar[X] <> 0 AND tt.av-pts[X] <> 0 THEN DO:       
                       FIND FIRST muntmf WHERE muntmf.TYPE = 4 AND muntmf.tarif = tt.av-tar[X] NO-LOCK NO-ERROR.
                                wsamt[1] = wsamt[1] + (muntmf.avl-amt * tt.av-pts[X])
                                         + (muntmf.avl-amt * tt.av-pts[X]) * (muntmf.vat-pcnt / 100). 
                    END. /*1*/
                END. /*2*/
            END. /*3*/
            /* Rates */
            if tt.rates-tarif > 0 AND wstype = 0 then do:
                wsCharge = 0.
                if tt.site > 0 or tt.bldg > 0 or tt.bc-val > 0 then do:
                     if tt.pcnt = 0 then tt.pcnt = 100.
                     find last muntmf where muntmf.type = 2
                            and muntmf.tarif = tt.rates-tarif no-error.
                     ASSIGN wsCharge = round(muntmf.assess[1] * tt.site,2)
                                    + round(muntmf.assess[2] * tt.bldg,2)
                                    + round(muntmf.other-a[1] * tt.site,2)
                                    + round(muntmf.other-a[2] * tt.bldg,2)
                                    + round(muntmf.other-b[1] * tt.site,2)
                                    + round(muntmf.other-b[2] * tt.bldg,2)
                                    + round(muntmf.other-c[1] * tt.site,2)
                                    + round(muntmf.other-c[2] * tt.bldg,2).
                END.
                wsAmt[1] = wsAmt[1] + wsCharge.
            END.
            /* Rental */
            IF CAN-FIND(FIRST mhhmf WHERE mhhmf.acc = tt.acc AND mhhmf.s-or-r = "R") THEN DO:
             wsCharge = 0.
                FIND LAST mhhmf WHERE mhhmf.acc = tt.acc AND mhhmf.s-or-r = "R" NO-LOCK NO-ERROR.
                IF AVAILABLE mhhmf THEN DO:
                    FIND FIRST mhsdf WHERE mhsdf.scheme = mhhmf.scheme NO-LOCK NO-ERROR.
                    do X = 1 to 10:
                      if mhhmf.amt[X] ne 0 then do:
                         wscharge = wscharge + mhhmf.amt[X].
                         if mhsdf.vat[X] > 0 THEN
                            assign wscharge = wscharge + ( truncate((mhhmf.amt[X]
                                                    * mhsdf.vat[X] / 100),2)).
                      end.
                   end.
                END.
                wsAmt[1] = wsAmt[1] + wsCharge.
            END.
        END.
        DO X = 1 TO 6:
            IF muncmf.sv-tar[X] <> 0 THEN DO:
               FIND FIRST muntmf WHERE muntmf.TYPE = 3 AND muntmf.tarif = muncmf.sv-tar[X] NO-LOCK NO-ERROR.
                wsamt[1] = wsamt[1] + ((muntmf.rate[1] * muncmf.sv-pts[X]) * muntmf.vat-pcnt / 100) + (muntmf.rate[1] * muncmf.sv-pts[X]).
            END.
            IF muncmf.av-tar[X] <> 0 AND muncmf.av-pts[X] <> 0 THEN DO:       
               FIND FIRST muntmf WHERE muntmf.TYPE = 4 AND muntmf.tarif = muncmf.av-tar[X] NO-LOCK NO-ERROR.
                        wsamt[1] = wsamt[1] + (muntmf.avl-amt * muncmf.av-pts[X])
                                 + (muntmf.avl-amt * muncmf.av-pts[X]) * (muntmf.vat-pcnt / 100). 
            END.
        END.
        /* Rates */
        if muncmf.rates-tarif > 0 AND wstype = 0 then do:
            wsCharge = 0.
            if muncmf.site > 0 or muncmf.bldg > 0 or muncmf.bc-val > 0 then do:
                 if muncmf.pcnt = 0 then muncmf.pcnt = 100.
                 find last muntmf where muntmf.type = 2
                        and muntmf.tarif = muncmf.rates-tarif no-error.
                 ASSIGN wsCharge = round(muntmf.assess[1] * muncmf.site,2)
                                + round(muntmf.assess[2] * muncmf.bldg,2)
                                + round(muntmf.other-a[1] * muncmf.site,2)
                                + round(muntmf.other-a[2] * muncmf.bldg,2)
                                + round(muntmf.other-b[1] * muncmf.site,2)
                                + round(muntmf.other-b[2] * muncmf.bldg,2)
                                + round(muntmf.other-c[1] * muncmf.site,2)
                                + round(muntmf.other-c[2] * muncmf.bldg,2).
            END.
            wsAmt[1] = wsAmt[1] + wsCharge.
        END.
        /* Rental */
        IF CAN-FIND(FIRST  mhhmf WHERE mhhmf.acc = muncmf.acc AND mhhmf.s-or-r = "R") THEN DO:
            wsCharge = 0.
            FIND LAST mhhmf WHERE mhhmf.acc = muncmf.acc AND mhhmf.s-or-r = "R" NO-LOCK NO-ERROR.
            IF AVAILABLE mhhmf THEN DO:
                FIND FIRST mhsdf WHERE mhsdf.scheme = mhhmf.scheme NO-LOCK NO-ERROR.
                
                do X = 1 to 10:
                  if mhhmf.amt[X] ne 0 then do:
                     wscharge = wscharge + mhhmf.amt[X].
                     if mhsdf.vat[X] > 0 THEN
                        assign wscharge = wscharge + ( truncate((mhhmf.amt[X]
                                                * mhsdf.vat[X] / 100),2)).
                  end.
               end.
            END.
            wsAmt[1] = wsAmt[1] + wsCharge.
        END.
        IF wsa = 0 THEN
            FOR EACH munbmf WHERE munbmf.acc = muncmf.acc AND munbmf.TYPE <> wstype:
                DO X = 1 TO 14:
                   ASSIGN wsAmt[2] = wsAmt[2] + munbmf.bal[X] + interest[X].
                END.
            END.
        ELSE 
            FOR EACH munbmf WHERE munbmf.TYPE <> wstype AND 
                (munbmf.acc = muncmf.acc OR munbmf.acc = wsa ):
                DO X = 1 TO 14:
                   ASSIGN wsAmt[2] = wsAmt[2] + munbmf.bal[X] + interest[X].
                END.
            END.
        IF wsAmt[2] < 0 THEN
           ASSIGN wsAmt[1] = ROUND((wsAmt[1] + wsAmt[2]),2)
                  wsAmt[2] = 0.00.
        IF wsAmt[1] < 0 THEN
           wsAmt[1] = 0.00.
        DO X = 1 TO 2:
           ws[X] = STRING(substring(string(wsAmt[X]),index(string(wsAmt[X]),".") + 1,2)).
           IF index(string(wsAmt[X]),".") = 0 THEN
               ws[X] = "00".
           IF LENGTH(ws[X]) < 2 THEN
                ws[X] = ws[X] + "0".
        END.
        wsrec = STRING(muncmf.acc) + "|"
                  + string(substring(string(wsAmt[2]),1,index(string(wsAmt[2]),".") - 1))
                  + "," + ws[2] + "|"
                  + string(substring(string(wsAmt[1]),1,index(string(wsAmt[1]),".") - 1))
                  + "," + ws[1].
            PUT STREAM b wsrec SKIP.
    END.
    OUTPUT STREAM b CLOSE.
    /* send email to entries in incparm.incvalue for MST */
    FIND FIRST incparm WHERE incparm.inclabel = "MST" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE incparm THEN DO: 
         MESSAGE "MST email not setup, please resolve this"
                VIEW-AS ALERT-BOX.
         QUIT. 
    END.
    ELSE
    REPEAT X = 1 TO NUM-ENTRIES(incparm.incvalue):
        CREATE "Outlook.Application" objOutlook.
        objoutlookMsg = objOutlook:CreateItem(0).
        objOutlookRecip = objOutlookMsg:Recipients:Add(ENTRY(x,incparm.incvalue)) /* "dsithole@aus.co.zw" transactfile@utilitiesworld.co.za */.
        objOutlookRecip:Type = 1.
        objOutlookMsg:Subject = "Kariba File Extract".
        objOutlookMsg:Body = "Please find attached the file extract for Kariba".
        
        objOutlookMsg:Attachments:Add("p:\results\MSTSim01.TXT").
        objOutlookRecip:Resolve.
        objOutlookMsg:Send.
        objoutlook:Quit().
        
        RELEASE OBJECT objOutlook.
        RELEASE OBJECT objOutlookMsg.
        RELEASE OBJECT objOutlookRecip. 
    END.
END.
 
