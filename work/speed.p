DEF VAR wsa AS DEC FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR X AS INT.
DEF VAR a AS INT.
DEF VAR wsAge LIKE dbmtf.Amt EXTENT 4.
DEF VAR wsCr LIKE dbmtf.Amt.
DEF VAR varDue LIKE dbmtf.Amt.
a = ETIME(YES).
DEF TEMP-TABLE wsT LIKE dbblf.
FOR EACH dbcmf NO-LOCK:
wsa = 0.
    FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK:
      
        CREATE wsT.
        BUFFER-COPY dbblf TO wsT.
    END.
END.

FOR EACH wsT.

    ASSIGN wsCr = wsCr + wsT.amt[1] WHEN wsT.amt[1] < 0
                   wsage[1] = wsage[1] + wsT.amt[1] WHEN wsT.amt[1] > 0
                   wsage[2] = wsage[2] + wsT.amt[2]
                   wsage[3] = wsage[3] + wsT.amt[3] + wsT.INT.
            DO X = 4 TO 15:
                wsage[4] = wsage[4] + wsT.amt[X].
            END.
            varDue = wsCr + wsage[1] + wsage[2] + wsage[3] + wsage[4].

        /*DO X = 1 TO 15:
            wsa = wsa + wsT.amt[X].
        END.*/

        DISPLAY  wsCr wsage[1] wsage[2] wsage[3] wsage[4] varDue.
        PAUSE 0.
END.
DISPLAY ETIME.

