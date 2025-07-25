/*Program:...............................dbChdTa.p
Description:.............................Program to check accounts with double tarifs set(pre-billing)
Author:......................................S. Chisoro
Date:..........................................06/02/24*/
DEF VAR X AS INT.
DEF VAR i AS INT.
DEF VAR j AS INT.
OUTPUT TO c:\simacc\reports\dabtorsRep.csv.
FOR EACH dbcmf:
    i = 0.
    j = 0.
    DO X = 1 TO 14:
        i = tarif[X].
        IF i <> 0 THEN DO:
            IF i = j THEN
                EXPORT DELIMITER ',' dbAcc i.
            j = i. 
        END.
         
    END.
   
END.
OS-COMMAND NO-WAIT VALUE("c:\simacc\reports\dabtorsRep.csv").
