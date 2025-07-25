FOR EACH cbtrans:
    DELETE cbtrans.
END.
FOR EACH cbkmf:
    DISPLAY bank bal. PAUSE 0.
   bal = 0.
END.
FOR EACH glbal.
    DELETE glbal.
END.
FOR EACH gltdf.
    DELETE gltdf.
END.
