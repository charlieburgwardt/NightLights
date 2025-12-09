;-------------------------------------------------------------
;+
; NAME:
;       PSG_MakeN
;
; PURPOSE:
;       Make an array of N values, linear between two given limits.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;       x = makeN( first, last, num)
;
; INPUTS:
;       first, last = array start and end values.          in
;       num = number of values from first to last.         in
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       x = array of values.                               out
;-
;-------------------------------------------------------------
FUNCTION PSG_MakeN, xlo, xhi, n

  COMPILE_OPT idl2

  IF n LE 1 THEN RETURN, [xlo]	; spec. case.
  xst = (xhi-xlo)/FLOAT(n-1)	; Step size.

RETURN, xlo+xst*FINDGEN(n)
END
