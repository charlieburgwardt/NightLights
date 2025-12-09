;+
; NAME:
;   PSG_UT_SETDIFFERENCE
;
; PURPOSE:
;   Given two arrays of positive values, A and B, this routine will
;   determine which elements in A are not in B
;
; CALLING SEQUENCE:
;   ddx = PSG_UT_SetDifference(A,B)
;
; REQUIRED INPUTS:
;   A - First matrix to compare.
;   B - Second matrix to compare.
;
; OPTIONAL KEYWORDS:
;   None.
;
; OUTPUTS:
;   Returns the values in A that are not in B.
;
; DEPENDENCIES:
;   None.
;-
;-------------------------------------------------------------------------
FUNCTION PSG_UT_SetDifference, a, b
  compile_opt idl2

   ; = a and (not b) = elements in A but not in B

  mina = Min(a, Max=maxa)
  minb = Min(b, Max=maxb)
  IF (minb GT maxa) OR (maxb LT mina) THEN RETURN, a ;No intersection...
  r = Where((Histogram(a, Min=mina, Max=maxa) NE 0) AND $
          (Histogram(b, Min=mina, Max=maxa) EQ 0), count)
  IF count eq 0 THEN RETURN, -1 ELSE RETURN, r + mina
END