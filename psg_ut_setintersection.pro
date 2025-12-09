;-------------------------------------------------------------------------
;+
; NAME:
;   UT_SETINTERSECTION
;
; PURPOSE:
;   Given two arrays of positive values, A and B, this routine will
;   determine which elements in A are also in B.
;
; CALLING SEQUENCE:
;   ddx = WP_UT_SetIntersecton(A, B [,INDEX=ndx])
;
; REQUIRED INPUTS:
;   A - First matrix to compare.
;   B - Second matrix to compare.
;
; OPTIONAL KEYWORDS:
;   NDX - The positional indices in A where the common values are found.
;
; OUTPUTS:
;   Returns the values in A that are also in B.
;
; DEPENDENCIES:
;   None.
;-
;-------------------------------------------------------------------------
FUNCTION PSG_UT_SetIntersection, a, b, index = rdx

  COMPILE_OPT idl2
  
  minab = Min(a, Max=maxa) > Min(b, Max=maxb) ;Only need intersection of ranges
  maxab = maxa < maxb

   ; If either set is empty, or their ranges don't intersect: result = NULL.

  IF maxab LT minab OR maxab LT 0 THEN RETURN, -1

  adx = WHERE( a GE minab AND a LE maxab, aCNT )
  IF aCNT EQ 0 THEN RETURN, -1
  a2 = a[adx]
  bdx = WHERE( b GE minab AND b LE maxab, bCNT )
  IF bCNT EQ 0 THEN RETURN, -1
  b2 = b[bdx]

  ; Make sure a and b contain unique elements.
  ia = UNIQ( a2, SORT(a2) );
  ib = UNIQ( b2, SORT(b2) );
  a2  = a2[ia]
  b2  = b2[ib]

  ; Find matching entries
  c   = [a2,b2]
  ndx = SORT(c)
  c   = c[ndx]
  d   = WHERE( c[0:N_ELEMENTS(c)-2] EQ c[1:N_ELEMENTS(c)-1], count )

  IF count EQ 0 THEN RETURN, -1

  ndx = ndx[ [d,d+1] ]
  c = c[d]
  n = N_ELEMENTS(a2)

  d = ndx > n

  rdx = adx[ ia[ ndx[WHERE(ndx NE d)] ] ]
RETURN, c
END
