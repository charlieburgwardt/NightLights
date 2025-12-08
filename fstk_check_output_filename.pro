FUNCTION FSTK_Check_Output_Filename, fname
  ; Returns flag: 0 - filename is bad (can't delete), 1 - file name is good, 2 - file name has been modified
  
  COMPILE_OPT idl2
  
  ; error status

  e=ENVI(/CURRENT)
  IF ~OBJ_VALID(e) THEN e=ENVI(/HEADLESS)

  IF ~FILE_TEST(fname) THEN RETURN, 1
  
  ; If we get here, then file already exists. Since ENVI will not allow
  ; overwriting an existing file, I need to either delete the current file
  ; or return a new file name.
  
  oDataColl = e.DATA
  currentRasters = oDataColl.Get(/RASTER)
  IF (N_ELEMENTS(currentRasters) gt 0) THEN BEGIN
    FOR cc=0,N_ELEMENTS(currentRasters)-1 DO BEGIN
      IF (currentRasters[cc].URI ne !null) THEN BEGIN
        currentFile = currentRasters[cc].URI
        IF STRCMP(currentFile, fname, /FOLD) THEN BEGIN
          oDataColl.Remove, currentRasters[cc]
          success = IDLcf$DeleteFiles( RASTER=(currentRasters[cc])._COMPONENT, /DESTROY )
          BREAK   ;don't need to check anymore object
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
  
  IF FILE_TEST(fname) gt 0 THEN BEGIN
    ; File still exists. Try to forceably delete it...
    auxiliary = ENVI_FILE_STRIP( fname, /BACK ) + '.hdr'
    FILE_DELETE, fname, /QUIET, /ALLOW_NONEXISTENT
    FOR cc=0,N_ELEMENTS(auxiliary)-1 DO FILE_DELETE, auxiliary[cc], /QUIET, /ALLOW_NONEXISTENT
  ENDIF
  
  ; One more time...
  IF FILE_TEST(fname) THEN BEGIN
    ;Rename the damn thing...
    tmpName = STRSPLIT(FILE_BASENAME(IDLcfGetTemporaryFile()), 'IDLcfTempFile', /EXTRACT)
    fname = ENVI_FILE_STRIP(fname, /BACK) + '_' + tmpName[1]
    RETURN, 2
  ENDIF ELSE RETURN, 1
  
  END
  