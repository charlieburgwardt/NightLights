
;=============================================================
; pick_and_group_by_day.pro
; Group VNP46A1 filenames by acquisition day-of-year (YYYYDDD)
; Uses ONLY the first AYYYYDDD right after the product short name.
;=============================================================

;-------------------------------------------------------------
; Get_AcqYYYYDDD
; Extract the 7-digit acquisition string (YYYYDDD) that appears
; immediately after "VNP46A1.A" in the BASE filename.
; Returns '' if not found.
;-------------------------------------------------------------
FUNCTION Get_AcqYYYYDDD, path
  COMPILE_OPT IDL2

  ; Work on the base filename only (avoid picking up digits from directories).
  base = (STRSPLIT(path, PATH_SEP(), /EXTRACT))[-1]

  ; Posix ERE via STREGEX:
  ;   ^VNP46A1\.A(\d{7})
  ; Capture group 1 = YYYYDDD
  ;pattern = '^VNP46A1\.A(\d{7})'
  pattern = '^VNP46A1\.A([0-9]{7})'

  ; Use STREGEX with /SUBEXPR and /EXTRACT to pull the capture(s).
  ; Returns array: [full_match, subexpr_1, subexpr_2, ...]
  parts = STREGEX(base, pattern, /SUBEXPR, /EXTRACT)
  ; If no match, STREGEX returns -1 (or empty) depending on keywords.
  IF (N_ELEMENTS(parts) LT 2) THEN RETURN, ''

  RETURN, parts[1]   ; first capture: YYYYDDD
END


;-------------------------------------------------------------
; Group_VNP46A1_By_DOY
; Groups full file paths into a structure array:
;   groups[j] = { key:'YYYYDDD', files:LIST() }
;
; Keywords:
;   OUTDIR    - if set, writes per-day text files: AYYYYDDD.txt
;   VERBOSE   - if set, prints progress and counts
;   WRITE_CSV - if set (with OUTDIR), writes groups_master.csv
;-------------------------------------------------------------
PRO Group_VNP46A1_By_DOY, files, groups, OUTDIR=outdir, VERBOSE=verbose, WRITE_CSV=write_csv
  COMPILE_OPT IDL2

  ; Default output (empty)
  groups = { key:'', files:LIST() }

  ; Guard: no input files
  IF (N_ELEMENTS(files) EQ 0) THEN BEGIN
    MESSAGE, 'Group_VNP46A1_By_DOY: No input files.', /INFO
    RETURN
  ENDIF

  n = N_ELEMENTS(files)
  IF KEYWORD_SET(verbose) THEN PRINT, 'Group_VNP46A1_By_DOY: input files = ', n

  ; Extract acquisition keys
  acq_keys = STRARR(n)
  valid    = INTARR(n)

  FOR i=0, n-1 DO BEGIN
    k = Get_AcqYYYYDDD(files[i])
    acq_keys[i] = k
    valid[i]    = (k NE '')
  ENDFOR

  vix = WHERE(valid EQ 1, vcount)
  IF (vcount EQ 0) THEN BEGIN
    MESSAGE, 'Group_VNP46A1_By_DOY: No valid "VNP46A1.AYYYYDDD" found.', /INFO
    RETURN
  ENDIF

  files_v = files[vix]
  acq_v   = acq_keys[vix]
  IF KEYWORD_SET(verbose) THEN PRINT, 'Valid VNP46A1 matches = ', vcount

  ; Sort for UNIQ
  srt          = SORT(acq_v)
  acq_sorted   = acq_v[srt]
  files_sorted = files_v[srt]

  uniq_ix   = UNIQ(acq_sorted)
  uniq_keys = acq_sorted[uniq_ix]
  m         = N_ELEMENTS(uniq_keys)

  IF (m EQ 0) THEN BEGIN
    MESSAGE, 'Group_VNP46A1_By_DOY: No unique acquisition keys.', /INFO
    RETURN
  ENDIF

  ; Build the output structure (LIST for flexible grouping)
  groups = REPLICATE({ key:'', files:LIST() }, m)

  ; Ensure OUTDIR exists if provided
  IF KEYWORD_SET(outdir) THEN BEGIN
    IF ~FILE_TEST(outdir, /DIRECTORY) THEN FILE_MKDIR, outdir
  ENDIF

  ; Populate groups and optionally write per-day lists
  FOR j=0, m-1 DO BEGIN
    mj = WHERE(acq_sorted EQ uniq_keys[j], cnt)

    groups[j].key = uniq_keys[j]
    IF (cnt GT 0) THEN groups[j].files = LIST(files_sorted[mj]) ELSE groups[j].files = LIST()

    IF KEYWORD_SET(verbose) THEN PRINT, 'A' + groups[j].key + ': ', groups[j].files.COUNT(), ' file(s)'

    ; Write AYYYYDDD.txt (one path per line)
    IF (KEYWORD_SET(outdir) AND (groups[j].files.COUNT() GT 0)) THEN BEGIN
      outname = FILEPATH('A' + groups[j].key + '.txt', ROOT_DIR=outdir)
      OPENW, lun, outname, /GET_LUN
      cnt_list = groups[j].files.COUNT()
      FOR ii=0, cnt_list-1 DO PRINTF, lun, groups[j].files[ii]
      FREE_LUN, lun
    ENDIF
  ENDFOR

  ; Optional master CSV
  IF (KEYWORD_SET(outdir) AND KEYWORD_SET(write_csv)) THEN BEGIN
    master = FILEPATH('groups_master.csv', ROOT_DIR=outdir)
    OPENW, mlun, master, /GET_LUN
    PRINTF, mlun, 'YYYYDDD,Year,DOY,Filename'
    FOR j=0, m-1 DO BEGIN
      yyyy = STRMID(groups[j].key, 0, 4)
      doy  = STRMID(groups[j].key, 4, 3)
      arr  = groups[j].files.TOARRAY()
      FOR ii=0, N_ELEMENTS(arr)-1 DO PRINTF, mlun, groups[j].key + ',' + yyyy + ',' + doy + ',' + arr[ii]
    ENDFOR
    FREE_LUN, mlun
    IF KEYWORD_SET(verbose) THEN PRINT, 'Master CSV written: ', master
  ENDIF
END


;-------------------------------------------------------------
; Example_Run
; Search a directory and group.
; Adjust 'root' and filename pattern as needed.
;-------------------------------------------------------------
PRO Example_Run
  COMPILE_OPT IDL2

  root = 'C:\Users\charlie.burgwardt\OneDrive - NV5\NightLights\Benin_DNB_Data'
  ; Let's try it with one grid per file
  ;root = 'C:\Users\charlie.burgwardt\OneDrive - NV5\NightLights\Crimea_VIIRS_NightLights_HDFs\Crimea_VIIRS_NightLights_HDFs\2024Sep01-15\2024Sep01-15

  ; Target only VNP46A1 files; use /SUBDIRS if recursion is desired
  files = FILE_SEARCH(root, 'VNP46A1*.h5')

  PRINT, 'Example_Run: FILE_SEARCH found ', N_ELEMENTS(files), ' file(s).'
  IF (N_ELEMENTS(files) EQ 0) THEN BEGIN
    PRINT, 'No files found. Check the path or filename pattern.'
    RETURN
  ENDIF

  ; Output directory for lists and CSV
  outdir = FILEPATH('lists', ROOT_DIR=root)
  IF ~FILE_TEST(outdir, /DIRECTORY) THEN FILE_MKDIR, outdir

  ; Group and write outputs
  Group_VNP46A1_By_DOY, files, groups, OUTDIR=outdir, VERBOSE=1, WRITE_CSV=1

  ; Console summary
  FOR j=0, N_ELEMENTS(groups)-1 DO BEGIN
    yyyy = STRMID(groups[j].key, 0, 4)
    doy  = STRMID(groups[j].key, 4, 3)
    PRINT, '=== Acquisition: Year ', yyyy, ' DOY ', doy, ' (A' + groups[j].key + ') ==='
    cnt_list = groups[j].files.COUNT()
    FOR k=0, cnt_list-1 DO PRINT, '  ', groups[j].files[k]
  ENDFOR

  PRINT, 'Per-day lists and master CSV are under: ', outdir

;STOP
END
