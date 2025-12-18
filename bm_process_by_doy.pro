
;=============================================================
; bm_process_by_doy.pro
; Orchestrates DOY grouping and per-DOY mosaics (DNB + QF).
; Fills oRasterCollection (one ENVIRaster per DOY).
;=============================================================


PRO bm_process_by_doy, root, oRasterCollection, OUTDIR=outdir, VERBOSE=verbose
  COMPILE_OPT IDL2

  ; 1) Find VNP46A1 files
  files = FILE_SEARCH(root, 'VNP46A1*.h5')
  IF (N_ELEMENTS(files) EQ 0) THEN BEGIN
    MESSAGE, 'bm_process_by_doy: no VNP46A1 files found at: ' + root, /INFO
    oRasterCollection = OBJARR(0)
    RETURN
  ENDIF
  IF KEYWORD_SET(verbose) THEN PRINT, 'Found ', N_ELEMENTS(files), ' file(s).'

  ; 2) Group by acquisition DOY using your routine
  ;    Note: pick_and_group_by_day.pro must be compiled already.
  Group_VNP46A1_By_DOY, files, groups, VERBOSE=KEYWORD_SET(verbose)

  ngroups = N_ELEMENTS(groups)
  IF (ngroups EQ 0) THEN BEGIN
    MESSAGE, 'bm_process_by_doy: no DOY groups produced.', /INFO
    oRasterCollection = OBJARR(0)
    RETURN
  ENDIF

  ; 3) Output folder
  IF ~KEYWORD_SET(outdir) THEN outdir = FILEPATH('bm_doy_mosaics', ROOT_DIR=root)
  IF ~FILE_TEST(outdir, /DIRECTORY) THEN FILE_MKDIR, outdir

  ; 4) Build mosaics per DOY and collect as ENVIURLRaster
  oRasterCollection = OBJARR(ngroups)
  ;oRasterCollection = LIST()

  FOR j=0, ngroups-1 DO BEGIN
    doy_key = groups[j].key   ; 'YYYYDDD'
    day_dir = FILEPATH('A' + doy_key, ROOT_DIR=outdir)

    ; --- Safe cleanup: empty contents without deleting the folder ---
    IF ~FILE_TEST(day_dir, /DIRECTORY) THEN FILE_MKDIR, day_dir
    children = FILE_SEARCH(day_dir, '*', /NULL)
    IF N_ELEMENTS(children) GT 0 THEN FILE_DELETE, children, /ALLOW_NONEXISTENT, /QUIET

    arr = groups[j].files.TOARRAY()
    IF KEYWORD_SET(verbose) THEN PRINT, 'DOY A' + doy_key + ' -> ', N_ELEMENTS(arr), ' file(s)'

    final_uri = bm_viirs_dnb_qf_mosaic_from_list(arr, day_dir, DOY_KEY=doy_key, USE_UNION_GRID=1)
    oRasterCollection[j] = ENVIURLRASTER(final_uri)
    ;oRasterCollection.Add, ENVIURLRASTER(final_uri)
  ENDFOR


  ;  FOR j=0, ngroups-1 DO BEGIN
  ;    doy_key = groups[j].key                ; 'YYYYDDD'
  ;    day_dir = FILEPATH('A' + doy_key, ROOT_DIR=outdir)
  ;
  ;    ;Clean out existing data for this day and start fresh
  ;    FILE_DELETE, day_dir, /ALLOW_NONEXISTENT, /NOEXPAND_PATH, /RECURSIVE
  ;
  ;    IF ~FILE_TEST(day_dir, /DIRECTORY) THEN FILE_MKDIR, day_dir
  ;
  ;    arr = groups[j].files.TOARRAY()
  ;    IF KEYWORD_SET(verbose) THEN PRINT, 'DOY A' + doy_key + ' -> ', N_ELEMENTS(arr), ' file(s)'
  ;
  ;    final_uri = bm_viirs_dnb_qf_mosaic_from_list(arr, day_dir, DOY_KEY=doy_key, USE_UNION_GRID=1)
  ;
  ;    ; ENVIURLRaster returns an ENVIRaster object opened by URI
  ;    oRasterCollection[j] = ENVIURLRASTER(final_uri)
  ;  ENDFOR
END
