;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task creates a new raster where values BELOW a specified threshold are set to 0.
;Removes "dark" pixels to help with clutter/noise. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO psgFilterVIIRSNightLightsTask, $
  INPUT_RASTER = oRaster, $           ;(required) specify raster to threshold
  INPUT_CLOUDMASK = oCloudMask, $     ;(optional) specify cloud mask to include as additional filter
  THRESHOLD = threshold, $            ;(optional) specify an array of threshold values (one per band)
  CLOUD_THRESH = cloudMaskThreshold, $ ;default is 242.
  OUTPUT_URI = fname_out_raster, $
  OUTPUT_RASTER = oFilteredRaster     ;reference to the output raster of filetype ENVI

  compile_opt idl2, hidden

  if n_elements(oRaster) eq 0 then return

  IF ~KEYWORD_SET(fname_out_raster) THEN BEGIN
    tempURI = IDLcfGetTemporaryFile()
    fname_out_raster = filepath(root=file_dirname(tempURI), 'FILTERED_'+file_basename(tempURI))
  ENDIF
  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( fname_out_raster )

  rasterDims = [oRaster.NCOLUMNS, oRaster.NROWS]
  outData = fltarr(rasterDims[0], rasterDims[1])

  if keyword_set(threshold) then begin
    threshVal = FLOAT(threshold)
  endif else begin
    stats = envirasterstatistics(oRaster) 
    threshVal = stats.MEAN + 0.15*stats.STDDEV
  endelse
  ;print, threshVal

  ;Set dark pixels to 0...
  data = oRaster.GetData(BAND=0)   ;one band at a time please...
  ddx = where( data gt threshVal[0], nlights )
  outData[ddx] = data[ddx]


  ;2025Nov10 - check for cloudmask input. remove pixels where cloudmask data is gt 242...
  if keyword_set(oCloudMask) and obj_valid(oCloudMask) then begin
    if ~keyword_set(cloudMaskThreshold) then cloudMaskThreshold = 242.
    cloudMaskData = oCloudMask.GetData(BAND=0)
    cldIdx = where( cloudMaskData gt float(cloudMaskThreshold), nclouds )
    if (nclouds gt 0) then begin
      excludeCloudPixels = psg_ut_setintersection(cldIdx, ddx, index=removeThese )
      if excludeCloudPixels[0] ne -1 then begin
        outData[excludeCloudPixels] = 0.0  ;set these obscured pixels back to zero...
      endif
    endif
  endif


  ;Some Metadata information please...
  thisMetadata = oRaster.METADATA
  bname = thisMetadata['BAND NAMES']
  if thisMetadata.HasTag('DATASET NAMES') then begin
    urlName = thisMetadata['DATASET NAMES']
  endif else begin
    urlName = oRaster.URI
    if n_elements(urlName) gt 0 then begin
      urlName = ENVI_FILE_STRIP(file_basename(urlName), /BACK)
    endif else $
      urlName = 'Virtual'
  endelse
  outMeta = ENVIRasterMetadata()
  outMeta.AddItem, 'BAND NAMES', bname ;[urlName]
  outMeta.AddItem, 'DESCRIPTION', 'Filtered NightLights'
  outMeta.AddItem, 'DATASET NAMES', urlName

  oSpatialRef = oRaster.SPATIALREF
  oFilteredRaster = ENVIRaster(outData, URI=fname_out_raster, $
    METADATA=outMeta, SPATIALREF=oSpatialRef)
  ;Call to SAVE here closes the raster for writing & converts it to read-only mode.
  oFilteredRaster.SAVE
  ; BUT (lesson learned here) the meta items DESCRIPTION and DATASET NAMES, while written
  ; out to FNAME_OUT_RASTER are NOT accessible/acknowledged within this oFilteredRaster ref.
  ; I need to call ENVIUrlRaster to build a new ENVIRaster reference.
  oFilteredRaster.Close
  oFilteredRaster = ENVIUrlRaster(fname_out_raster)

  return
END
