;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task builds a "change detection" classification map
;
;The returned product is an ENVI Classification Map raster, defining 3 classes:
;
; Persistent ROI = pixel addr where both rasters contain non-zero data.
; Red Fled ROI = pixel addr where raster1 contain positive value but raster2 is zero.
; Blue New ROI = pixel addr where raster2 contain positive value but raster1 is zero.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO psgTransientNightLightsMapTask, $
  INPUT_RASTER1 = oRaster1, $           ;specify baseline raster
  INPUT_RASTER2 = oRaster2, $
  OUTPUT_URI = fname_classmap, $
  OUTPUT_RASTER = oChangeDetClassMap    ;3 ENVIRois: "Persistent", "Red Fled", "Blue New"  

  compile_opt idl2

  if n_elements(oRaster1) eq 0 then return
  if n_elements(oRaster2) eq 0 then return

  IF ~KEYWORD_SET(fname_classmap) THEN BEGIN
    tempURI = IDLcfGetTemporaryFile()
    fname_classmap = filepath(root=file_dirname(tempURI), 'CHGMAP_'+file_basename(tempURI))
  ENDIF
  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( fname_classmap )

  ospatialref = oRaster1.spatialref
  dims = [oRaster1.ncolumns, oRaster1.nrows]

  iDataFrom1 = oRaster1.GetData(BAND=0)
  iDataFrom2 = oRaster2.GetData(BAND=0)
  pixArr1 = where( iDataFrom1 gt 0.0, npix1 )
  pixArr2 = where( iDataFrom2 gt 0.0, npix2 )

  ;Find locations where both rasters have non-zero pixels (persistent)
  persistentTMP = psg_ut_setintersection( pixArr1, pixArr2, index=tdx )
  ppersistent2d = array_indices(dims, persistentTMP, /DIM)

  oPersistentROI = ENVIRoi( NAME='Persistent Lights', COLOR='yellow' )
  oPersistentROI.AddPixels, ppersistent2d, spatialref=oSPATIALREF
  oPersistentROI.Pixelate, oRaster1

  ;Find locations where raster1 is positive (on) but raster2 is zero (off)
  redfledTMP = psg_ut_setdifference( pixArr1, pixArr2 )
  predfled2d = array_indices(dims, redfledTMP, /DIM)

  oRedFledROI = ENVIRoi( NAME='Red Fled Lights', COLOR='red' )
  oRedFledROI.AddPixels, predfled2d, spatialref=oSPATIALREF
  oRedFledROI.Pixelate, oRaster1

  ;Find locations where raster2 is positive (on) but raster1 is zero (off)
  bluenewTMP = psg_ut_setdifference( pixArr2, pixArr1 )
  pbluenew2d = array_indices(dims, bluenewTMP, /DIM)

  oBlueNewROI = ENVIRoi( NAME='Blue New Lights', COLOR='blue' )
  oBlueNewROI.AddPixels, pbluenew2d, spatialref=oSPATIALREF
  oBlueNewROI.Pixelate, oRaster1

  oChangeDetROIArray = [oPersistentROI,oRedFledROI,oBlueNewROI]
  
  ;Convert to Classification Map...
  task = ENVITask('ROIToClassification')
  task.INPUT_ROI = oChangeDetROIArray
  task.INPUT_RASTER = oRaster1
  task.OUTPUT_RASTER_URI = fname_classmap
  task.Execute
  
  oChangeDetClassMap = task.OUTPUT_RASTER

  ;Some Metadata information please...
  thisMetadata1 = oRaster1.METADATA
  if thisMetadata1.HasTag('DATASET NAMES') then begin
    rasterURL1 = thisMetadata1['DATASET NAMES']
  endif else begin
    rasterURL1 = oRaster1.URI
    if ~n_elements(rasterURL1) then begin
      rasterURL1 = 'VirtualRaster1'
    endif else begin
      rasterURL1 = ENVI_FILE_STRIP(file_basename(rasterURL1), /BACK)
    endelse
  endelse
  thisMetadata2 = oRaster2.METADATA
  if thisMetadata2.HasTag('DATASET NAMES') then begin
    rasterURL2 = thisMetadata2['DATASET NAMES']
  endif else begin
    rasterURL2 = oRaster2.URI
    if ~n_elements(rasterURL2) then begin
      rasterURL2 = 'VirtualRaster2'
    endif else begin
      rasterURL2 = ENVI_FILE_STRIP(file_basename(rasterURL2), /BACK)
    endelse
  endelse

  oMetaData = oChangeDetClassMap.METADATA
  if oMetaData.HasTag('DESCRIPTION') then begin
    oMetaData.UpdateItem, 'DESCRIPTION', 'Persistent vs Transient NightLights'
  endif else begin
    oMetaData.AddItem, 'DESCRIPTION', 'Persistent vs Transient NightLights'
  endelse
  if oMetaData.HasTag('BAND NAMES') then begin
    oMetaData.UpdateItem, 'BAND NAMES', rasterURL2
  endif else begin
    oMetaData.AddItem, 'BAND NAMES', rasterURL2
  endelse
  oChangeDetClassMap.METADATA = oMetaData
  oChangeDetClassMap.WriteMetadata ; Update the ENVI format *.hdr file with new metadata.

  ; BUT (lesson learned here) the meta item DESCRIPTION, while written
  ; out to FNAME_OUT_RASTER is NOT accessible within this oChangeDetClassMap ref.
  ; I need to call ENVIUrlRaster to build a new ENVIRaster reference.
  oChangeDetClassMap.Close
  oChangeDetClassMap = ENVIUrlRaster(fname_classmap)

  foreach oroi, oChangeDetROIArray do oroi.CLOSE
  
  return
END
