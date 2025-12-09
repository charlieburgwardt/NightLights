;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task applies the ImageBandDifference task which performs a difference analysis
;on two VIIRS NightLights images from different dates. 
;  * Negative values mean light intensity is greater in the baseline raster.
;  * Positive values mean light intensity is greater in the second raster.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO psgApplyNightLightsImageDifference, $
  INPUT_RASTER1 = oRaster1, $           ;specify baseline raster
  INPUT_RASTER2 = oRaster2, $
  OUTPUT_URI = fname_out_raster, $
  OUTPUT_RASTER = oImgDiffRaster

  compile_opt idl2, hidden

  if n_elements(oRaster1) eq 0 then return
  if n_elements(oRaster2) eq 0 then return

  IF ~KEYWORD_SET(fname_out_raster) THEN BEGIN
    tempURI = IDLcfGetTemporaryFile()
    fname_out_raster = filepath(root=file_dirname(tempURI), 'IMGDIFF'+file_basename(tempURI))
  ENDIF
  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( fname_out_raster )

  task = ENVITask('ImageBandDifference')
  task.INPUT_RASTER1 = oRaster1
  task.INPUT_RASTER2 = oRaster2
  task.OUTPUT_RASTER_URI = fname_out_raster
  task.Execute

  oImgDiffRaster = task.OUTPUT_RASTER

  rasterURL1 = oRaster1.URI
  if ~n_elements(rasterURL1) then begin
    rasterURL1 = 'VirtualRaster1'
  endif else begin
    rasterURL1 = ENVI_FILE_STRIP(file_basename(rasterURL1), /BACK)
  endelse
  rasterURL2 = oRaster2.URI
  if ~n_elements(rasterURL2) then begin
    rasterURL2 = 'VirtualRaster2'
  endif else begin
    rasterURL2 = ENVI_FILE_STRIP(file_basename(rasterURL2), /BACK)
  endelse

  oMetaData = oImgDiffRaster.METADATA
  if oMetaData.HasTag('DESCRIPTION') then begin
    oMetaData.UpdateItem, 'DESCRIPTION', 'Image Difference: '+rasterURL1+' and '+rasterURL2
  endif else begin
    oMetaData.AddItem, 'DESCRIPTION', 'Image Difference: '+rasterURL1+' and '+rasterURL2
  endelse
  oImgDiffRaster.METADATA = oMetaData
  oImgDiffRaster.WriteMetadata ; Update the ENVI format *.hdr file with new metadata.

  ; BUT (lesson learned here) the meta item DESCRIPTION, while written
  ; out to FNAME_OUT_RASTER is NOT accessible within this oImgDiffRaster ref.
  ; I need to call ENVIUrlRaster to build a new ENVIRaster reference.
  oImgDiffRaster.Close
  oImgDiffRaster = ENVIUrlRaster(fname_out_raster)
  
  return
END
