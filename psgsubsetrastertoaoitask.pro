;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task creates a new raster. 
;
;NOTE: Because the input raster uses a GLT spatial ref, the output subset raster must
;      remain as a virtual raster - exporting to a file will not retain the map info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION psgSubsetRasterToAOITask, $
  INPUT_RASTER = oRaster, $          
  INPUT_ROI = input_roi_xml, $
  ERROR_STAT = estat

  compile_opt idl2, hidden

  estat = 1

  if n_elements(oRaster) eq 0 then return, 0

  e = envi(/current)
  oAOIRegion = e.OpenRoi(input_roi_xml)
  aoiExtent = oAOIRegion.GetExtent(oRaster)
  
  ;IMPORTANT! Keep it a virtual raster so that GLT spatialref is inherited from oRaster
  oSubset = envisubsetraster(oRaster, SUB_RECT=aoiExtent)  ;subset ALL bands in oRaster
  
  ;TODO - Set up the Metadata to include original URI in the DATASET NAMES
  thisMetadata = oSubset.METADATA

  urlName = oRaster.URI
  if n_elements(urlName) gt 0 then begin
    urlName = ENVI_FILE_STRIP(file_basename(urlName), /BACK)
  endif else urlName = 'Virtual'

  nBands = oSubset.NBANDS
  bandnames = strarr(nBands)+urlName
  if thisMetadata.HasTag('BAND NAMES') then begin
    thisMetadata.UpdateItem, 'BAND NAMES', bandnames  ;'Band: '+urlName
  endif else begin
    thisMetadata.AddItem, 'BAND NAMES', bandnames     ;[urlName]
  endelse
  if thisMetadata.HasTag('DESCRIPTION') then begin
    thisMetadata.UpdateItem, 'DESCRIPTION', 'Subset Raster to AOI Extent'
  endif else begin
    thisMetadata.AddItem, 'DESCRIPTION', 'Subset Raster to AOI Extent'
  endelse
  if thisMetadata.HasTag('DATASET NAMES') then begin
    thisMetadata.UpdateItem, 'DATASET NAMES', bandnames ;urlName
  endif else begin
    thisMetadata.AddItem, 'DATASET NAMES', bandnames    ;urlName
  endelse
  oSubset.METADATA = oMetaData
  ;NOTE! I can't call WriteMetadata here b/c oSUBSET is a Virtual raster...
  ;oSubset.WriteMetadata ; Update the ENVI format *.hdr file with new metadata.
  ;oSubset.Close
  ;oSubset = ENVIUrlRaster(output_uri)

  estat = 0
  return, oSubset
END
