;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
; psgBuildVIIRSNightLightsBandStack Task
;
; :Description:
;   This task retrieves collection of HDF5 data files in specfied folder location and performs
;   the following tasks to prepare raster data for processing:
;   1) Imports selected dataset field(s) from the HDF5 and builds collection of ENVIRaster refs.
;   2) If keyword INPUT_ROI is set, every HDF5 raster is subsetted to selected AOI region.
;   3) Each georeferenced ENVIRaster is then reprojected from GLT SpatialRef to Standard SpatialRef.
;
;   This task returns an ENVIRasterSeries object containing collection of
;   georeferenced ENVIRasters, one for each HDF5 dataset listed in input directory folder.
;
; :Keywords:
;   INPUT_RASTERS {input}{required}
;     Array of ENVIRasters created from importing HDF5 datasets.
;
;   INPUT_ROI {input}{optional}
;     Input ROI XML file to determine AOI region.
;
;-----------------------------------------------------------------------------
PRO psgBuildVIIRSNightLightsBandStack, $
  INPUT_RASTERS = oRasterCollection, $
  ;INPUT_ROI = input_roi_xml, $
  OUTPUT_URI = output_uri, $                       ;optional, but extension must be ".series" if set
  OUTPUT_REPROJECT_SERIES = reprojSeriesRaster, $  ;return the reprojected band stack
  OUTPUT_CLOUDMASK_SERIES = oCloudMaskCollection   ;return reprojected cloudmask band stack, was oCloudMaskSeries

  COMPILE_OPT idl2

  e = ENVI(/CURRENT)
  oDataColl = e.DATA

  nfiles = n_elements(oRasterCollection)
  if nfiles eq 0 then return

  ;Band 1 = DNB_At_Sensor_Radiance
  ;Band 2 = QF_Cloud_Mask
  if keyword_set(input_roi_xml) then begin
    ;NOTE1: The input XML must have extents in PIXEL coordinates b/c oRasterCollection has GLT ref
    ;NOTE2: Because input raster(s) will have GLT spatialref I have to keep output oSubset
    ;       as a virtual raster!
    PRINT, 'An AOI has been provided. Building subsetted rasters...'
    oSubsetCollection = objarr(nfiles)
    for rr=0,nfiles-1 do begin
      oSubset = psgSubsetRasterToAOITask( $
        INPUT_RASTER = oRasterCollection[rr], $
        INPUT_ROI = input_roi_xml, $
        ERROR_STAT = estat )

      ;NOTE2: oSubset is a VIRTUAL raster, meaning there is no URL filename for reference!
      ;       But this task does store the original filename as the band name and in
      ;       "dataset names" which is where PSGReprojectGLTtoStandardSpatialRef below
      ;       is going to pick up the original filename for its oMetadata info.
      oSubsetCollection[rr] = oSubset
    endfor
    oRasterCollection = oSubsetCollection
    oDataColl.Add, oSubsetCollection
  endif

  ;================================= Charlie B. 12/11/2025 ==================================
  ;Raster collection already reprojected to Standard map reference during mosaicing process in:
  ;bm_process_by_day.pro
  ;pick_and_group_by_day.pro
  ;bm_viirs_dnb_qf_mosaic_from_list.pro
  ;==========================================================================================
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 2) Reproject raster collection from GLT to Standard map reference...
  ENVI_REPORT_INIT, 'Reprojecting Rasters...', base=base, title='NightLights Reprojection'
  ENVI_REPORT_INC, base, nfiles

  oReprojectedRaster = objarr(nfiles)
  ;2025Nov10 - check to see if oRasterCollection rasters have 2nd band defining cloud mask...
  if (oRasterCollection[0]).NBANDS gt 1 then begin
    oCloudMaskCollection = objarr(nfiles)
  endif



  for rr=0,nfiles-1 do begin
    ENVI_REPORT_STAT, base, rr+1, nfiles
    psgReprojectGLTtoStandardSpatialRef, $
      INPUT_RASTER=oRasterCollection[rr], $
      OUTPUT_RASTER=oReprojRaster, $
      OUTPUT_CLOUDMASK = oCloudMaskRaster   ;2025Nov10 - NEW

    oReprojectedRaster[rr] = oReprojRaster
    if obj_valid(oCloudMaskRaster) then oCloudMaskCollection[rr] = oCloudMaskRaster ;2025Nov10
  endfor



  ENVI_REPORT_INIT, base=base, /finish

  
  if ~n_elements(output_uri) then begin
    output_uri = IDLcfGetTemporaryFile(ext='series')
  endif
  ;Output file must contain a .series extension - check for this...
  fileExt = envi_file_strip(file_basename(output_uri), /extension)
  if ~strcmp(fileExt, 'series') then begin
    if strlen(fileExt) eq 0 then begin
      output_uri = filepath(root=file_dirname(output_uri), file_basename(output_uri)+'.series')
    endif else begin
      output_uri = filepath(root=file_dirname(output_uri), file_basename(output_uri, '.'+fileExt)+'.series')
    endelse
  endif
  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( output_uri )

  ;Build a raster series for this collection of envirasters...
  task = ENVITask('BuildRasterSeries')
  task.INPUT_RASTERS = oReprojectedRaster
  task.OUTPUT_RASTERSERIES_URI = output_uri
  task.Execute
  reprojSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, reprojSeriesRaster


  return
END