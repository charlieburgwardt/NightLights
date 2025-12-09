;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
; NightLights_2CMV_ChangeDetection Task
; 
; :Description:
;   This metatask executes the following VIIRS Nightlights tasks:
;   *  BuildNightLightsBandStackTask
;   *  PSGFilterVIIRSNightLightsTask
; 
;   A collection of change detection images are then created using the 
;   csgTwoColorMultiView task, where the first raster in data series is the
;   baseline image and each successive raster in series is applied as the 
;   second image in each call to this task.
; 
;   The final product of this metatask is an ENVIRasterSeries containing all
;   2CMV raster datasets that can be displayed in multiple ENVIViews or as a
;   Band Animation movie.
; 
; :Keywords:
;   INPUT_DATASET_FOLDER {input}{required}
;     Input directory folder location of the HDF5 datasets to import.
;     
;   INPUT_ROI {input}{optional}
;     Input ROI XML file to determine AOI region.
;     
;-----------------------------------------------------------------------------
PRO NightLights_2CMV_ChangeDetection, $
  INPUT_DATASET_FOLDER = dataResource, $
  INPUT_ROI = input_roi_xml, $
  OUTPUT_URI = output_cmvseries, $     ;output filename for CMV raster series
  OUTPUT_CMV_SERIES = CMVSeriesRaster  ;rasterseries displayed in ELT
  
  COMPILE_OPT idl2

  e = ENVI(/CURRENT)
  oDataColl = e.DATA
  oView = e.GetView()

  ;NOTE: I already built the VIIRS DNB HDF5 data import template found here:
  ;C:\Users\kellie.mcnaron\.idl\envi\custom_code6_1\viirs_dnb.xml - Template Name: VIIRS DNB'
  psgInputVIIRSNightLightsData, $
    INPUT_DATASET_FOLDER = dataResource, $
    OUTPUT_RASTERS = oRasterCollection  ;return array of envirasters

  psgBuildVIIRSNightLightsBandStack, $
    INPUT_RASTERS = oRasterCollection, $
    INPUT_ROI = input_roi_xml, $
    OUTPUT_REPROJECT_SERIES = reprojSeriesRaster, $  ;return the reprojected band stack
    OUTPUT_CLOUDMASK_SERIES = oCloudMaskSeries       ;2025Nov10 - this is a RasterSeries
    
  if ~obj_valid(reprojSeriesRaster) then return
  nfiles = reprojSeriesRaster.Count

  validCloudMask = 0
  if obj_valid(oCloudMaskSeries) then validCloudMask = 1
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ; Build filtered rasters to suppress "dark" pixels to help with noise/clutter...
;  psgBuildMetaspectralRasterFromSeries, $
;    INPUT_RASTERSERIES = reprojSeriesRaster, $
;    OUTPUT_RASTER = oReprojCollection
;    
;  validCloudMask = 0
;  if obj_valid(oCloudMaskSeries) then begin
;    validCloudMask = 1
;    psgBuildMetaspectralRasterFromSeries, $
;      INPUT_RASTERSERIES = oCloudMaskSeries, $
;      OUTPUT_RASTER = oCloudMaskCollection
;  endif

  ;;Retrieve all the rasters in the series and calculate statistics on each day's info.
  ;rStats = ENVIRasterStatistics(oReprojCollection)
  ;threshValForCollection = MEAN(rStats.MEAN) + 0.2*MEAN(rStats.STDDEV)

  oFilterCollection = objarr(nfiles)
  for cc=0,nfiles-1 do begin
    ;WARNING: About retrieving rasters from an ENVIRasterSeries - the raster objref
    ;         will only contain a basic Metadata objref with just BAND NAMES key in it.
    ;         This kind of screws up my attempts to retain the original HDF5 fileinfo that
    ;         I've kept in the Metadata key DATASET NAMES... How can I fix this?
    ;
    ; I'm going to have to define a new ENVIRaster reference using ENVIUrlRaster...STUPID!
    reprojSeriesRaster.Set, cc
    otmpraster = reprojSeriesRaster.Raster
    reprojRaster = ENVIUrlRaster(otmpraster.URI)

    if validCloudMask then begin
      oCloudMaskSeries.Set, cc
      ocldtemp = oCloudMaskSeries.Raster
      oCloudRaster = ENVIUrlRaster(otmpraster.URI)
    endif

    ;reprojRaster = oReprojCollection[cc]
    ;if validCloudMask then oCloudRaster = oCloudMaskCollection[cc]  ;2025Nov10
    
    ;NOTE: input threshold value foreach raster will be customized using raster stats...
    psgFilterVIIRSNightLightsTask, $
      INPUT_RASTER = reprojRaster, $     ;specify raster to threshold
      INPUT_CLOUDMASK = oCloudRaster, $  ;(optional) cloud mask to include as additional filter
      THRESHOLD = threshValForCollection, $ ;common threshold value for raster collection
      OUTPUT_RASTER = oFiltrRaster 
    
    oFilterCollection[cc] = oFiltrRaster
  endfor

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Quick-and-dirty test with 2-color multiview task...
  oCMVRasterCollection = LIST()
  for rr=1,nfiles-1 do begin
    ;fname_2cmv = IDLcfGetTemporaryFile()
    ;CMV between "baseline" raster and successive days...
    csgTwoColorMultiView, $
      INPUT_RASTER1=oFilterCollection[0], $
      INPUT_RASTER2=oFilterCollection[rr], $
      ;OUTPUT_URI=fname_2cmv, $
      OUTPUT_RASTER=oCMVRaster
      
    oCMVRasterCollection.Add, oCMVRaster
  endfor
  
  if ~n_elements(output_cmvseries) then begin
    tempURI = IDLcfGetTemporaryFile(ext='series')
    output_cmvseries = filepath(root=file_dirname(tempURI),'CMV_'+file_basename(tempURI))
  endif
  ;Output file must contain a .series extension - check for this...
  fileExt = envi_file_strip(file_basename(output_cmvseries), /extension)
  if ~strcmp(fileExt, 'series') then begin
    if strlen(fileExt) eq 0 then begin
      output_cmvseries = filepath(root=file_dirname(output_cmvseries), file_basename(output_cmvseries)+'.series')
    endif else begin
      output_cmvseries = filepath(root=file_dirname(output_cmvseries), file_basename(output_cmvseries, '.'+fileExt)+'.series')
    endelse
  endif
  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( output_cmvseries )

  task = ENVITask('BuildRasterSeries')
  task.INPUT_RASTERS = oCMVRasterCollection.ToArray()
  task.OUTPUT_RASTERSERIES_URI = output_cmvseries
  task.Execute
  CMVSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, CMVSeriesRaster

  return
END