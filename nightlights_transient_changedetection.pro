;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
; NightLights_Transient_ChangeDetection Task
; 
; :Description:
;   This metatask executes the following VIIRS Nightlights tasks:
;   *  BuildNightLightsBandStackTask
;   *  psgFilterVIIRSNightLightsTask
;   *  psgTransientNightLightsMapTask
;
;   The final product of this metatask is an ENVIRasterSeries containing all
;   HeatMap raster datasets that can be displayed in multiple ENVIViews or as a
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
PRO NightLights_Transient_ChangeDetection, $
  INPUT_DATASET_FOLDER = dataResource, $
  INPUT_ROI = input_roi_xml, $
  OUTPUT_URI = output_uri, $       ;output filename for heatmap raster series
  OUTPUT_HEATMAP_SERIES = ChangeDetSeriesRaster  ;rasterseries displayed in ELT
  
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

  ; Build a raster series for this collection of envirasters...
  tempURI = IDLcfGetTemporaryFile(ext='series')
  output_filtered_uri = filepath(root=file_dirname(tempURI),'FLTR_'+file_basename(tempURI))

  task = ENVITask('BuildRasterSeries')
  task.INPUT_RASTERS = oFilterCollection
  task.OUTPUT_RASTERSERIES_URI = output_filtered_uri
  task.Execute
  filteredSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, filteredSeriesRaster

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Build collection of image difference rasters where 1st raster is "baseline"...
  oImageDiffCollection = LIST()
  for rr=1,nfiles-1 do begin
    psgTransientNightLightsMapTask, $
      INPUT_RASTER1 = oFilterCollection[0], $   ;specify raster to threshold
      INPUT_RASTER2 = oFilterCollection[rr], $
      OUTPUT_RASTER = oChangeDetClassMap  ;3 ENVIRois: "Persistent", "Red Fled", "Blue New"

    oImageDiffCollection.Add, oChangeDetClassMap
  endfor
  ndays = oImageDiffCollection.COUNT()

  if ~n_elements(output_uri) then begin
    tempURI = IDLcfGetTemporaryFile(ext='series')
    output_uri = filepath(root=file_dirname(tempURI),'MAP_'+file_basename(tempURI))
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

  task = ENVITask('BuildRasterSeries')
  task.INPUT_RASTERS = oImageDiffCollection.ToArray()
  task.OUTPUT_RASTERSERIES_URI = output_uri
  task.Execute
  ChangeDetSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, ChangeDetSeriesRaster

  ;Set up Band Animation for a movie...
  oSeriesLayer = oView.createlayer(ChangeDetSeriesRaster)

  return
END