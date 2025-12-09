;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
; NightLights_HeatMap_ChangeDetection Task
; 
; :Description:
;   This metatask executes the following VIIRS Nightlights tasks:
;   *  BuildNightLightsBandStackTask
;   *  PSGFilterVIIRSNightLightsTask
;   *  PSGApplyImageDifferenceTask
;   *  psgBuildNightLightsHeatmapTask
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
PRO NightLights_HeatMap_ChangeDetection, $
  INPUT_DATASET_FOLDER = dataResource, $
  INPUT_ROI = input_roi_xml, $
  OUTPUT_URI = output_uri, $       ;output filename for heatmap raster series
  OUTPUT_HEATMAP_SERIES = HeatMapSeriesRaster  ;rasterseries displayed in ELT
  
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
  ; Build collection of image difference rasters where 1st raster is "baseline" raster...
  oImageDiffCollection = LIST()
  for rr=1,nfiles-1 do begin
    psgApplyNightLightsImageDifference, $
      INPUT_RASTER1 = oFilterCollection[0], $      ;specify baseline raster
      INPUT_RASTER2 = oFilterCollection[rr], $
      OUTPUT_RASTER = oImgDiffRaster

    oImageDiffCollection.Add, oImgDiffRaster
  endfor

  tempURI = IDLcfGetTemporaryFile(ext='series')
  output_diff_uri = filepath(root=file_dirname(tempURI),'IMGDIFF_'+file_basename(tempURI))
  ok = FSTK_Check_Output_Filename( output_diff_uri )

  task = ENVITask('BuildRasterSeries')
  task.INPUT_RASTERS = oImageDiffCollection.ToArray()
  task.OUTPUT_RASTERSERIES_URI = output_diff_uri
  task.Execute
  DifferenceSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, DifferenceSeriesRaster


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Task to build collection of HeatMap classmaps for every day compared against baseline data...
  psgBuildNightLightsHeatmapTask, $
    INPUT_RASTERSERIES = DifferenceSeriesRaster, $
    OUTPUT_HEATMAP=oHeatMapCollection  ;LIST

  if ~n_elements(output_uri) then begin
    tempURI = IDLcfGetTemporaryFile(ext='series')
    output_uri = filepath(root=file_dirname(tempURI),'HeatMap_'+file_basename(tempURI))
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
  task.INPUT_RASTERS = oHeatMapCollection.ToArray()
  task.OUTPUT_RASTERSERIES_URI = output_uri
  task.Execute
  HeatMapSeriesRaster = task.OUTPUT_RASTERSERIES
  oDataColl.Add, HeatMapSeriesRaster

  ;Set up Band Animation for a movie...
  oSeriesLayer = oView.createlayer(HeatMapSeriesRaster)
      
  return
END