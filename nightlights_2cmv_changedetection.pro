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
  ;INPUT_ROI = input_roi_xml, $
  OUTPUT_FOLDER = output_folder, $     ;output filename for CMV raster series
  OUTPUT_CMV_SERIES = CMVSeriesRaster  ;rasterseries displayed in ELT

  COMPILE_OPT idl2

  e = ENVI(/CURRENT)
  IF ~OBJ_VALID(e) THEN e = ENVI()

  ; Build the mosaics (bm)
  bm_process_by_doy, dataResource, oRasterCollection, OUTDIR=FILEPATH('bm_doy_mosaics', ROOT_DIR=dataResource), VERBOSE=1

  oDataColl = e.DATA
  oView = e.GetView()

  nfiles = n_elements(oRasterCollection)
  if nfiles eq 0 then return

  oDNB_RasterCollection = objarr(nfiles)
  ;2025Nov10 - check to see if oRasterCollection rasters have 2nd band defining cloud mask...
  if (oRasterCollection[0]).NBANDS gt 1 then begin
    oCloudMaskCollection = objarr(nfiles)
  endif

  tempURI = IDLcfGetTemporaryFile()
  fname_reproj = filepath(root=file_dirname(tempURI), 'REPROJ_'+file_basename(tempURI))

  ; Do only what is needed from psgReprojectGLTtoStandardSpatialRef
  for rr=0,nfiles-1 do begin

    oRaster = oRasterCollection[rr]
    imgData = oRaster.GetData(BAND=0)


    ;Some Metadata information please...
    bname = oRaster.METADATA['BAND NAMES']
    urlName = oRaster.URI
    if n_elements(urlName) gt 0 then begin
      urlName = ENVI_FILE_STRIP(file_basename(urlName), /BACK)
    endif else begin
      tempMeta = oRaster.METADATA
      if tempMeta.HasTag('DATASET NAMES') then begin
        urlName = tempMeta['DATASET NAMES']
      endif else urlName = 'Virtual'
    endelse
    urlName = urlName[0]  ;just in case oRaster includes 2nd band for cloud mask...
    outMeta = ENVIRasterMetadata()
    outMeta.AddItem, 'BAND NAMES', [urlName]
    outMeta.AddItem, 'DESCRIPTION', 'Ortho-Rectified from GLT SpatialRef'
    outMeta.AddItem, 'DATASET NAMES', urlName

    tempURI = IDLcfGetTemporaryFile()
    ;fname_reproj = filepath(root=file_dirname(tempURI), 'REPROJ_'+file_basename(tempURI))
    fname_reproj = filepath(root=file_dirname(tempURI), 'REPROJ_'+urlName.replace('_QF_', '_'))

    imgData = oRaster.GetData(BAND=0)   ;oRaster already reprojected, see note above
    ; Create a new raster
    oSpatialRef = oRaster.SPATIALREF
    oReprojRaster = ENVIRaster(imgData, URI=fname_reproj,  METADATA=outMeta, SPATIALREF=oSpatialRef)
    oReprojRaster.SAVE
    oReprojRaster.Close
    oReprojRaster = ENVIUrlRaster(fname_reproj)

    ;oReprojectedRaster[rr] = oReprojRaster
    oDNB_RasterCollection[rr] = oReprojRaster


    if oRaster.NBANDS eq 2 then begin
      imgData = oRaster.GetData(BAND=1)  ;CLOUD MASK
      urlName = urlName[0]  ;just use the zeroith urlName
      outCloudMeta = ENVIRasterMetadata()
      outCloudMeta.AddItem, 'BAND NAMES', [urlName]
      outCloudMeta.AddItem, 'DESCRIPTION', 'CloudMask Ortho-Rectified from GLT SpatialRef'
      outCloudMeta.AddItem, 'DATASET NAMES', urlName

      tempURI = IDLcfGetTemporaryFile()
      ;fname_cloud_reproj = filepath(root=file_dirname(tempURI), 'CLOUD_'+file_basename(tempURI))
      fname_cloud_reproj = filepath(root=file_dirname(tempURI), 'CLOUD_'+urlName.replace('_DNB_', '_'))

      ; Get cloud bits
      cloud_status_bits = UINT(imgData) AND 192
      oCloudMaskRaster = ENVIRaster( cloud_status_bits, URI=fname_cloud_reproj, $
        METADATA=outCloudMeta, SPATIALREF=oSpatialRef)

      oCloudMaskRaster.SAVE
      oCloudMaskRaster.Close
      oCloudMaskRaster = ENVIUrlRaster(fname_cloud_reproj)
    endif

    if obj_valid(oCloudMaskRaster) then oCloudMaskCollection[rr] = oCloudMaskRaster ;2025Nov10

  endfor

  ; Clean out the series folder at this time
  ; --- Safe cleanup: empty contents without deleting the folder ---
  IF ~FILE_TEST(output_folder, /DIRECTORY) THEN FILE_MKDIR, day_dir
  children = FILE_SEARCH(output_folder, '*', /NULL)
  IF N_ELEMENTS(children) GT 0 THEN FILE_DELETE, children, /ALLOW_NONEXISTENT

  ;=================== Use Valid pixels now ===============================


  oBaseCloudRaster = oCloudMaskCollection[0]  ; Filter the reference image


  ;  reprojRaster = oDNB_RasterCollection[0]   ; Filter the reference image
  ;  psgFilterVIIRSNightLightsTask, $
  ;    INPUT_RASTER = reprojRaster, $     ;specify raster to threshold
  ;    INPUT_CLOUDMASK = oCloudRaster, $  ;(optional) cloud mask to include as additional filter
  ;    THRESHOLD = threshValForCollection, $ ;common threshold value for raster collection
  ;    OUTPUT_RASTER = oFilteredBaseRaster

  ; Move the filtered base file and cloud file to the series folder - do this later
  ;  newFile = file_basename(reprojRaster.URI)
  ;  newFile = newfile.replace('REPROJ', 'FILTERED_BASE')
  ;  newFile = filepath(root=output_folder, newFile)
  ;  oFilteredBaseRaster.Export, newfile, 'ENVI'
  ;  oFilteredBaseRaster = ENVIURLRaster(newFile)
  ;
  ;  newFile = file_basename(oCloudRaster.URI)
  ;  newFile = newfile.replace('CLOUD', 'BASE_CLOUD')
  ;  newFile = filepath(root=output_folder, newFile)
  ;  oCloudRaster.Export, newfile, 'ENVI'
  ;  oCloudRaster = ENVIURLRaster(newFile)

  oCMVRasterCollection = LIST()
  oLocalCldMaskCollection = LIST()
  for rr=1,nfiles-1 do begin

    ;    oCloudRaster = oCloudMaskCollection[rr]
    ;    reprojRaster = oDNB_RasterCollection[rr]

    ;    psgFilterVIIRSNightLightsTask, $
    ;      INPUT_RASTER = reprojRaster, $     ;specify raster to threshold
    ;      INPUT_CLOUDMASK = oCloudRaster, $  ;(optional) cloud mask to include as additional filter
    ;      THRESHOLD = threshValForCollection, $ ;common threshold value for raster collection
    ;      OUTPUT_RASTER = oFilteredRaster
    ;
    ;    ; Move the filtered file and cloud file to the series folder
    ;    newFile = file_basename(reprojRaster.URI)
    ;    newFile = newfile.replace('REPROJ', 'FILTERED')
    ;    newFile = filepath(root=output_folder, newFile)
    ;        oFilteredRaster.Export, newfile, 'ENVI'
    ;    oFilteredRaster = ENVIURLRaster(newFile)
    ;
    ;    newFile = file_basename(oCloudRaster.URI)
    ;    newFile = filepath(root=output_folder, newFile)
    ;    oCloudRaster.Export, newfile, 'ENVI'
    ;    oCloudRaster = ENVIURLRaster(newFile)


    
    ; Run CMV
    csgTwoColorMultiView, $
      INPUT_RASTER1=oDNB_RasterCollection[0], $
      INPUT_RASTER2=oDNB_RasterCollection[rr], $
      OUTPUT_RASTER=cloudy_CMVRaster

    ; Build invalid mask from cloud rasters
;    cloudMaskThreshold   = 0  ;Was 64
;    baseCloudMaskData    = oCloudMaskCollection[0].GetData(BAND=0)
;    currentCloudMaskData = oCloudMaskCollection[rr].GetData(BAND=0)
;    invalidMask = (baseCloudMaskData gt float(cloudMaskThreshold)) OR $
;      (currentCloudMaskData gt float(cloudMaskThreshold))

    ; Gray out invalid pixels in CMV RGB output
;    cmvData = cloudy_CMVRaster.GetData()   ; [ns, nl, 3]
;    grayVal = 0B    ; Was 128B
;    FOR b=0,2 DO BEGIN
;      band = cmvData[*,*,b]
;      band[invalidMask] = grayVal
;      cmvData[*,*,b] = band
;    ENDFOR


rasterDims = [oRaster.NCOLUMNS, oRaster.NROWS]
outData = fltarr(rasterDims[0], rasterDims[1])

if keyword_set(threshold) then begin
  threshVal = FLOAT(threshold)
endif else begin
  stats = envirasterstatistics(oRaster)
  threshVal = stats.MEAN + 0.15*stats.STDDEV
endelse
;print, threshVal


data = oRaster.GetData(BAND=0)   ;one band at a time please...
ddx = where( data gt threshVal[0], nlights )
outData[ddx] = data[ddx]

; --- Build cloud union indices as above ---
cloudMaskThreshold   = 64.0
baseCloudMaskData    = oCloudMaskCollection[0].GetData(BAND=0)
currentCloudMaskData = oCloudMaskCollection[rr].GetData(BAND=0)

baseCldIdx    = WHERE(baseCloudMaskData    GT cloudMaskThreshold, nBase)
currentCldIdx = WHERE(currentCloudMaskData GT cloudMaskThreshold, nCurr)
IF nBase LE 0 THEN baseCldIdx = LONG([])
IF nCurr LE 0 THEN currentCldIdx = LONG([])

cloudIdxUnion = UNION(baseCldIdx, currentCldIdx)  ;From Beaumont library
excludeCloudPixels = cloudIdxUnion

;excludeCloudPixels = psg_ut_setintersection(cloudIdxUnion, ddx, index=removeThese )  ;Try this

; --- Fetch CMV cube and apply gray ---
cmvData = cloudy_CMVRaster.GetData()  ; [ns, nl, 3], likely FLOAT (type code 4)

IF N_ELEMENTS(excludeCloudPixels) GT 0 THEN BEGIN
  grayVal = 0.5    ; float gray; use 0.0 for black, or 128B for byte types

  FOR b=0, 2 DO BEGIN
    band = cmvData[*,*,b]
    band[excludeCloudPixels] = grayVal
    cmvData[*,*,b] = band
  ENDFOR
ENDIF

; --- Persist changes back to the raster (or create a new one) ---
;CATCH, err
;IF err EQ 0 THEN BEGIN
;  cloudy_CMVRaster.SetData, cmvData
;ENDIF ELSE BEGIN
;  CATCH, /CANCEL
;  env = ENVI()
;  newURI = 'cloudy_cmv_gray.tif'
;  newRaster = env.CreateRaster(cmvData, URI=newURI, NB=3, DATA_TYPE=4, $   ; 4 = float
;    SPATIALREF=cloudy_CMVRaster.SPATIALREF)
;ENDELSE




    cmvFile = file_basename(oDNB_RasterCollection[rr].URI)
    cmvFile = cmvFile.replace('REPROJ', 'CMV')
    cmvFile = filepath(root=output_folder, cmvFile)
    
    CMVRaster=e.CreateRaster(cmvFile, cmvData, INHERITS_FROM=cloudy_CMVRaster)
    CMVRaster.Save
    
    oCMVRasterCollection.Add, CMVRaster
    
    ; Save CloudMask to local directory
    oCloudRaster = oCloudMaskCollection[rr]
    cldFile = file_basename(oCloudRaster.URI)
    cldFile = filepath(root=output_folder, cldFile)
    oLocalCldMaskCollection.Add, oCloudRaster

  endfor

  STOP

  ; Output the series files and the CMV images to a local directory, not temporary
  ;OUTPUT_URI = output_cmvseries, $     ;output filename for CMV raster series
  ;OUTPUT_FOLDER = output_folder, $     ; Location of rasters in series and the series .series file.
  ;OUTPUT_CMV_SERIES = CMVSeriesRaster  ;rasterseries displayed in ELT

  if ~n_elements(output_cmvseries) then begin
    ;tempURI = IDLcfGetTemporaryFile(ext='series')
    ;output_cmvseries = filepath(root=file_dirname(tempURI),'CMV_'+file_basename(tempURI))
    ;output_cmvseries = filepath(root=output_folder,'CMV_'+file_basename(tempURI))
    output_cmvseries = filepath(root=output_folder, 'VNP46A1_DNB.series')

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

  if n_elements(oCloudMaskCollection) gt 0 then begin
    ;========================== Charlie B. 12/5/2025 ==========================
    ;Output file must contain a .series extension - check for this...
    ;Add "_cld" to file name to designate a cloud mask series
    output_cloudseries_uri = output_cmvseries.Replace('.series', '_cloud.series')

    task = ENVITask('BuildRasterSeries')
    ;task.INPUT_RASTERS = oCloudMaskCollection
    task.INPUT_RASTERS = oLocalCldMaskCollection.ToArray()
    task.OUTPUT_RASTERSERIES_URI = output_cloudseries_uri
    task.Execute
    oCloudMaskSeries = task.OUTPUT_RASTERSERIES
    oDataColl.Add, oCloudMaskSeries
  endif

  STOP

  return
END