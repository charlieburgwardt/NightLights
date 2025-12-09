;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task creates a "change detection"-like classification heatmap based on the
;contents from an ImageBandDifference raster product.
;  * Negative values mean light intensity is greater in the baseline raster. 
;  * Positive values mean light intensity is greater in the following raster.
;  
;The task basically separates the "positive" and "negative" pixel values and builds
;two distinct classification ranges, and where the "negative" class range is 
;colored shades of RED and the "positive" class range is colored in shades of BLUE.
; 
;This task uses the ColorSliceClassification task.
;
;The returned product is a LIST containing the collection of HeatMap classification 
;rasters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO psgBuildNightLightsHeatmapTask, $
  INPUT_RASTERSERIES = oImgDiffSeries, $
  OUTPUT_HEATMAP=oHeatMapCollection  ;LIST

  compile_opt idl2

  nfiles = oImgDiffSeries.Count

  ;Retrieve all the rasters in the series and calculate statistics on each day's info.
  oImageDiffCollection = LIST()
  foreach otmpraster, oImgDiffSeries do oImageDiffCollection.Add, otmpraster
  oDiffBandStack = ENVIMetaspectralRaster(oImageDiffCollection.ToArray())
  diffStats = ENVIRasterStatistics(oDiffBandStack)

  number_of_ranges = 8

  ;In order to keep the class range vals consistent between each day's HeatMap product,
  ;I need to determine a reasonable data range to apply to all image difference rasters...
  ;
  ;1) first, class range for positive values...
  gstats_min1 = 0.001
  gstats_mean1 = MAX(diffStats.mean)
  gstats_stddev1 = MAX(diffStats.stddev)
  gstats_max1 = MAX(diffStats.mean) + 5.0*MAX(diffStats.stddev)
  classRngVals1 = PSG_MakeN(gstats_min1[0], gstats_mean1[0]+2.0*gstats_stddev1[0], number_of_ranges+1)
  classRngVals1[number_of_ranges] = gstats_max1[0]
  class_ranges1 = FLTARR(2,number_of_ranges)
  FOR nn=1,number_of_ranges DO class_ranges1[*,nn-1] = [classRngVals1[nn-1],classRngVals1[nn]]

  ;Define the color table to use in the heatmap...
  ;ctable = 8B & reverseFlag=0 & cind=[100, 235] ; Green
  ctable = 1B & reverseFlag=0 & cind=[100, 235] ; Blue
  class_colors1 = COLORTABLE(ctable, /TRANSPOSE, REVERSE=reverseFlag)
  cIndices = FLOOR(PSG_MakeN(cind[0], cind[1], number_of_ranges))
  class_colors1 = class_colors1[*,cIndices]

  ;2) second, class range for negative values...
  gstats_min2 = MIN(diffStats.mean) - 2.0*MAX(diffStats.stddev)
  gstats_max2 = -0.001
  classRngVals2 = PSG_MakeN(gstats_min2[0], gstats_max2, number_of_ranges+1)
  classRngVals2[0] = MIN(diffStats.mean) - 5.0*MAX(diffStats.stddev)
  class_ranges2 = FLTARR(2,number_of_ranges)
  FOR nn=1,number_of_ranges DO class_ranges2[*,nn-1] = [classRngVals2[nn-1],classRngVals2[nn]]

  ;Define the color table to use in the heatmap...
  ctable = 3B & reverseFlag=0 & cind=[100, 220] ; Red
  ;ctable = 7B & reverseFlag=0 & cind=[100, 220] ; Red-Purple
  class_colors2 = COLORTABLE(ctable, /TRANSPOSE, REVERSE=reverseFlag)
  cIndices = FLOOR(PSG_MakeN(cind[0], cind[1], number_of_ranges))
  class_colors2 = class_colors2[*,cIndices]

  ;3) Combine the two ranges for use in ColorSliceClassification task...
  class_ranges = [[class_ranges2],[class_ranges1]]
  class_colors = [[REVERSE(class_colors2,2)],[class_colors1]]

  ;Build a HeatMap classification raster for each ImageDifference dataset...
  ENVI_REPORT_INIT, 'Building HeatMap...', base=base, title='NightLight Changes'
  ENVI_REPORT_INC, base, nfiles

  rr=0
  oHeatMapCollection = LIST()
  foreach oraster, oImgDiffSeries do begin
    rr++
    ENVI_REPORT_STAT, base, rr, nfiles

    dims = [oraster.ncolumns, oraster.nrows]
    idata = oraster.getdata(band=0)
    ospatialref = oraster.spatialref

    ;;capture all pixels with values NOT set to 0.0 and build an ROI...
    pdx = where( float(idata) ne 0.0, ncnt )
    oROI = ENVIROI(name='New From Baseline', color='red')
    pix2d = array_indices(dims, pdx, /DIM)
    oROI.AddPixels, pix2d, spatialref=oSPATIALREF
    oROI.Pixelate, oraster

    ;; Now build a mask raster to be used in call to ColorSliceClassification...
    oTempMask = ENVIRoiMaskRaster(oraster, oROI)

    ;; Now build a heatmap using ColorSliceClassification task...
    tempURI = IDLcfGetTemporaryFile(ext='dat')
    output_uri = filepath(root=file_dirname(tempURI),'DAY'+strtrim(rr,2)+'_'+file_basename(tempURI))
    ok = fstk_check_output_filename(output_uri)

    sTask = ENVITask('ColorSliceClassification')
    sTask.INPUT_RASTER = oTempMask
    sTask.CLASS_RANGES = class_ranges
    sTask.CLASS_COLORS = class_colors
    sTask.OUTPUT_RASTER_URI = output_uri
    sTask.Execute

    clrSliceClassRaster = sTask.OUTPUT_RASTER
    
    ;Some Metadata information please...
    thisMetadata = oRaster.METADATA
    if thisMetadata.HasTag('DATASET NAMES') then begin
      rasterURL = thisMetadata['DATASET NAMES']
    endif else begin
      rasterURL = oRaster.URI
      if ~n_elements(rasterURL) then begin
        rasterURL1 = 'VirtualRaster'
      endif else begin
        rasterURL = ENVI_FILE_STRIP(file_basename(rasterURL), /BACK)
      endelse
    endelse

    oMetaData = clrSliceClassRaster.METADATA
    if oMetaData.HasTag('DESCRIPTION') then begin
      oMetaData.UpdateItem, 'DESCRIPTION', 'NightLights Change Detection HeatMap'
    endif else begin
      oMetaData.AddItem, 'DESCRIPTION', 'NightLights Change Detection HeatMap'
    endelse
    if oMetaData.HasTag('BAND NAMES') then begin
      oMetaData.UpdateItem, 'BAND NAMES', rasterURL
    endif else begin
      oMetaData.AddItem, 'BAND NAMES', rasterURL
    endelse
    clrSliceClassRaster.METADATA = oMetaData
    clrSliceClassRaster.WriteMetadata ; Update the ENVI format *.hdr file with new metadata.
    clrSliceClassRaster.Close
    
    oColorSliceRaster = ENVIURLRaster(output_uri, SPATIALREF_OVERRIDE=oSpatialRef)
    oHeatMapCollection.Add, oColorSliceRaster

    oROI.Close
  endforeach
  ENVI_REPORT_INIT, base=base, /finish

  return
END
