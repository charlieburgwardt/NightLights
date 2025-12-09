;
; This task compares imagery collected over the same area at two different times, 
; and highlights features that have changed between the two collections.
; 
; The output is a 3-band raster composite depicting a colorized "before and after"  
; scenario, where band 1 is copy of the first input raster and bands 2 & 3 are copies 
; of the second input raster. This output raster, when displayed as an image, shows areas  
; in the scene that are "new" in blue/cyan color (i.e. "Blue New") and areas that are  
; significantly different (or changed) in the scene are colored red (i.e "Red Fled"). 
;
PRO csgTwoColorMultiView, $
  INPUT_RASTER1 = oRaster1, $   ;assumes only one band
  INPUT_RASTER2 = oRaster2, $   ;assumes only one band
  OUTPUT_URI = fname_2cmv, $
  OUTPUT_RASTER = oCMVRaster
  
  compile_opt idl2
  
  e = envi(/current)
  oData = e.DATA

  IF ~KEYWORD_SET(fname_2cmv) THEN BEGIN
    tmpsuffix = STRSPLIT(FILE_BASENAME(IDLcfGetTemporaryFile()), 'IDLcfTempFile', /EXTRACT)
    fname_2cmv = FILEPATH( 'csg'+tmpsuffix[1]+'_2cmv.dat', /TMP )
  ENDIF
  ok = FSTK_Check_Output_Filename( fname_2cmv )

  ;ensure the two rasters have same spatial dimensions...
  if (oRaster1.NCOLUMNS ne oRaster2.NCOLUMNS) || (oRaster1.NROWS ne oRaster2.NROWS) then return

  iimage = oRaster2.GetData(BAND=[0]) 
  iimage = [ [[iimage]], [[iimage]], [[iimage]] ]  ;[nx,ny,3]
  iimage[*,*,0] = oRaster1.GetData(BAND=[0])

  outMeta = ENVIRasterMetadata()
  outMeta.AddItem, 'DESCRIPTION', 'Two-Color MultiView Raster'

  bname1 = oRaster1.METADATA['BAND NAMES']
  bname1 = bname1[0]  ;just in case oRaster1 is multi-band...
  urlName1 = oRaster1.URI
  urlName1 = ENVI_FILE_STRIP(file_basename(urlName1), /BACK)

  bname2 = oRaster2.METADATA['BAND NAMES']
  bname2 = bname2[0] ;just in case oRaster2 is multi-band...
  urlName2 = oRaster2.URI
  urlName2 = ENVI_FILE_STRIP(file_basename(urlName2), /BACK)
  
  outMeta.AddItem, 'BAND NAMES', $
    ['Band 1 ('+bname1+': '+urlName1+')', $
     'Band 2 ('+bname2+': '+urlName2+')', $
     'Band 3 ('+bname2+': '+urlName2+')']
     
  oCMVRaster = ENVIRaster(iimage, METADATA=outMeta, $
    URI=fname_2cmv, SPATIALREF=oRaster1.SPATIALREF)
  oCMVRaster.Save

  oData.Add, oCMVRaster
  ;if widget_info(e.widget_id, /valid_id) then begin
  ;  ; Display the result
  ;  view = e.GetView()
  ;  layer = view.CreateLayer(oCMVRaster)
  ;  ;view.Zoom, /FULL_EXTENT
  ;endif
  
  return
END
