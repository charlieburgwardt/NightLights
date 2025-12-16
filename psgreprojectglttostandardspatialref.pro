;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PSG_ORTHOGRID: Ortho-rectify an image with arbitrary spatial sampling.
;
;This is really just a wrapper to IDLs TRIANGULATE and TRIGRID functions.  Given an image
;and a set of geographic coordinates (e.g. lat and long) for every pixel in the image,
;this function will find a Delauney triangulation of the geographic coordinates and
;resample the image onto a regular pixel grid covering the same area as the
;irregularly sampled coordinates.  If no image is supplied, the function will do only
;the triangulation and pass back the result through the keyword TRIANGLES.  Triangulation
;is a time-consuming step that can be performed once and then applied to multiple images
;that are on the same sampling grid (e.g. multiple image bands or filter planes).  A triangulation
;can also be passed into the function through the TRIANGLES keyword in which case the
;function will perform only the resampling.  Resampling is always bilinear.
;
;Calling Sequence
;================
;rsim = psg_orthogrid(longlat,image=image,triangles=triangles,gs=gs,fact=fact,longvec=longvec,latvec=latvec)
;
;LONGLAT  : A 3-D array containing the geographic coordinates for every pixel in an image.  The
;           geographic coordinate of pixel [i,j] is given by the pair (longlat[i,j,0],longlat[i,j,1]).
;           Despite the variable name, these coordinates are treated as planar cartesian coordinates
;           and could be lat and long, UTM or any other coordinate system.
;IMAGE    : An image on the geographic coordinates specified in LONGLAT.  (2-D, one band only.)  If
;           an input image is supplied via this keyword, then it will be resampled onto a regularly
;           spaced grid of points on an area that includes the coordinates specified in LONGLAT.
;           The output grid is in the same coordinate system as LONGLAT.  If IMAGE is omitted then
;           no resampling will be done (obviously).  But triangluation may still be done.
;TRIANGLES: A triangulation of the coordinates specified in longlat.  This is both an input and
;           an output variable.  If TRIANGLES is omitted or undefined on input, IDL's
;           TRIANGULATE function will be used to generate a triangulation.  The result will be
;           passed back out via TRIANGLES.  If a triangulation is input through TRIANGLES then
;           TRIANGULATE will not be called.  The input triangulation will be used for resampling
;           if an input image is provided through the IMAGE keyword.
;GS       : An input keyword to specify the output grid spacing.  A two element vector specifying
;           the spacing in the first and second image dimensions.  If GS is omitted then a square
;           grid is used with spacing estimated as follows: first the average ground sampling
;           distance (GSD) is estimated from the first image direction (cross-scan).  That estimate
;           is then divided by the value supplied via FACT. The result is used for the grid spacing
;           in both dimensions. (FACT defaults to 2.0, so if neither GS nor FACT is supplied, the
;           default resamples the image at approximatedly twice the cross-scan resolution.)
;FACT     : A magnification factor.  If the output grid spacing is not supplied via GS, then this
;           factor is used in choosing a default grid.  See above.
;LONGVEC  : A vector specifying the output grid point locations in the first geographic coordinate
;           dimension.  Both input and output.  On input, LONGVEC and LATVEC take precedence
;           over GS and FACT in specifying the output grid.
;LATVEC   : A vector specifying the output grid point locations in the second geographic coordinate
;           dimension.  Both input and output.  On input, LONGVEC and LATVEC take precedence
;           over GS and FACT in specifying the output grid.
;ROTATION : Rotation value to be undone prior to output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function psg_orthogrid, longlat, $
  image=image, triangles=triangles, gs=gs, fact=fact, missing=missing, longvec=longvec, $
  latvec=latvec, error_stat=error_stat, rotation=rotation, mask=mask

  compile_opt idl2
  catch, error_status
  if error_status ne 0 then begin
    catch, /cancel
    help, /traceback, output=s_stack
    print, s_stack
    return, 1
  endif

  if n_elements(rotation) gt 0 then begin
    ; Get inverse of rotation so as to undo it
    case (rotation mod 8) of
      0:
      1: rotation = 3
      2:
      3: rotation = 1
      4:
      5: rotation = 7
      6:
      7: rotation = 5
    endcase
    lat = longlat[*,*,0]
    lon = longlat[*,*,1]
    nlat = rotate(lat, rotation)
    nlon = rotate(lon, rotation)
    lat = rotate(nlat, 7)
    lon = rotate(nlon, 7)
    geo = [[[lat]],[[lon]]]
    if keyword_set(image) then begin
      imag = rotate(image, rotation)
      img = rotate(imag, 7)
    endif
  endif else begin
    if n_elements(image) gt 0 then img = image
    geo = longlat
  endelse

  error_stat = 1

  N=size(geo,/dim)
  long=reform(geo[*,*,0],[N[0]*N[1],1])
  lat=reform(geo[*,*,1],[N[0]*N[1],1])

  if not keyword_set(triangles) then begin
    ;Call triangulate
    triangles=0L
    triangulate,long,lat,triangles
  endif

  if 0 eq n_elements(img) then return,1

  ; Call TRIGRID
  N=size(img)

  if N[0] gt 2 then begin
    print,'Sorry, one band at a time.  Doing band 0'
    im=reform(img[*,*,0],[N[1]*N[2],1])
  endif else begin
    im=reform(img,[N[1]*N[2],1])
  endelse

  if 0 eq n_elements(missing) then missing = 0

  if keyword_set(longvec) then begin
    if 0 ne n_elements(gs) then print,'You specified both GS and LONGVEC,LATVEC  Ignoring GS.'
    out = trigrid(long,lat,im,triangles,missing=missing,xout=longvec,yout=latvec)
    return,out
  endif

  if 0 eq n_elements(gs) then begin

    ;2025Aug15 (KMB) - instead of building a default GS that's square,
    ;                  determine GS using both X-Track and Along-Scan GSDs.
    ; Calculate GS using both x-track GSD and along-scan GSD...
    xtrack_longdiffs = geo[floor(N[1]/2),1:(N[2]-1),0]-geo[floor(N[1]/2),0:(N[2]-2),0]
    xtrack_latdiffs  = geo[floor(N[1]/2),1:(N[2]-1),1]-geo[floor(N[1]/2),0:(N[2]-2),1]
    ascan_longdiffs  = geo[1:(N[1]-1),0,0]-geo[0:(N[1]-2),0,0]
    ascan_latdiffs   = geo[1:(N[1]-1),0,1]-geo[0:(N[1]-2),0,1]

    xtrackgsd = mean(sqrt(xtrack_longdiffs*xtrack_longdiffs + xtrack_latdiffs*xtrack_latdiffs))
    ascangsd  = mean(sqrt(ascan_longdiffs*ascan_longdiffs + ascan_latdiffs*ascan_latdiffs))

    if 0 eq n_elements(fact) then fact=1.0
    gs = [ascangsd,xtrackgsd]/fact

  endif

  out = trigrid(long,lat,im,triangles,gs,missing=missing,xgrid=longvec,ygrid=latvec)

  ; Apply Mask to Clean Up Image Outside Footprint Polygon
  IF n_elements(mask) eq 0 THEN BEGIN
    ; Recalculate Missing Values for Areas Outside Footprint Polygon
    polygon_X   = [REFORM(geo[0:N[1]-1,0,0]), REFORM(geo[N[1]-1,1:N[2]-1,0]), $
      REVERSE(REFORM(geo[0:N[1]-2,N[2]-1,0])), REVERSE(REFORM(geo[0,0:N[2]-2,0]))]
    polygon_Y   = [REFORM(geo[0:N[1]-1,0,1]), REFORM(geo[N[1]-1,1:N[2]-1,1]), $
      REVERSE(REFORM(geo[0:N[1]-2,N[2]-1,1])), REVERSE(REFORM(geo[0,0:N[2]-2,1]))]
    polygon_iX  = INTERPOL(LINDGEN(N_ELEMENTS(longvec)), longvec, polygon_X)
    polygon_iY  = INTERPOL(LINDGEN(N_ELEMENTS(latvec)), latvec, polygon_Y)
    footprint   = OBJ_NEW('IDLanROI', polygon_iX, polygon_iY)
    mask        = footprint->ComputeMask(DIMENSIONS=[N_ELEMENTS(longvec), N_ELEMENTS(latvec)])
  ENDIF
  badPixels = WHERE(mask EQ 0, nBad)
  IF (nBad GT 0) THEN out[badPixels] = missing

  error_stat = 0
  return, out
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This task reprojects a raster georeferenced by a GLT (Geographic Lookup Table)
;to standard map information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO psgReprojectGLTtoStandardSpatialRef, $
  INPUT_RASTER = oRaster, $
  OUTPUT_URI = fname_reproj, $
  OUTPUT_RASTER = oReprojRaster, $
  OUTPUT_CLOUDMASK = oCloudMaskRaster

  compile_opt idl2, hidden

  if n_elements(oRaster) eq 0 then return

  ;================================= Charlie B. 12/11/2025 ==================================
  ;Raster collection already reprojected to Standard map reference during mosaicing process in:
  ;bm_process_by_day.pro
  ;pick_and_group_by_day.pro
  ;bm_viirs_dnb_qf_mosaic_from_list.pro
  ;So we just copy the already reprojected band 0 over to oReprojRaster
  ;==========================================================================================



  ;  oGLTSpatialRef = oRaster.SPATIALREF
  ;  if n_elements(oGLTSpatialRef) eq 0 then return
  ;  if ~strcmp(obj_class(oGLTSpatialRef), 'ENVIGLTRasterSpatialRef', /fold) then return
  ;
  IF ~KEYWORD_SET(fname_reproj) THEN BEGIN
    tempURI = IDLcfGetTemporaryFile()
    fname_reproj = filepath(root=file_dirname(tempURI), 'REPROJ_'+file_basename(tempURI))
  ENDIF;  ;Ensure output filename is unique and does not already exist...
  ok = FSTK_Check_Output_Filename( fname_reproj )
  ;
  ;  rasterDims = [oRaster.NCOLUMNS, oRaster.NROWS]
  ;  lonRaster = oGLTSpatialRef.XMAP_GRID
  ;  if (lonRaster.NCOLUMNS ne rasterDims[0]) || (lonRaster.NROWS ne rasterDims[1]) then begin
  ;    ; ERROR: GLT lat/lon data must be same spatial size as RASTER image...
  ;    PRINT, 'ERROR: GLT lat/lon data must be same spatial size as RASTER image...'
  ;    return
  ;  endif
  ;
  ;  lonlat = make_array( rasterDims[0], rasterDims[1], 2, /double )
  ;  lonlat[*,*,0] = (oGLTSpatialRef.XMAP_GRID).GetData()
  ;  lonlat[*,*,1] = (oGLTSpatialRef.YMAP_GRID).GetData()
  ;
  ;  ;Start with the DNB data...
  ;  imgData = oRaster.GetData(BAND=0)
  ;
  ;  ;Apply orthorectification using custom function - can only handle one band at a time...
  ;  longvec = 0 & latvec = 0 & help, temporary(longvec), temporary(latvec), output = o
  ;  image_geo = psg_orthogrid(lonlat, image = imgData, $
  ;    longvec = longvec, latvec = latvec, missing = -1, error_stat=errstat)
  ;  if errstat gt 0 then return
  ;
  ;  image_geo = reverse(image_geo, 2)  ;reverse the rows to ensure that [0,0] is at top of display...
  ;
  ;  ;Build the Standard Map Information for ENVIStandardRasterSpatialRef
  ;  nx = n_elements(longvec)
  ;  ny = n_elements(latvec)
  ;  lonvec = [longvec[0], longvec[nx-1]]
  ;  latvec = [latvec[0], latvec[ny-1]]
  ;  xsize = abs(lonvec[1] - lonvec[0]) / nx
  ;  ysize = abs(latvec[1] - latvec[0]) / ny
  ;  pixelSize = [xsize,ysize]
  ;
  ;  if (lonvec[0] lt 0) then begin
  ;    tie_point_map = [max(lonvec),max(latvec)]  ;maybe?
  ;  endif else begin
  ;    tie_point_map = [min(lonvec),max(latvec)]
  ;  endelse
  ;  thisPix = [0L,0L] ;upper-left pixel coord of image...
  ;  outWKID = 4326
  ;  oSpatialRef = ENVIStandardRasterSpatialRef( COORD_SYS_CODE=LONG(outWKID), /GEOGCS, $
  ;    PIXEL_SIZE=pixelSize, ROTATION=0.0, $
  ;    TIE_POINT_PIXEL=thisPix, TIE_POINT_MAP=tie_point_map )
  ;
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
  ;
  ;  oReprojRaster = ENVIRaster( image_geo, URI=fname_reproj, $
  ;    METADATA=outMeta, SPATIALREF=oSpatialRef)

  imgData = oRaster.GetData(BAND=0)   ;oRaster already reprojected, see note above
  ; Create a new raster of the subsetted data
  oSpatialRef = oRaster.SPATIALREF
  oReprojRaster = ENVIRaster(imgData, URI=fname_reproj,  METADATA=outMeta, SPATIALREF=oSpatialRef)
  ;Call to SAVE here closes the raster for writing & converts it to read-only mode.
  oReprojRaster.SAVE
  ; BUT (lesson learned here) the meta items DESCRIPTION and DATASET NAMES, while written
  ; out to FNAME_REPROJ are NOT accessible/acknowledged within this oReprojRaster ref.
  ; I need to call ENVIUrlRaster to build a new ENVIRaster reference - STUPID!
  oReprojRaster.Close
  oReprojRaster = ENVIUrlRaster(fname_reproj)

  ;2025Nov10 - NEW! Address Cloud Mask input...
  ;If there is a second band in oRaster (i.e. cloud mask), it should be ortho-rectified as well...
  oCloudMaskRaster = []
  if oRaster.NBANDS eq 2 then begin

    imgData = oRaster.GetData(BAND=1)  ;CLOUD MASK
    ;longvec = 0 & latvec = 0 & help, temporary(longvec), temporary(latvec), output = o
    ;cloud_geo = psg_orthogrid(lonlat, image = imgData, $
    ;longvec = longvec, latvec = latvec, missing = -1, error_stat=errstat)
    ;if errstat gt 0 then return

    ;cloud_geo = reverse(cloud_geo, 2)  ;reverse the rows to ensure that [0,0] is at top of display...

    outCloudMeta = ENVIRasterMetadata()
    outCloudMeta.AddItem, 'BAND NAMES', [urlName]
    outCloudMeta.AddItem, 'DESCRIPTION', 'CloudMask Ortho-Rectified from GLT SpatialRef'
    outCloudMeta.AddItem, 'DATASET NAMES', urlName

    tempURI = IDLcfGetTemporaryFile()
    fname_cloud_reproj = filepath(root=file_dirname(tempURI), 'CLOUD_'+file_basename(tempURI))

    ;========================= Charlie B. 12/5/2025 ============================
    cloud_status_bits = UINT(imgData) AND 192
    oCloudMaskRaster = ENVIRaster( cloud_status_bits, URI=fname_cloud_reproj, $
      METADATA=outCloudMeta, SPATIALREF=oSpatialRef)

    ;    oCloudMaskRaster = ENVIRaster( cloud_geo, URI=fname_cloud_reproj, $
    ;      METADATA=outCloudMeta, SPATIALREF=oSpatialRef)
    ;===========================================================================
    oCloudMaskRaster.SAVE
    oCloudMaskRaster.Close
    oCloudMaskRaster = ENVIUrlRaster(fname_cloud_reproj)

  endif

  return
END
