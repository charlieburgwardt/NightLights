
;==============================================================================
; bm_viirs_dnb_qf_mosaic.pro (updated)
; Two-band mosaic (DNB Radiance + QF Cloud Mask) from two VNP46A1 tiles.
; Fixes: separate spatial refs per tile + optional union grid for exact dims.
;==============================================================================

FUNCTION _extract_sds_raster, e, file, sds_name, data_ignore=ignore
  COMPILE_OPT idl2
  t = ENVITask('ExtractRasterFromFile')
  t.INPUT_URI    = file
  t.DATASET_NAME = sds_name
  IF N_ELEMENTS(ignore) GT 0 THEN t.DATA_IGNORE_VALUE = ignore
  CATCH, err
  IF err NE 0 THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, 'Failed to extract "'+sds_name+'" from '+file
  ENDIF
  t.Execute
  RETURN, t.OUTPUT_RASTER
END

FUNCTION _make_wgs84_spatialref, lat_vec, lon_vec
  COMPILE_OPT idl2
  IF (N_ELEMENTS(lat_vec) LT 2) OR (N_ELEMENTS(lon_vec) LT 2) THEN $
    MESSAGE, 'lat/lon axis vectors must each have length >= 2'
  ; Pixel sizes (IDL X,Y): lon -> X, lat -> Y
  px_x = MEAN(ABS(lon_vec[0:N_ELEMENTS(lon_vec)-2] - lon_vec[1:N_ELEMENTS(lon_vec)-1]))
  px_y = MEAN(ABS(lat_vec[0:N_ELEMENTS(lat_vec)-2] - lat_vec[1:N_ELEMENTS(lat_vec)-1]))
  tie_map = [MIN(lon_vec), MAX(lat_vec)]   ; UL map coord: (min lon, max lat)
  tie_pix = [0D, 0D]
  spref = ENVIStandardRasterSpatialRef( $
    COORD_SYS_CODE = 4326L, GEOGCS = 1, $
    PIXEL_SIZE     = [px_x, px_y], $
    ROTATION       = 0.0D, $
    TIE_POINT_PIXEL= tie_pix, $
    TIE_POINT_MAP  = tie_map )
  RETURN, spref
END

FUNCTION _apply_spatialref, ras, spref, uri_out, data_ignore=ignore
  COMPILE_OPT idl2
  data = ras.GetData()
  meta = ENVIRasterMetadata()
  IF N_ELEMENTS(ignore) GT 0 THEN meta.AddItem, 'data ignore value', ignore
  out_ras = ENVIRaster(data, URI=uri_out, METADATA=meta, SPATIALREF=spref)
  out_ras.Save
  out_ras.Close
  RETURN, ENVIUrlRaster(uri_out)
END

PRO bm_viirs_dnb_qf_mosaic, file1, file2, out_dir, USE_UNION_GRID=use_union
  COMPILE_OPT idl2
  ON_ERROR, 2
  e = ENVI(/HEADLESS)

  PRINT, '>>> Opening tiles:'
  PRINT, '    ', file1
  PRINT, '    ', file2

  dnb_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/DNB_At_Sensor_Radiance'
  qf_sds  = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/QF_Cloud_Mask'
  lat_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lat'
  lon_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lon'

  dnb_fill = -999.9
  qf_fill  = 65535

  ;--- Extract rasters from both tiles
  dnb1 = _extract_sds_raster(e, file1, dnb_sds, data_ignore=dnb_fill)
  dnb2 = _extract_sds_raster(e, file2, dnb_sds, data_ignore=dnb_fill)
  qf1  = _extract_sds_raster(e, file1, qf_sds,  data_ignore=qf_fill)
  qf2  = _extract_sds_raster(e, file2, qf_sds,  data_ignore=qf_fill)

  ;--- Axis vectors per tile (build spref1/spref2 separately)
  lat1_raster = _extract_sds_raster(e, file1, lat_sds)
  lat1 = lat1_raster.GetData()
  lon1_raster = _extract_sds_raster(e, file1, lon_sds)
  lon1 = lon1_raster.GetData()
  lat2_raster = _extract_sds_raster(e, file2, lat_sds)
  lat2 = lat2_raster.GetData()
  lon2_raster = _extract_sds_raster(e, file2, lon_sds)
  lon2 = lon2_raster.GetData()

  lat1_vec = REFORM(lat1, N_ELEMENTS(lat1), /OVERWRITE)
  lon1_vec = REFORM(lon1, N_ELEMENTS(lon1), /OVERWRITE)
  lat2_vec = REFORM(lat2, N_ELEMENTS(lat2), /OVERWRITE)
  lon2_vec = REFORM(lon2, N_ELEMENTS(lon2), /OVERWRITE)

  spref1 = _make_wgs84_spatialref(lat1_vec, lon1_vec)
  spref2 = _make_wgs84_spatialref(lat2_vec, lon2_vec)

  PRINT, '>>> Tile1 pixel size (deg): ', spref1.PIXEL_SIZE, '  UL: ', spref1.TIE_POINT_MAP
  PRINT, '>>> Tile2 pixel size (deg): ', spref2.PIXEL_SIZE, '  UL: ', spref2.TIE_POINT_MAP

  ;--- Assign spatial refs (no resampling; L3 grid)
  tmp1_dnb = FILEPATH('dnb1_reproj.dat', ROOT_DIR=out_dir)
  tmp2_dnb = FILEPATH('dnb2_reproj.dat', ROOT_DIR=out_dir)
  tmp1_qf  = FILEPATH('qf1_reproj.dat',  ROOT_DIR=out_dir)
  tmp2_qf  = FILEPATH('qf2_reproj.dat',  ROOT_DIR=out_dir)

  dnb1_rp = _apply_spatialref(dnb1, spref1, tmp1_dnb, data_ignore=dnb_fill)
  dnb2_rp = _apply_spatialref(dnb2, spref2, tmp2_dnb, data_ignore=dnb_fill)
  qf1_rp  = _apply_spatialref(qf1,  spref1, tmp1_qf,  data_ignore=qf_fill)
  qf2_rp  = _apply_spatialref(qf2,  spref2, tmp2_qf,  data_ignore=qf_fill)

  ;--- Optional: force a 10°×20° union grid at 15″ resolution
  IF KEYWORD_SET(use_union) THEN BEGIN
    PRINT, '>>> Building explicit union grid...'
    ; CoordSys object from WGS84
    cs = ENVICoordSys(COORD_SYS_CODE=4326)  ; GEOGCS, EPSG:4326
    ; Grid defs for each tile
    gd1 = ENVIGridDefinition(cs, $
      PIXEL_SIZE    = spref1.PIXEL_SIZE, $
      NROWS         = dnb1_rp.NROWS, $
      NCOLUMNS      = dnb1_rp.NCOLUMNS, $
      TIE_POINT_MAP = spref1.TIE_POINT_MAP, $
      TIE_POINT_PIXEL = spref1.TIE_POINT_PIXEL)

    gd2 = ENVIGridDefinition(cs, $
      PIXEL_SIZE    = spref2.PIXEL_SIZE, $
      NROWS         = dnb2_rp.NROWS, $
      NCOLUMNS      = dnb2_rp.NCOLUMNS, $
      TIE_POINT_MAP = spref2.TIE_POINT_MAP, $
      TIE_POINT_PIXEL = spref2.TIE_POINT_PIXEL)

    ; Union extent
    union_extent = gd1.Union(gd2)
    PRINT, '>>> Union extent [xmin, ymax, xmax, ymin]: ', union_extent

    ; Final union grid at tile pixel size (should be ~0.0041667° both axes)
    union_grid = ENVIGridDefinition(cs, EXTENT=union_extent, PIXEL_SIZE=spref1.PIXEL_SIZE)

    ; Reproject each raster to the union grid (guarantees 2400×4800)
    dnb1_u = ENVISpatialGridRaster(dnb1_rp, GRID_DEFINITION=union_grid)
    dnb2_u = ENVISpatialGridRaster(dnb2_rp, GRID_DEFINITION=union_grid)
    qf1_u  = ENVISpatialGridRaster(qf1_rp,  GRID_DEFINITION=union_grid)
    qf2_u  = ENVISpatialGridRaster(qf2_rp,  GRID_DEFINITION=union_grid)

    ; Mosaic on the union grid
    m_dnb = ENVITask('BuildMosaicRaster')
    m_dnb.INPUT_RASTERS     = [dnb1_u, dnb2_u]
    m_dnb.DATA_IGNORE_VALUE = dnb_fill
    m_dnb.FEATHERING_METHOD = 'None'
    m_dnb.Execute
    dnb_mosaic = m_dnb.OUTPUT_RASTER

    m_qf = ENVITask('BuildMosaicRaster')
    m_qf.INPUT_RASTERS      = [qf1_u, qf2_u]
    m_qf.DATA_IGNORE_VALUE  = qf_fill
    m_qf.FEATHERING_METHOD  = 'None'
    m_qf.Execute
    qf_mosaic = m_qf.OUTPUT_RASTER

  ENDIF ELSE BEGIN
    ; Mosaic directly (relies on ENVI's default mosaic grid behavior)
    PRINT, '>>> Mosaicking DNB (direct)...'
    m_dnb = ENVITask('BuildMosaicRaster')
    m_dnb.INPUT_RASTERS     = [dnb1_rp, dnb2_rp]
    m_dnb.DATA_IGNORE_VALUE = dnb_fill
    m_dnb.FEATHERING_METHOD = 'None'
    m_dnb.Execute
    dnb_mosaic = m_dnb.OUTPUT_RASTER

    PRINT, '>>> Mosaicking QF (direct)...'
    m_qf = ENVITask('BuildMosaicRaster')
    m_qf.INPUT_RASTERS      = [qf1_rp, qf2_rp]
    m_qf.DATA_IGNORE_VALUE  = qf_fill
    m_qf.FEATHERING_METHOD  = 'None'
    m_qf.Execute
    qf_mosaic = m_qf.OUTPUT_RASTER
  ENDELSE

  PRINT, '>>> DNB mosaic dims: ', dnb_mosaic.NCOLUMNS, ' x ', dnb_mosaic.NROWS
  PRINT, '>>> QF  mosaic dims: ', qf_mosaic.NCOLUMNS,  ' x ', qf_mosaic.NROWS

  ; Export mosaics
  dnb_mosaic_uri = FILEPATH('VNP46A1_DNB_mosaic.dat', ROOT_DIR=out_dir)
  qf_mosaic_uri  = FILEPATH('VNP46A1_QF_mosaic.dat',  ROOT_DIR=out_dir)
  dnb_mosaic.Export, dnb_mosaic_uri, 'ENVI'
  qf_mosaic.Export,  qf_mosaic_uri,  'ENVI'

  ; Two-band stack
  PRINT, '>>> Building two-band stack (Band0=DNB, Band1=QF)...'
  dnb_mo = e.OpenRaster(dnb_mosaic_uri)
  qf_mo  = e.OpenRaster(qf_mosaic_uri)
  bs     = ENVITask('BuildBandStack')
  bs.INPUT_RASTERS     = [dnb_mo, qf_mo]
  bs.OUTPUT_RASTER_URI = FILEPATH('VNP46A1_DNB_QF_mosaic.dat', ROOT_DIR=out_dir)
  bs.Execute

  final_uri = bs.OUTPUT_RASTER.URI
  PRINT, '>>> Final two-band mosaic: ', final_uri
  PRINT, '>>> Final dims           : ', bs.OUTPUT_RASTER.NCOLUMNS, ' x ', bs.OUTPUT_RASTER.NROWS

  END