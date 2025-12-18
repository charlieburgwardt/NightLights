
;=============================================================
; bm_viirs_dnb_qf_mosaic_from_list.pro
; Build a two-band mosaic (Band 0=DNB, Band 1=QF) for one DOY
; from N tiles in VNP46A1 HDF5 files. Returns the output URI.
;=============================================================



FUNCTION _extract_sds_raster, f, sds_name, data_ignore=ignore
  COMPILE_OPT IDL2
  t = ENVITask('ExtractRasterFromFile')
  t.INPUT_URI    = f
  t.DATASET_NAME = sds_name
  IF N_ELEMENTS(ignore) GT 0 THEN t.DATA_IGNORE_VALUE = ignore
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
  
  ;ENVIRaster writes to URI but will not overwrite an existing file
  FILE_DELETE, uri_out, /ALLOW_NONEXISTENT
  
  data = ras.GetData()
  meta = ENVIRasterMetadata()
  IF N_ELEMENTS(ignore) GT 0 THEN meta.AddItem, 'data ignore value', ignore
  out_ras = ENVIRaster(data, URI=uri_out, METADATA=meta, SPATIALREF=spref)
  out_ras.Save
  out_ras.Close
  RETURN, ENVIUrlRaster(uri_out)
END

FUNCTION bm_viirs_dnb_qf_mosaic_from_list, files, out_dir, DOY_KEY=doy_key, USE_UNION_GRID=use_union
  COMPILE_OPT IDL2
  ON_ERROR, 2

  IF (N_ELEMENTS(files) EQ 0) THEN MESSAGE, 'No input files.'

  ; Launch ENVI (headless is fine for batch)
  ;e = ENVI(/HEADLESS)  ; ENVI object  ; see ENVI() docs  ;Started envi before calling bm_process_by_doy.pro
    
  ; https://nv5geospatialsoftware.co.jp/docs/envi.html  [7](https://nv5geospatialsoftware.co.jp/docs/envimetaspectralraster.html)

  ; HDF5 dataset names in VNP46A1
  dnb_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/DNB_At_Sensor_Radiance'
  qf_sds  = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/QF_Cloud_Mask'
  lat_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lat'
  lon_sds = 'HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data Fields/lon'

  ; Fill values from Black Marble docs
  ; DNB fill ~ -999.9 ; QF fill ~ 65535
  dnb_fill = -999.9
  qf_fill  = 65535
  ; (See VNP46A1 SDS tables in the User Guide.) [8](https://nv5inc-my.sharepoint.com/personal/charlie_burgwardt_nv5_com/Documents/Microsoft%20Copilot%20Chat%20Files/bm_viirs_dnb_qf_mosaic.pro.txt)

  ;--- Helper: Extract one HDF5 dataset as ENVIRaster via task
  ;--- Create WGS84-referenced rasters for each tile (DNB + QF)
  dnb_rasters = OBJARR(N_ELEMENTS(files))
  qf_rasters  = OBJARR(N_ELEMENTS(files))

  FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
    f = files[i]

    ; Extract DNB, QF, lat, lon rasters
    dnb_i = _extract_sds_raster(f, dnb_sds, data_ignore=dnb_fill)
    qf_i  = _extract_sds_raster(f, qf_sds,  data_ignore=qf_fill)
    lat_r = _extract_sds_raster(f, lat_sds)
    lon_r = _extract_sds_raster(f, lon_sds)

    lat = lat_r.GetData()
    lon = lon_r.GetData()
    lat_vec = REFORM(lat, N_ELEMENTS(lat), /OVERWRITE)
    lon_vec = REFORM(lon, N_ELEMENTS(lon), /OVERWRITE)
    spref = _make_wgs84_spatialref(lat_vec, lon_vec)

    ; Persist referenced rasters (no resampling yet)
    dnb_uri = FILEPATH('tile_'+STRTRIM(i,2)+'_DNB.dat', ROOT_DIR=out_dir)
    qf_uri  = FILEPATH('tile_'+STRTRIM(i,2)+'_QF.dat',  ROOT_DIR=out_dir)
    dnb_rasters[i] = _apply_spatialref(dnb_i, spref, dnb_uri, data_ignore=dnb_fill)
    qf_rasters[i]  = _apply_spatialref(qf_i,  spref, qf_uri,  data_ignore=qf_fill)
  ENDFOR

  ;--- Compute union grid to guarantee same rows/cols before mosaicking
  IF KEYWORD_SET(use_union) THEN BEGIN
    gtask = ENVITask('CalculateGridDefinitionFromRasterUnion')
    gtask.INPUT_RASTERS = dnb_rasters
    gtask.Execute
    union_grid = gtask.OUTPUT_GRIDDEFINITION
    ; https://www.nv5geospatialsoftware.com/docs/envicalculategriddefinitionfromrasteruniontask.html  [2](https://www.nv5geospatialsoftware.com/docs/stregex.html)

    ; Regrid each tile to the common union grid
    dnb_rg = OBJARR(N_ELEMENTS(dnb_rasters))
    qf_rg  = OBJARR(N_ELEMENTS(qf_rasters))
    FOR i=0, N_ELEMENTS(dnb_rasters)-1 DO BEGIN
      dnb_rg[i] = ENVISpatialGridRaster(dnb_rasters[i], GRID_DEFINITION=union_grid)
      qf_rg[i]  = ENVISpatialGridRaster(qf_rasters[i],  GRID_DEFINITION=union_grid)
    ENDFOR
    ; https://www.nv5geospatialsoftware.com/docs/envispatialgridraster.html  [3](https://envi.geoscene.cn/help/Subsystems/envi/Content/ExtendCustomize/ENVIMosaicRaster/ENVIMosaicRaster.htm)

    ; Mosaic DNB & QF
    tm_dnb = ENVITask('BuildMosaicRaster')
    tm_dnb.INPUT_RASTERS     = dnb_rg
    tm_dnb.DATA_IGNORE_VALUE = dnb_fill
    tm_dnb.FEATHERING_METHOD = 'None'
    tm_dnb.Execute
    dnb_mosaic = tm_dnb.OUTPUT_RASTER

    tm_qf = ENVITask('BuildMosaicRaster')
    tm_qf.INPUT_RASTERS      = qf_rg
    tm_qf.DATA_IGNORE_VALUE  = qf_fill
    tm_qf.FEATHERING_METHOD  = 'None'
    tm_qf.Execute
    qf_mosaic = tm_qf.OUTPUT_RASTER

  ENDIF ELSE BEGIN
    ; Mosaic directly (ENVI picks a base grid); may vary rows/cols if tiles differ
    tm_dnb = ENVITask('BuildMosaicRaster')
    tm_dnb.INPUT_RASTERS     = dnb_rasters
    tm_dnb.DATA_IGNORE_VALUE = dnb_fill
    tm_dnb.FEATHERING_METHOD = 'None'
    tm_dnb.Execute
    dnb_mosaic = tm_dnb.OUTPUT_RASTER

    tm_qf = ENVITask('BuildMosaicRaster')
    tm_qf.INPUT_RASTERS      = qf_rasters
    tm_qf.DATA_IGNORE_VALUE  = qf_fill
    tm_qf.FEATHERING_METHOD  = 'None'
    tm_qf.Execute
    qf_mosaic = tm_qf.OUTPUT_RASTER
  ENDELSE
  ; BuildMosaicRaster: https://www.nv5geospatialsoftware.com/docs/envibuildmosaicrastertask.html  [4](https://www.nv5geospatialsoftware.com/docs/envitasksmainpage.html)

  ;--- Two-band stack: Band 0 = DNB, Band 1 = QF
  bs = ENVITask('BuildBandStack')
  bs.INPUT_RASTERS     = [dnb_mosaic, qf_mosaic]
  bs.OUTPUT_RASTER_URI = FILEPATH('VNP46A1_DNB_QF_A' + doy_key + '.dat', ROOT_DIR=out_dir)
  bs.Execute
  ; BuildBandStack: https://www.nv5geospatialsoftware.com/docs/envibuildbandstacktask.html  [5](https://envi.geoscene.cn/help/Subsystems/envi/Content/ExtendCustomize/ENVIRaster/enviRaster.htm?TocPath=Programming%7CData%20Management%20Routines%7CENVIRaster%7C_____0)

  final_uri = bs.OUTPUT_RASTER.URI
  RETURN, final_uri
END
