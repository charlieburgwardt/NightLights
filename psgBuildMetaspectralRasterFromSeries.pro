;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
;   psgBuildMetaspectralRasterFromSeries
; 
; :Description:
;   This task converts an ENVIRasterSeries collection of rasters to an ENVIMetaspectral raster.
; 
; :Keywords:
;   INPUT_RASTERSERIES {input}{required}
;     Input ENVIRasterSeries object.
;     
;-----------------------------------------------------------------------------
PRO psgBuildMetaspectralRasterFromSeries, $
  INPUT_RASTERSERIES = inputRasterSeries, $
  OUTPUT_RASTER = oMetaspectralRaster
  
  compile_opt idl2

  oDataCollection = LIST()
  foreach otmpraster, inputRasterSeries do begin
    ;WARNING: About retrieving rasters from an ENVIRasterSeries - the raster objref
    ;         will only contain a basic Metadata objref with just BAND NAMES key in it.
    ;         This kind of screws up my attempts to retain the original HDF5 fileinfo that
    ;         I've kept in the Metadata key DATASET NAMES... How can I fix this?
    ;
    ; I'm going to have to define a new ENVIRaster reference using ENVIUrlRaster...STUPID!
    updatedRaster = ENVIUrlRaster(otmpraster.URI)
    oDataCollection.Add, updatedRaster
  endforeach
  oMetaspectralRaster = ENVIMetaspectralRaster(oDataCollection.ToArray())
    
  return
END