;-----------------------------------------------------------------------------
; UNCLASSIFIED
;-----------------------------------------------------------------------------
; :Name:
; psgInputVIIRSNightLightsData Task
; 
; :Description:
;   This task searches specfied folder location for a collection of HDF5 files 
;   of VIIRS NightLights (VNP46A1) data:
;   1) Extracts selected dataset field(s) from the HDF5 using the H5 import 
;      template "VIIRS DNB CLOUDMASK" and builds collection of ENVIRaster refs.
; 
; :Keywords:
;   INPUT_DATASET_FOLDER {input}{required}
;     Input directory folder location of the HDF5 datasets to import.
;     
;-----------------------------------------------------------------------------
PRO psgInputVIIRSNightLightsData, $
  INPUT_DATASET_FOLDER = dataResource, $
  OUTPUT_RASTERS = oRasterCollection  ;return array of envirasters
  
  COMPILE_OPT idl2

  e = ENVI(/CURRENT)
  oDataColl = e.DATA
  oUI = e.UI

  ;NOTE: I already built the VIIRS DNB HDF5 data import template(s) found here:
  ;C:\Users\kellie.mcnaron\.idl\envi\custom_code6_1\viirs_dnb.xml - Template Name: VIIRS DNB'
  ;C:\Users\kellie.mcnaron\.idl\envi\custom_code6_1\viirs_dnb.xml - Template Name: VIIRS DNB CLOUDMASK'

  if ~file_test(dataResource, /DIR) then return
  
  ;cd, dataResource
  ;element = ENVIURI_UI( filter='*.h5', /MULTIPLE )
  ;h5Files = oUI.CreateFromDialog( element, TITLE='Select HDF5 Files To Process...' )
  h5Files = DIALOG_PICKFILE( PATH=dataResource, $
    FILTER='*.h5', /MULTIPLE_FILES, $
    TITLE='Select HDF5 Files To Process...' )
  nfiles = n_elements(h5Files)
  if nfiles eq 0 then return
  
  ;h5Files = file_search(dataResource, '*.h5', count=nfiles)
  ;if nfiles eq 0 then return

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 1) Load these datasets into a ENVIRasters...
  oRasterCollection = objarr(nfiles)
  for rr=0,nfiles-1 do $
    oRasterCollection[rr] = enviurlraster(h5Files[rr], template='VIIRS DNB CLOUDMASK')

;  ; Build a raster series for this collection of envirasters...
;  output_uri = IDLcfGetTemporaryFile(ext='series')
;  
;  task = ENVITask('BuildRasterSeries')
;  task.INPUT_RASTERS = oRasterCollection
;  task.OUTPUT_RASTERSERIES_URI = output_uri
;  task.Execute
;  oSeriesRaster = task.OUTPUT_RASTERSERIES
;  oDataColl.Add, oSeriesRaster
    
  return
END