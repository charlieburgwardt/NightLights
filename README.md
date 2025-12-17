### Cloud Mitigation
Starting with code from Kellie I try to mitigate the effect of cloud cover in 2cmv change detection.
This may take the form of using the cloud mask to highlight areas of uncertainty or
I may discover that just excluding images with more than a certain percentage of cloud cover is the way to go.
The GLT to standard spatial reference code is a first step toward getting the cloud mask to overlay the change detection.

### Multigrids
We added code to connect multiple VIIRS grids togther. 

- bm_process_by_doy.pro  
- bm_viirs_dnb_qf_mosaic.pro  
- bm_viirs_dnb_qf_mosaic_from_list.pro  

These IDL routines group VNP46A1 filenames by acquisition day-of-year (DOY) using only the first YYYYDDD string that appears right after "VNP46A1.A" in the filename (e.g., "A2025305" → year 2025, DOY 305). Per the Black Marble naming convention, this first A-YYYYDDD block is the acquisition date, while the later YYYYDDDHHMMSS block is the production (processing) date—we explicitly ignore the second one.  

Each DOY group is processed separately and the resulting rasters added to a list named oRasterCollection. bm_process_by_doy iterates group‑by‑group, mosaics, then adds the DOY result to oRasterCollection as an ENVIURLRaster (which is an ENVIRaster opened from disk).  

These rasters will only have 2 bands: DNB_At_Sensor_Radiance and QF_Cloud_Mask. The script builds two mosaics (DNB and QF) and then BuildBandStack combines them into a two‑band raster (Band 0=DNB, Band 1=QF). ENVI’s BuildBandStack requires inputs having the same rows/cols, which we impose by regridding to a union grid first.  

In the end these rasters will have been already reprojected and mosaiced upon output to oRasterCollection. Using CalculateGridDefinitionFromRasterUnion and ENVISpatialGridRaster ensures all tiles share the same grid before mosaicking; the final band stack is thus a two‑band product ready to use.  


