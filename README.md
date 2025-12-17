Starting with code from Kellie I try to mitigate the effect of cloud cover in 2cmv change detection.
This may take the form of using the cloud mask to highlight areas of uncertainty or
I may discover that just excluding images with more than a certain percentage of cloud cover is the way to go.
The GLT to standard spatial reference code is a first step toward getting the cloud mask to overlay the change detection.

I added code to acoonect multiple VIIRS grids togther. This IDL routine groups VNP46A1 filenames by acquisition day-of-year (DOY) using only the first YYYYDDD string that appears right after "VNP46A1.A" in the filename (e.g., "A2025305" → year 2025, DOY 305). Per the Black Marble naming convention, this first A-YYYYDDD block is the acquisition date, while the later YYYYDDDHHMMSS block is the production (processing) date—we explicitly ignore the second one.


