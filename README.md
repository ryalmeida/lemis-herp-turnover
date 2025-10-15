# lemis-herp-turnover
Code reproduces turnover, species accumulation curve, and residence time analyses from Chan et al. (2025), "Two decades of live reptile and amphibian imports reveal variable and dynamic trade patterns across key ports of entry in the United States". The general workflow is as follows:

1. Download Marshall et al. (2025) corrected LEMIS data from the following repository: https://figshare.com/s/960af99373aba13791be
2. Clean data using LEMIS_herps_dataClean.R
3. Run analyses using cleaned data (herpsLEMIS.csv) in LEMIS_herps_analyses.R

