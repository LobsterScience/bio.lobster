ReadMe.txt

GLORYs Downloaded from: https://resources.marine.copernicus.eu/product-detail/GLOBAL_MULTIYEAR_PHY_001_030/ 

Files were downloaded as netCDF's using motu client through python. 
For example 1993:
python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_MULTIYEAR_PHY_001_030-TDS --product-id cmems_mod_glo_phy_my_0.083_P1D-m --longitude-min -70 --longitude-max -56 --latitude-min 41 --latitude-max 48 --date-min "1993-01-01 12:00:00" --date-max "1993-12-31 12:00:00" --depth-min 0.494 --depth-max 0.4941 --variable bottomT --variable so --variable uo --variable vo --out-dir ~/tmp --out-name Glorys1993 --user <> --pwd <>

Forecasts for 2020-2022 coming from : https://resources.marine.copernicus.eu/product-detail/GLOBAL_ANALYSIS_FORECAST_PHY_001_024/

python -m motuclient --motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024 --longitude-min -70 --longitude-max -55 --latitude-min 41 --latitude-max 48 --date-min "2020-01-01 12:00:00" --date-max "2020-12-31 12:00:00" --depth-min 0.494 --depth-max 0.4941 --variable bottomT --variable so --variable uo --variable vo --out-dir ~/tmp --out-name Glorys2020fc --user <> --pwd <>


Model outputs are on a daily temporal resolution at 1/12deg spatial resolution. Only regional outputs were downloaded (lon = c(-70,-56); lat = c(41,48)). Variables of bottom temperature (C), salinity (psu), uo (eastward sea velocity m/s), vo (northward sea velocity m/s). 


netCDFs were converted to .rds objects using github.com/LobsterScience/bio.lobster/R/glorysReshape.r which relys on three packages <satin, dplyr and purrr>.
