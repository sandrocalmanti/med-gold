Cambiato il file di phenology.

In the folder BSC_demo_scripts_for_ICT_platform, you can find six R scrips along with a readme.txt file, a folder data containing demo data to run these scripts and a pdf file explaining indices.
 
Each R script computes, corrects bias and produces most likely tercile plot for each agroclimatic index selected by SOGRAPE. 

These indices are (1) Growing Degree Days (GDD) (2) Growing Season Temperature (GST), (3) Spring Rain (sprR), (4) Harvest rain (harvR), (5) Warm Spell Duration Index (WSDI) and (6) Number of heat heat stres days (SU35). For detail descriptions of these indices please read the file INDICES Explained.pdf. 

Thus, the scripts GDD_MstLik.complete.R, GST_MLik_complete.R, harvR_Mst_Lik_complete.R, sprR_MstLik_complete.R, su35_MstLik_complete.R, WSDI_MstLik_complete.R are for indices GDD, GST, harvR, sprR, SU35 and WSDI respectively. 

After successfully running each script, you will get a bias-corrected .RDS file of an index and a most-likely tercile plot.

Before running these scripts you must modify them as follows. These modifications apply to each script. 

(a) Modify yearini and yearend in the script. Yearini is the start year and yearend is the end year of your data.

(b) Modify case in the script. This is the case study year. I have taken 1993 but you can choose any period covered by your data. 

(c) Modify path_input. This is the path of your data. This path is just an example for the model data. You have to provide path of your own data. 

(d) Modify path_out. This is the path where your plots will be saved. Specify the path where you want to save your plots.

(e) Modify path of land-sea-mask. In the script the path for land-sea-mask is '/esarchive/scratch/pjha/plots_combd_scripts/GST/data’ but you need to change this to where you have saved the land-sea-mask file. Bear in mind that this is the mask for ECMWF System5 c3s data, which has a resolution of 181 x 360. This mask will not fit for data with different resolution.

(f) Modify latitude and longitude values in the line lsm_chunk=lsm1[127:134,352:360]. Here 127:134 is the latitude and 352:360 is the longitude. This is the domain of the model data but you must change this value according to the domain of your data. 

(g) Change name of input files. Following input files are needed in the following data matrix structure. If the matrix structure is different then change it to follow the following order.

            Dimension characteristic of Indices

            GST
            Input files: Monthly Maximum and Minimum Temperature

            Data matrix structure
            Reference = [year, month, latitude, longitude]
            Model = [member, year, month, latitude, longitude]

            Spring Rain
            Input file: Daily average precipitation

            Data matrix structure
            Model = [member, year, days, latitude, longitude]
            Reference = [year, days, latitude, longitude]

            Harvest Rain
            Input file: Daily average precipitation

            Data matrix structure
            Model = [member, year, days, latitude, longitude]
            Reference = [year, days, latitude, longitude]

            GDD
            Input files:Daily Maximum and Minimum Temperature

            Data matrix structure
            Model = [member, year, days, latitude, longitude] 
            Reference = [year, days, latitude, longitude] 

            WSDI
            Input file: Daily Maximum Temperature

            Same as GDD

            SU35
            Input file: Daily Maximum Temperature
            Same as GDD

(h) Start Month

            GST
            Start Month is 04 (April), which is zero month lead because GST covers the period April to October. But you can change this to before or after.

            Spring Rain
            Start Month is 04 (April),  which is zero month lead because it covers the period April 21 to June 21.  But you can change this to before or after.

            Harvest Rain
            Start Month is 08 (August), which is zero month lead because it covers the period August 21 to October 21. But you can change this to before or after.

            GDD
            Start Month is 04 (April), which is zero month lead because GDD covers the period April to October. But you can change this before or after. Please bear in mind that if you change start month you have to change leadtimemax as well to cover the period from April to October.

            WSDI
            Start Month is 06 (June) because the forecasts are of seven month length and from June they cover the entire year. You can increase or decrease it depending on your period of interest.

            SU35
            Start Month is 06 (June) because the forecasts are of seven month length and from June they cover the entire year. You can increase or decrease it depending on your period of interest.

Good luck !
