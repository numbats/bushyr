## Data processing of the hotspot data
---
The hotspot data is sourced from Himawari-8. The data processing and clustering is based on Patrick Li's thesis (https://github.com/TengMCing/bushfire-paper). 
                      
## About the data
---
To download Himawari-8 hotspots data, you need to first submit an account request for data use on https://www.eorc.jaxa.jp/ptree/registration_top.html. Then you will receive an email which will contain the address of the **FTP** server, your ID and your password. Use your web browser to access to that **FTP** server. Under the path `/pub/himawari/L3/WLF/bet/`, you will find the hotspots data store in different folders. For example, `201507/` represents the hotspots data in July 2015. We need the monthly data from October 2019 to March 2020. Thus, go to `/pub/himawari/L3/WLF/bet/201910/monthly` and download the csv file `H08_20191001_0000_1MWLFbet_FLDK.06001_06001.csv`. And repeat this step for `201911`, `201912`, `202001`, `202002` and `202003`. The downloaded csv file should be placed in the folder `data`.
                     
## Data processing
---
`hotspot_data_wraangling.Rmd` provides all the code needed to read and process the data. Files needed to perform the clustering and wrangling can be found inside the `scripts` folder. The processes are outline below:

### 1. Clustering-Python-setup.R or reading-data chunk
**Runtime: ~5 minutes**

**Input: hotspot data**

**Output: `VIC_hotspots_before_clustering.csv`, `VIC_hotspots_raw.csv`**

This preprocesses the raw hotspot data by using code taken from `Clustering-Python-setup.R`. The outcomes include two csv files `VIC_hotspots_before_clustering.csv` and `VIC_hotspots_raw.csv`. `VIC_hotspots_before_clustering.csv` only contains observed time and location of hostpots, while `VIC_hotspots_raw.csv` contains full information.

### 2. Clustering Parameters Tuning

**Runtime: ~8-10 hours**

**Input: `VIC_hotspots_before_clustering.csv`**

**Output: `clustering_grid.csv`**

This runs the clustering algorithm using a grid of parameter values by calling `clustering_tune.py`. It provides the results of different attempts in `clustering_grid.csv`.

### 3. Choose-Optimal-Parameters-For-Clustering

**Runtime: ~20 seconds**

**Input: `clustering_grid.csv`**

**Output: `setting.txt, clustering_tuning_1.jpeg, clustering_tuning_2.jpeg`**

This chooses the optimal parameters for the clustering algorithm by calling `clustering_tune_vis.R`. The result is saved in `setting.txt`.

### 4. Hotspots-Clustering

**Runtime: ~6 minutes**

**Input: `VIC_hotspots_before_clustering.csv`**

**Output: `VIC_hotspots_after_clustering.csv, summary.txt`**

This chunk runs the clustering algorithm on the optimal parameters by calling `main.py`.  The outcomes are the final clustering result `VIC_hotspots_after_clustering.csv` and a brief summary of the result `summary.txt`. 

### 5. Bushfires-Data-Integration-Hotspots

**Runtime: ~10 minutes**

**Input: `VIC_hotspots_raw.csv`, `VIC_hotspots_after_clustering.csv`**

**Output: `predict_x.csv`**

This chunk performs data integration on the clustering result by calling `combine_current.R`. The outome is a dataset contains fire ignitions provided in `predict_x.csv`.

(Taken from link)



