import requests
import os
from datetime import datetime
from pathlib import Path
import pyarrow
import geopandas as gpd 
import pandas as pd 
import matplotlib.pyplot as plt
from shapely.ops import unary_union
import contextily as cx
import seaborn as sns
from datetime import datetime

root_dir = Path("~/Desktop/Desktop/epidemiology_PhD/00_repos/").expanduser()

url = "https://hub.arcgis.com/api/v3/datasets/025fb2ea05f14890b2b11573341b5b18_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1"

output_dir = root_dir / "los_angeles_2025_fire_disasters_exp/data/00_raw/calfire_boundaries"
data_dir = root_dir / "los_angeles_2025_fire_disasters_exp/data"

so_cal_counties = ["025", "029", "037", "065", "059", "071", "073", "083", "111", "079"]

# outline 
# goal: census tract daily dataset with exposure (yes/no) based on various buffers (specified below)
# steps:
    # 1. download fires data
    # 2. load census tract data
    # 3. load wf boundary data
    # 4. create merged fire polys (agg over days)
    # 5. buffer the fire polys (buffers: .5, 1, 10, 20 km)
    # 6. overlay each buffer version with census tracts to determine if there is any overlap (any overlap at all = exposed)
    # 7. final dataset: census_tract, exposed_0.5km, exposed_1km, exposed_10km, exposed_20km

# step 1: download fires data

# generate a filename with the current date
filename = f"data_{datetime.now().strftime('%Y_%m_%d')}.geojson"
output_path = os.path.join(output_dir, filename)

# Download the file
response = requests.get(url)
# Check if the request was successful
response.raise_for_status()  

# Save
with open(output_path, "wb") as file:
    file.write(response.content)

# steps 2-3: load and clean fires and cts data 

# data contains wf data going back to 2024 or 2023 
# filter to dates since January 7th, 2025 and take out time
fires = gpd.read_file(output_path).to_crs(epsg=2229)
cts = gpd.read_file(data_dir / "00_raw/tl_2010_06_tract10.shp").to_crs(epsg=2229)
cts = cts[['geometry', 'GEOID10', 'COUNTYFP10']]
cts = cts[cts['COUNTYFP10'].isin(so_cal_counties)]

fires["poly_DateCurrent"] = fires["poly_DateCurrent"].dt.tz_convert('US/Pacific')
fires = fires[fires['poly_DateCurrent'] > '2025-01-06']
fires["poly_DateCurrent"] = fires["poly_DateCurrent"].dt.date

# fill in names for all fires 
fires["incident_name"] = fires["incident_name"].str.lower()
fires['incident_name'].fillna(fires['mission'].str.split('-').str[2].str.lower(), inplace=True)

# filter to only the cols we need
# incident_name, poly_DateCurrent, geometry
fires = fires[["incident_name", "poly_DateCurrent", "geometry"]]

# NOTE: there is one row that is called kenneth but has a poly that covers both pallisades and kenneth. from jan 9
# leaving it for now!

# step 4: create merged fire polys (agg over days)
# step 5: buffer the fire polys (buffers: .5, 1, 10, 20 km)
# step 6: overlay each buffer version with census tracts to determine if there is any overlap (any overlap at all = exposed)
# step 7: final dataset: census_tract, exposed_0.5km, exposed_1km, exposed_10km, exposed_20km
buffer_distances = [0.5, 1, 10, 20]
buffers = [500, 1_000, 10_000, 20_000]

# Create empty dictionary to store results
tract_exposures = {}
# Create list to store summary table
summary_data = []

# For each buffer distance
for dist, dist_km in zip(buffers, buffer_distances):
    buffered_fires = fires.geometry.buffer(dist)
    combined_buffer = buffered_fires.unary_union
    exposed_tracts = cts[cts.geometry.intersects(combined_buffer)]
    num_exposed = len(exposed_tracts)
    
    summary_data.append({
        'buffer_distance_km': dist_km,
        'num_exposed_tracts': num_exposed,
        'pct_exposed': round((num_exposed / len(cts))*100, 2)
    })
    
    exposed_tracts = cts[cts.geometry.intersects(combined_buffer)]
    tract_exposures[f'exposed_{dist_km}buffer'] = exposed_tracts['GEOID10'].tolist()

result_df = pd.DataFrame({'GEOID10': cts['GEOID10']})
result_df = result_df.set_index('GEOID10')
summary_df = pd.DataFrame(summary_data)

# add exposure columns
for col in tract_exposures.keys():
    result_df[col] = result_df.index.isin(tract_exposures[col]).astype(int)

# save data!
result_df.to_csv(data_dir / f"02_processed/ct_exposures_{datetime.now().strftime('%Y_%m_%d')}.csv")
summary_df.to_csv(data_dir / f"03_summaries/ct_summary_exposures_{datetime.now().strftime('%Y_%m_%d')}.csv", index=False)

# python code/01_exposure_assessment/ct/exposure_ct.py -o data/02_processed