import requests
import os
from datetime import datetime

# URL of the file to download
url = "https://hub.arcgis.com/api/v3/datasets/025fb2ea05f14890b2b11573341b5b18_0/downloads/data?format=shp&spatialRefId=3857&where=1%3D1"

# Directory to save the downloaded file
output_dir = "/path/to/your/directory"

# Generate a filename with the current date
filename = f"data_{datetime.now().strftime('%Y%m%d')}.zip"
output_path = os.path.join(output_dir, filename)

# Download the file
response = requests.get(url)
response.raise_for_status()  # Check if the request was successful

# Save the file
with open(output_path, "wb") as file:
    file.write(response.content)

print(f"File downloaded and saved as {output_path}")
