import pandas as pd
import requests
from io import StringIO

# URL to the CSV file containing COVID-19 death data
csv_url = 'https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD'

# Download the CSV data
response = requests.get(csv_url)
if response.status_code == 200:
    csv_data = StringIO(response.text)
    # Read the CSV data into a DataFrame
    df = pd.read_csv(csv_data)
    # Convert the 'Data As Of' column to datetime format
    df['Data as of'] = pd.to_datetime(df['Data as of'])
    df['End Week'] = pd.to_datetime(df['End Week'])
    # Filter data for the years 2020 to 2024
    df_filtered = df[(df['Data as of'].dt.year >= 2020) & (df['Data as of'].dt.year <= 2024)]

    df_filtered = df_filtered[(df_filtered["Sex"] == "All Sex") & (df_filtered["Age Group"] == "All Ages")]
    df_filtered = df_filtered[["End Week", "COVID-19 Deaths"]]

    # Display the filtered data
    df_filtered.to_csv("covid.csv", index=False)
else:
    print(f'Failed to download data: {response.status_code}')
