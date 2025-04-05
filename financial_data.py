import pandas_datareader.data as web
import pandas as pd
from datetime import datetime
import os

# Set your FRED API key (get it at https://fred.stlouisfed.org/)
os.environ["FRED_API_KEY"] = "4435722de5f82f301da0f241c1141f4c"

# Define date range
start_date = "2018-01-01"
end_date = "2024-12-31"

# Define FRED tickers (including monthly S&P 500 index)
symbols_fred = {
    "S&P 500": "SP500",
    "10Y Treasury Rate": "GS10",
    "Fed Funds Rate": "FEDFUNDS",
    "Consumer Price Index (CPI)": "CPIAUCNS",
    "Unemployment Rate": "UNRATE",
    "GDP": "GDP"
}

# Function to get FRED data via pandas_datareader
def get_data_fred(series_id, start, end):
    return web.DataReader(series_id, 'fred', start, end)

# Dictionary to store data
data_dict = {}

# Get economic data from FRED
for name, series_id in symbols_fred.items():
    try:
        print(f"Downloading {name} ({series_id}) from FRED...")
        data = get_data_fred(series_id, start_date, end_date)
        data_dict[name] = data[series_id]
    except Exception as e:
        print(f"Failed to get {name} from FRED: {e}")

# Combine data into a single DataFrame
combined_df = pd.DataFrame(data_dict)

# Save to CSV
combined_df.to_csv("economic_indicators_2018_2024.csv")

print("Data collection complete. Saved to 'economic_indicators_2018_2024.csv'.")
