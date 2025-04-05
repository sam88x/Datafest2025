import pandas as pd

# Assuming you have loaded your data frames already:
# population, tax_2020, tax_2021, tax_2022, tax_2023
population = pd.read_excel("C:/Users/byerl/Downloads/DataFest/NST-EST2023-POP.xlsx")

tax_2020 = pd.read_excel("C:/Users/byerl\Downloads\DataFest\FY2020-STC-Category-Table-Transposed.xlsx")
tax_2021 = pd.read_excel("C:/Users/byerl\Downloads\DataFest\FY2021-STC-Category-Table-Transposed.xlsx")
tax_2022 = pd.read_excel("C:/Users/byerl\Downloads\DataFest\FY2022-STC-Category-Table-Transposed.xlsx")
tax_2023 = pd.read_excel("C:/Users/byerl\Downloads\DataFest\FY2023-STC-Category-Table-Transposed.xlsx")

# Assuming you have loaded your dataframes already: population, tax_2020, tax_2021, tax_2022, tax_2023

# Step 1: Clean up the column names to remove any newlines or unwanted characters
#population.columns = [col.replace("\n", " ") for col in population.columns]
tax_2020.columns = [col.replace("\n", " ") for col in tax_2020.columns]
tax_2021.columns = [col.replace("\n", " ") for col in tax_2021.columns]
tax_2022.columns = [col.replace("\n", " ") for col in tax_2022.columns]
tax_2023.columns = [col.replace("\n", " ") for col in tax_2023.columns]

# Step 2: Reshape the population data to long format (melt)
population_long = population.melt(id_vars=["Unnamed: 0"], var_name="year", value_name="population")
population_long.rename(columns={"Unnamed: 0":"state"}, inplace=True)
#print(population_long)

# Step 3: Reshape the tax data for each year to long format (melt)
tax_2020_long = tax_2020.melt(id_vars=["Tax Type"], var_name="state", value_name="tax_2020").drop(columns=["Tax Type"])
tax_2021_long = tax_2021.melt(id_vars=["Tax Type"], var_name="state", value_name="tax_2021").drop(columns=["Tax Type"])
tax_2022_long = tax_2022.melt(id_vars=["Tax Type"], var_name="state", value_name="tax_2022").drop(columns=["Tax Type"])
tax_2023_long = tax_2023.melt(id_vars=["Tax Type"], var_name="state", value_name="tax_2023").drop(columns=["Tax Type"])
#print(tax_2020_long)
tax_2020_long["year"] = 2020
tax_2021_long["year"] = 2021
tax_2022_long["year"] = 2022
tax_2023_long["year"] = 2023

# Step 4: Merge the population data with the tax data for each year
merged_2020 = pd.merge(population_long, tax_2020_long, on=["state","year"])
merged_2021 = pd.merge(population_long, tax_2021_long, on=["state","year"])
merged_2022 = pd.merge(population_long, tax_2022_long, on=["state","year"])
merged_2023 = pd.merge(population_long, tax_2023_long, on=["state","year"])

# Step 5: Calculate the tax per capita for each year
merged_2020['tax_per_capita_2020'] = merged_2020['tax_2020'] / merged_2020['population'] * 1000
merged_2021['tax_per_capita_2021'] = merged_2021['tax_2021'] / merged_2021['population'] * 1000
merged_2022['tax_per_capita_2022'] = merged_2022['tax_2022'] / merged_2022['population'] * 1000
merged_2023['tax_per_capita_2023'] = merged_2023['tax_2023'] / merged_2023['population'] * 1000

print(merged_2020)
print(merged_2021)

# Step 6: Merge the tax per capita data for all years
final_df = pd.concat([merged_2020[["state","year","tax_per_capita_2020"]],merged_2021["tax_per_capita_2021"],
                      merged_2022["tax_per_capita_2022"], merged_2023["tax_per_capita_2023"]], axis = 1)

# # Clean up the final dataframe by dropping unnecessary columns
# final_df = final_df.drop(columns=["Unnamed: 0"])

# Show the final dataframe
final_df.to_csv("tax.csv")
