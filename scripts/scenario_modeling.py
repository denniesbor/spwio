import os
import pickle
from itertools import product

import numpy as np
import pandas as pd

# Set the global data path
data_path = os.path.join(os.getcwd(), "..", "data")


class ScenarioModeling:
    def __init__(self, grid_failures: list, failure_bounds: list):

        self.grid_failures = grid_failures
        self.failure_bounds = failure_bounds
        self.load_prepare_data()
        self.nested_dict = self.states_economic_data()
        self.failure_combinations()
        self.calculate_scenario_percentiles()
        self.get_transformed_data()

    def load_prepare_data(self):
        """
        Load and prepare the input data.

        This method reads the pre processed data and population data from CSV files.
        It then performs various data transformations and aggregations to prepare the data for further analysis.
        The resulting data includes the total population per FERC region, the sum of business establishments and GDP,
        and additional calculated columns such as daily GDP and the share of regional NAICS businesses.

        Returns:
            None
        """

        # Read the processed data
        processed_data = pd.read_csv(
            os.path.join(data_path, "NAICS_EST_GDP2022_ZCTA.csv"), low_memory=False
        )

        # Read the population data
        population_df = pd.read_csv(
            os.path.join(data_path, "2020_decennial_census_at_ZCTA_level.csv"),
            low_memory=False,
        )

        # Unique ZCTA in grid regions
        unique_zcta_regions = processed_data[
            ["ZCTA", "REGIONS", "STABBR"]
        ].drop_duplicates()

        # Merging and aggregating data to get population per FERC region
        regions_pop_df = unique_zcta_regions.merge(population_df, on=["ZCTA", "STABBR"])
        self.total_pop_regions = (
            regions_pop_df.groupby("REGIONS")["POP20"].sum().reset_index()
        )

        # Sum business establishments and GDP (USD millions)
        df_business = (
            processed_data.groupby(["REGIONS", "NAICS"])[["EST", "GDP2022"]]
            .sum()
            .reset_index()
            .assign(GDP2022=lambda x: x.GDP2022 / 1000)
        )  # Convert millions to billions inline

        sum_total_gdp = df_business.GDP2022.sum() * 1 / 365  #  Daily GDP

        # Perform all DataFrame transformations in a single chain
        self.df_business = df_business.assign(
            DailyGDP=lambda x: x.GDP2022 / 365,  # Convert annual GDP to daily GDP
            NAICS=lambda x: x.NAICS.astype(str),  # Convert NAICS code to string
            ESTPerc=lambda x: x.EST / df_business["EST"].sum(),
        ).query(  # Calculate share of regional NAICS businesses
            "REGIONS != 'NotOrder1000'"
        )  # Exclude Non-order 1000 region

        # Sum metrics by FERC regions, merge with total population data, and calculate fraction of daily GDP total GDP
        self.df_index_T = (
            self.df_business.groupby("REGIONS")[["EST", "DailyGDP", "ESTPerc"]]
            .sum()
            .reset_index()
            .merge(self.total_pop_regions, on="REGIONS")
            .assign(DailyGDPPerc=lambda x: x["DailyGDP"] / sum_total_gdp)
            .set_index("REGIONS")
        ).T

        # Subset daily GDP only
        self.daily_gdp_array = self.df_index_T[
            self.df_index_T.index == "DailyGDP"
        ].to_numpy()

    def states_economic_data(self):
        """
        Create a dictionary of states economic data
        by NAICs and regions for the year 2022. The data include
        GDP contribution, establishment count, number of employees, and population of the state

        Returns
        -------
        dict
            A dictionary of states economic data. The dictionary has the following structure:
            {
                CAISO: {
                    "NAICS": {
                        11: {
                            "EST": establishment_count,
                            "ESTPerc": establishment_percentage,
                            "DAILYGDP": daily_gdp
                        },
                        21: {
                            "EST": establishment_count,
                            "ESTPerc": establishment_percentage,
                            "DAILYGDP": daily_gdp
                        },
                        ...
                    },
                    "POP20": 36669676,
                },
                NYISO: {
                    ...
                },
                ...
            }
        """

        # Construct a nested dictionary for business data
        nested_dict = {}
        region_pop = self.total_pop_regions.set_index("REGIONS")

        # Pickle file path
        pickle_file_path = os.path.join(data_path, "nested_dict.pkl")

        if os.path.exists(pickle_file_path):
            with open(pickle_file_path, "rb") as file:
                nested_dict = pickle.load(file)
        else:
            for _, row in self.df_business.iterrows():
                region = row["REGIONS"]
                naics = row["NAICS"]

                if region not in nested_dict:
                    nested_dict[region] = {
                        "NAICS": {},
                        "POP20": region_pop.loc[region, "POP20"],
                    }

                nested_dict[region]["NAICS"][naics] = {
                    "EST": row["EST"],
                    "ESTPerc": row["ESTPerc"],
                    "DAILYGDP": row["DailyGDP"],
                }
            with open(pickle_file_path, "wb") as file:
                pickle.dump(nested_dict, file)

        return nested_dict

    def failure_combinations(self):
        # Unique FERC regions
        regions = self.df_business["REGIONS"].unique()

        # Define states: 0 with power, 1 without power
        states = [0, 1]

        # Generate all possible combinations of states for each region
        combinations = np.asarray(list(product(states, repeat=len(regions))))

        # Generate scenarios
        scenario_dfs = []

        for i in range(1, len(regions) + 1):
            # Filter combinations based on the sum of states being equal to i
            # This ensures we're looking at combinations with exactly i regions without power
            filtered_combinations = combinations[np.sum(combinations, axis=1) == i]

            # Calculate the GDP impact for filtered combinations
            filtered_result = self.daily_gdp_array * filtered_combinations

            # Convert the result to a DataFrame to perform sum and sorting operations
            df_filtered_result = pd.DataFrame(filtered_result, columns=regions)
            df_filtered_result["GDPSum"] = df_filtered_result.sum(axis=1)
            df_filtered_result = df_filtered_result.sort_values(
                by="GDPSum", ascending=True
            )

            scenario_dfs.append(df_filtered_result)

        self.scenario_dfs = scenario_dfs

    def calculate_scenario_percentiles(self, std=1.96):
        """
        Calculate and return dataframes for each grid failure scenario, extracting rows for mean,
        and 1.96 standard deviations away from the mean.

        Parameters:
        - grid_failures: List of integers, each representing a scenario number.
        - std: The multiplier for the standard deviation to calculate bounds (default: 1.96).

        Returns:
        - List of dataframes, each containing rows for the mean, upper, and lower bounds of GDPSum.
        """
        scenario_percentile_dfs = []

        for scenario_number in self.grid_failures:
            current_df = self.scenario_dfs[scenario_number - 1]
            mean_gdp = current_df["GDPSum"].mean()
            sd_gdp = current_df["GDPSum"].std()
            upper_bound = mean_gdp + (std * sd_gdp)
            lower_bound = mean_gdp - (std * sd_gdp)

            closest_to_mean = current_df.iloc[
                (current_df["GDPSum"] - mean_gdp).abs().argsort()[:1]
            ]
            closest_to_upper = current_df.iloc[
                (current_df["GDPSum"] - upper_bound).abs().argsort()[:1]
            ]
            closest_to_lower = current_df.iloc[
                (current_df["GDPSum"] - lower_bound).abs().argsort()[:1]
            ]

            bound_df = pd.concat([closest_to_mean, closest_to_upper, closest_to_lower])
            scenario_percentile_dfs.append(bound_df)

        self.scenario_percentile_dfs = scenario_percentile_dfs

    def get_transformed_data(self):
        """
        Extracts and computes impact on GDP, establishments, and population by NAICS and regions
        from simulated scenarios.

        Returns:
        array-like: lists of Aggregated data including GDP loss, establishment counts, and affected population for each scenario.
        """

        df_index = self.df_index_T.T
        agg_data = []  # Append data for each scenario here

        for df in self.scenario_percentile_dfs:
            # Transpose df for easier row iteration, drop 'GDPSum', and set new column names
            df_transposed = (
                df.drop("GDPSum", axis=1)
                .reset_index(drop=True)
                .T.rename(
                    columns=dict(
                        zip(range(len(self.failure_bounds)), self.failure_bounds)
                    )
                )
            )

            # Initialize lists for GDP and establishment data rows
            gdp_rows, est_rows = [], []
            col_names = ["REGIONS", "NAICS"] + self.failure_bounds

            # Process each region and NAICS to calculate impacts
            for region, naics_data in self.nested_dict.items():
                for naics, data in naics_data["NAICS"].items():
                    gdp_row = [region, naics] + [
                        data["DAILYGDP"] if df_transposed.loc[region, quant] != 0 else 0
                        for quant in self.failure_bounds
                    ]
                    est_row = [region, naics] + [
                        data["EST"] if df_transposed.loc[region, quant] != 0 else 0
                        for quant in self.failure_bounds
                    ]

                    gdp_rows.append(gdp_row)
                    est_rows.append(est_row)

            # Convert lists to DataFrames
            df_gdp = pd.DataFrame(gdp_rows, columns=col_names)
            df_est = pd.DataFrame(est_rows, columns=col_names)

            # Aggregate data by regions and NAICS
            grouped_gdp_naics = df_gdp.groupby("NAICS").sum(numeric_only=True)
            grouped_gdp_regions = df_gdp.groupby("REGIONS").sum(numeric_only=True)
            grouped_est_regions = df_est.groupby("REGIONS").sum(numeric_only=True)
            grouped_est_naics = df_est.groupby("NAICS").sum(numeric_only=True)

            # Calculate percentages and affected population
            gdp_percentage = (
                grouped_gdp_naics / df_index["DailyGDP"].sum(numeric_only=True)
            ).round(2)
            est_binary = grouped_est_regions.ne(0).astype(int)
            pop_affected = (
                est_binary.mul(df_index.loc[est_binary.index, "POP20"], axis=0) / 1e6
            ).round(2)
            pop_percentage = (
                pop_affected / (df_index["POP20"].sum(numeric_only=True) / 1e6)
            ).round(2)

            agg_data.append(
                {
                    "gdp_by_naics": grouped_gdp_naics,
                    "gdp_by_regions": grouped_gdp_regions,
                    "est_by_regions": grouped_est_regions,
                    "est_by_naics": grouped_est_naics,
                    "pop_affected": pop_affected,
                    "pop_percentage": pop_percentage,
                }
            )

        self.transformed_data = agg_data
