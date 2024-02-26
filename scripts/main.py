"""
Uncertainty Quantification of Economic Impacts from the US Electricity Grid Infrastructure Failure

Authors: Dennies Bor, Edward Oughton, Martin Sprintson
Date: February 25, 2024
Funding: This work is supported by the NSF NCAR Grant.
"""

import pandas as pd
from scenario_modeling import ScenarioModeling as SM
from io_models import InputOutputModel as IOM
from plots import Figures
from naics_industries import NAICSIndustries, sort_key

# Define grid failures and failure bounds
grid_failures = [2, 4, 6, 8]  # Grid outages to be considered
failure_bounds = ["Mean", "1.96sd", "-1.96sd"]  # Statistical bounds
cf_percentiles = [0.01, 0.05, 0.5, 0.95]  # Cumulative frequency percentiles

def main():
    # Initialize scenario modeling with specified parameters
    scenario_models = SM(grid_failures=grid_failures, failure_bounds=failure_bounds)
    sc_dfs = scenario_models.scenario_dfs
    scenario_percentile_dfs = scenario_models.scenario_percentile_dfs
    transformed_data = scenario_models.transformed_data

    # Prepare dataframes for impact analysis
    impact_dfs = []
    for i, (grid_num, data) in enumerate(zip(scenario_models.grid_failures, transformed_data), start=1):
        for failure_bound in scenario_models.failure_bounds:
            # Sort and map NAICS industries
            va = data["gdp_by_naics"][failure_bound].sort_index(key=lambda x: x.map(sort_key.get)).to_frame(name="Output")
            va["NAICS"] = va.index
            va["NAICSIndustries"] = va["NAICS"].map(NAICSIndustries)
            
            # Calculate downstream impacts using the Ghosh model
            ghosh = IOM(value_added=va["Output"].to_numpy())
            downstream_impacts = ghosh.ghosh() - va["Output"].to_numpy()

            # Combine direct and downstream impacts into a single DataFrame
            concat_df = pd.DataFrame({
                "Scenario": f"s{i}",
                "Type": ["Downstream"] * len(downstream_impacts) + ["Direct"] * len(va.index),
                "Count": grid_num,
                "Variable": failure_bound,
                "NAICSIndustries": list(va["NAICSIndustries"]) + list(va["NAICSIndustries"]),
                "Output": list(downstream_impacts) + list(va["Output"]),
            })
            impact_dfs.append(concat_df)

    # Aggregate and process all scenario impact DataFrames
    impact_df = pd.concat(impact_dfs)
    pivot_df = impact_df.pivot_table(index=["Scenario", "Count", "Variable", "NAICSIndustries"], columns="Type", values="Output", aggfunc="sum").reset_index()
    pivot_df["Total"] = pivot_df[["Direct", "Downstream"]].sum(axis=1)
    pivot_df.columns.name = None  # Clean up the DataFrame for display
    df = pivot_df.copy()

    # Initialize the Figures class and plot the results
    fig = Figures(grid_failures=grid_failures)
    fig.cf_plot(sc_dfs, failure_rates=cf_percentiles)  # Plot cumulative frequency
    fig.ferc_regions_plot(transformed_data, failure_bounds=failure_bounds)  # Plot regional data
    fig.direct_indirect_plot(df=df)  # Plot direct and indirect impacts

if __name__ == "__main__":
    main()
