import os
import pandas as pd
import numpy as np
import string
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

# Use matplotlib timess roman font
plt.rcParams["font.family"] = "Times New Roman"

# Set the global data path
data_path = os.path.join(os.getcwd(), "..", "data")

# ferg regions geojson
ferc_gdf = gpd.read_file(os.path.join(data_path, "nerc_gdf.geojson"))

# gdf states
gdf_states = gpd.read_file(os.path.join(data_path, "gdf_states.geojson"))

# Figures dir
figures_dir = os.path.join(os.getcwd(), "figures")


class Figures:
    """
    A class to generate figures for scenario analysis of space weather impacts.

    Attributes
    ----------
    us_crs : str
        Coordinate Reference System for the United States.
    cmap : Colormap
        Viridis color map from matplotlib.
    letters : str
        Alphabet letters for labeling.

    Methods
    -------
    cf_plot(scenario_dfs: list, failure_rates: list, grid_failures: list, failure_rates_labels: list = None):
        Plots the cumulative frequency of the possible failure combinations.

    ferc_regions_plot(scenario_percentile_dfs, failure_bounds=["Mean", "1.96sd", "-1.96sd"], variables=['gdp_by_regions', 'pop_affected', 'est_by_regions']):
        Plots FERC regions with data on GDP, population affected, and business establishments.
    """

    def __init__(self, grid_failures):
        """Initialize the Figures class with default attributes."""
        self.us_crs = "epsg:4269"  # Coordinate Reference System for the United States
        self.cmap = plt.cm.viridis  # Viridis color map
        self.letters = string.ascii_uppercase  # Alphabet letters
        self.grid_failures = grid_failures

    def cf_plot(
        self, scenario_dfs: list, failure_rates: list, failure_rates_labels: list = None
    ):
        """
        Plots the cumulative frequency of possible failure combinations.

        Parameters
        ----------
        scenario_dfs : list
            List of DataFrames, each representing a scenario.
        failure_rates : list
            List of failure rates to be highlighted on the plot.
        failure_rates_labels : list, optional
            Labels for the failure rates. If not provided, defaults to failure rates with ' CP' suffix.
        """
        if failure_rates_labels is None or len(failure_rates_labels) != len(
            failure_rates
        ):
            failure_rates_labels = [f"{fr} CP" for fr in failure_rates]

        fig = plt.figure(figsize=(10, 6))

        for i in self.grid_failures:
            df_scenario = scenario_dfs[i - 1]
            df_scenario["Rank"] = df_scenario["GDPSum"].rank(ascending=False)
            n = len(df_scenario)
            df_scenario["Fi"] = df_scenario["Rank"] / (n + 1)
            df_scenario = df_scenario.sort_values("GDPSum", ascending=True)
            plt.plot(
                df_scenario["GDPSum"], df_scenario["Fi"], linestyle="-", label=f"{i}"
            )

        plt.yscale("log")

        for value, label in zip(failure_rates, failure_rates_labels):
            plt.axhline(y=value, color="k", linestyle="--", linewidth=1)
            plt.text(
                x=max(df_scenario["GDPSum"]),
                y=value,
                s=f" {label} CP",
                color="k",
                va="bottom",
                ha="right",
                fontsize=10
            )

        plt.xlabel("Total Daily GDP Impact (US$ Billions)", fontsize=11)
        plt.ylabel("Cumulative Frequency", fontsize=11)
        plt.title(
            "F-N Cumulative Frequency Curves for Heterogeneous Regional Grid Failure Scenarios", fontsize=12
        )
        plt.legend(
            loc="upper center", bbox_to_anchor=(0.5, -0.1), ncol=4, frameon=False
        )
        plt.text(
            x=0.16,
            y=-0.1,
            s="Quantity of Failed\nFERC Regions Per Scenario",
            ha="center",
            va="top",
            transform=plt.gca().transAxes,
            weight="bold",
            fontsize=10,
        )
        plt.grid(True)
        fig.savefig(
            os.path.join(figures_dir, "F-N_curve.png"),
            pad_inches=0.4,
            bbox_inches="tight",
        )
        
        plt.show()
        plt.close()

    def ferc_regions_plot(
        self,
        scenario_percentile_dfs,
        failure_bounds,
        variables=["gdp_by_regions", "pop_affected", "est_by_regions"],
    ):
        """
        Plots FERC regions with data on GDP, population affected, and business establishments.

        Parameters
        ----------
        scenario_percentile_dfs : list
            List of DataFrames, each representing scenario data at different percentiles.
        failure_bounds : list, optional
            Labels for the bounds to plot (default is ["Mean", "1.96sd", "-1.96sd"]).
        variables : list, optional
            List of variables to plot (default is ['gdp_by_regions', 'pop_affected', 'est_by_regions']).
        """
        ncols = 3  # One column for each data type (GDP, population, estimate)
        nrows = len(scenario_percentile_dfs)  # Number of scenarios

        fig = plt.figure(figsize=(12, 8), dpi=300)
        gs = gridspec.GridSpec(nrows, ncols, hspace=0.15, wspace=0)
        self.cmap.set_under("#cccccc")  # Set the background color as grey

        k = 0  # Counter for subplot labeling
        for j, data in enumerate(scenario_percentile_dfs):
            for i, data_type in enumerate(variables):
                ax = fig.add_subplot(gs[j, i])

                df_ = data[data_type]
                if data_type == "est_by_regions":
                    df_["Mean"] = df_["Mean"] / 1e6  # Adjusting units if necessary

                gdf_copy = ferc_gdf.merge(df_, on="REGIONS")
                gdf_copy = gdf_copy.dropna(subset=["Mean"])

                # Plotting
                gdf_copy.to_crs(self.us_crs).plot(
                    "Mean", cmap=self.cmap, ax=ax, vmin=0.1
                )
                gdf_states.to_crs(self.us_crs).boundary.plot(
                    ax=ax, edgecolor="black", linewidth=0.1
                )

                # Setting titles and labels
                title_mappings = {
                    "gdp_by_regions": "Daily GDP (US $Billion)",
                    "pop_affected": "2020 Population (M)",
                    "est_by_regions": "Regional Business Establishment Count (M)",
                }
                ax.set_title(
                    f"({self.letters[k]}) {title_mappings[data_type]}",
                    loc="left",
                    fontsize=7,
                )

                # ScalarMappable for colorbar
                sm = plt.cm.ScalarMappable(
                    cmap=self.cmap,
                    norm=plt.Normalize(vmin=0.1, vmax=gdf_copy["Mean"].max()),
                )
                sm.set_array([])
                cbar = fig.colorbar(sm, ax=ax, shrink=0.5, aspect=10, pad=0.02)
                cbar.ax.tick_params(labelsize=6)

                ax.set_facecolor("#cccccc")
                ax.axis("off")
                k += 1  # Increment subplot label counter

        plt.tight_layout()

        # Adjust the path as necessary
        fig.savefig(
            os.path.join(figures_dir,  "scenario_map_all.png"), bbox_inches="tight"
        )
        plt.show()

    def direct_indirect_plot(self, df):

        num_plots = len(self.grid_failures)  # Number of subplots

        # Create figure and GridSpec with desired number of rows
        fig = plt.figure(figsize=(8.27, 11.69), dpi=300)
        gs = gridspec.GridSpec(num_plots, 1, figure=fig, hspace=0.35)

        # Initialize axes array for storing subplot axes
        axes = []

        for index, grid_num in enumerate(self.grid_failures):
            # Create subplot in specified GridSpec slot
            ax = fig.add_subplot(gs[index, 0])
            axes.append(ax)
            # Filter the DataFrame for mean, lower, and upper bounds for each grid failure Scenario
            mean_df = df[(df['Count'] == grid_num) & (df['Variable'] == 'Mean')].reset_index(drop=True)
            lower_df = df[(df['Count'] == grid_num) & (df['Variable'] == '-1.96sd')].reset_index(drop=True)
            upper_df = df[(df['Count'] == grid_num) & (df['Variable'] == '1.96sd')].reset_index(drop=True)

            # Sort mean_df in ascending order by 'Direct' values
            mean_df_sorted = mean_df.sort_values(by='Total', ascending=True)

            # Reorder lower_df and upper_df to match the sorted order of mean_df
            lower_df_sorted = lower_df.reindex(mean_df_sorted.index)
            upper_df_sorted = upper_df.reindex(mean_df_sorted.index)

            # Horizontal stacked bar chart for 'Downstream' and 'Direct'
            ax.barh(mean_df_sorted['NAICSIndustries'], mean_df_sorted['Downstream'], label='Indirect Impact (Supply)', color='lightblue')
            ax.barh(mean_df_sorted['NAICSIndustries'], mean_df_sorted['Direct'], left=mean_df_sorted['Downstream'], label='Direct Impact', color='orange')

            # Calculate lower and upper errors for 'Direct' and set negative values to zero
            xerr_lower = np.clip(mean_df_sorted['Direct'] - lower_df_sorted['Direct'], 0, None)
            xerr_upper = np.clip(upper_df_sorted['Direct'] - mean_df_sorted['Direct'], 0, None)
            xerr = [xerr_lower, xerr_upper]

            # Error bars for the 'Direct' values
            ax.errorbar(mean_df_sorted['Downstream'] + mean_df_sorted['Direct'], mean_df_sorted['NAICSIndustries'], xerr=xerr, fmt="none",
                        ecolor='red', elinewidth=1, capsize=2, capthick=1)

            # Add a subtitle in the first plot giving more information about the error bars
            if index == 0:
                ax.text(0.0, 1.13, f"({self.letters[index]}) Mean Direct Economic Impact from {self.grid_failures[index]} Electricity Grid Failure Scenarios", transform=ax.transAxes, ha='left', va='bottom', fontsize=8)
                ax.text(0.0, 1.05, f"Reported by NAICS. The error bars indicate results from the simulation 1.96 deviation from group mean", transform=ax.transAxes, ha='left', va='bottom', fontsize=7)
            else:
                # Use text as a title
                ax.text(0.0, 1.05, f"({self.letters[index]}) Mean Direct and Indirect Economic Impact from {self.grid_failures[index]} Electricity Grid Failure Scenarios", transform=ax.transAxes, ha='left', va='bottom', fontsize=8)

            # If it's the last subplot, set the x-axis label
            if index == len(self.grid_failures) - 1:
                ax.set_xlabel('Total Daily GDP Impact (US$ Billions)', fontsize=7)

            ax.set_ylim(-0.5, len(mean_df_sorted['NAICSIndustries']) - 0.5)

            ax.yaxis.set_tick_params(labelsize=7) # Adjust y-axis tick label
            ax.xaxis.set_tick_params(labelsize=7)  # Adjust x-axis tick label

            ax.set_xlim(0, 14)  # Sets the x-axis range from 0 to 6
            ax.grid(True, linestyle='-', linewidth=0.5, color='lightgrey') # Grid
            ax.set_axisbelow(True)  # Ensures grid lines are placed behind

            # Adjust the thickness of the bounding box
            for spine in ax.spines.values():
                spine.set_linewidth(0.5)  # Set to a thinner line

        # Retrieve handles and labels from the first subplot
        handles, labels = axes[0].get_legend_handles_labels()

        # Reorder the handles and labels
        handles[0], handles[1] = handles[1], handles[0]
        labels[0], labels[1] = labels[1], labels[0]

        # Create a legend on the figure withg reordered labels
        fig.legend(handles, labels, loc='upper left', bbox_to_anchor=(0.3, 0.07), ncol=4, frameon=False, fontsize=9)

        # Adjust layout for better readability
        fig.tight_layout()
        fig.savefig(os.path.join(figures_dir, "direct_indirect_plots.png"),
                    bbox_inches='tight')
        
        # Show the entire plot
        plt.show()
        
        plt.close()