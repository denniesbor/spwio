"""
Economic Input-Output Models using Leontief and Ghosh

- Ghosh is used to calculate the supply chain impacts due to change in value added.
- Leontief is used to calculate the change in industrial output due to change in final demand.
"""

import os
import numpy as np
import pandas as pd

# Set the global data path
data_path = os.path.join(os.getcwd(), "..", "data")


class InputOutputModel:
    def __init__(self, value_added=None, final_demand=None):
        """
        Initialize the InputOutputModel with value added or final demand data.

        Parameters:
        - value_added (numpy.ndarray, optional): Initial value added data (Ghosh).
        - final_demand (numpy.ndarray, optional): Initial final demand data (Leontief).
        """
        self.final_demand = final_demand
        self.value_added = value_added
        self.A = None  # Technical coefficients matrix
        self.I = None  # Identity matrix
        self.B = None  # B matrix for Ghosh model
        self.industry_output = None  # Industry output vector
        self.load_input_data()
        self.calculate_B_matrix()

    def load_input_data(self):
        """Load the input data, including the technical coefficients matrix and industry output."""
        # Technical coefficients matrix
        technology_matrix = pd.read_csv(os.path.join(data_path, "direct_requirements.csv"))

        # A matrix (trim the value added in the IO matrix)
        self.A = technology_matrix.iloc[:15].to_numpy()

        # Identity matrix
        self.I = np.eye(self.A.shape[1])

        # Real 2022 US gross output by industries, in billion dollars
        self.industry_output = (
            pd.read_csv(os.path.join(data_path, "us_gross_output.csv"), usecols=["2022Q3"])
            .to_numpy()
            .reshape(-1)
        )

    def calculate_B_matrix(self):
        """Calculate the B matrix for the Ghosh model using the industry output and the A matrix."""
        # Diagonalize the industry output (x_hat)
        gross_diag = np.diag(self.industry_output)

        # Inverse of the diagonalized industry output (x_hat^-1)
        gross_diag_inv = np.linalg.inv(gross_diag)

        # Compute the B matrix
        self.B = np.dot(np.dot(gross_diag_inv, self.A), gross_diag)

    def leontief(self):
        """
        Calculate the total output of each sector using the Leontief model.

        Returns:
        numpy.ndarray: The output vector representing the total output of each sector.
        """
        if self.final_demand is not None:
            return np.linalg.inv(self.I - self.A).dot(self.final_demand)

    def ghosh(self):
        """
        Calculate the supply chain impacts due to change in value added using the Ghosh model.

        Returns:
        numpy.ndarray: The total downstream impacts.
        """
        if self.value_added is not None:
            # Ghosh model calculation
            self.ghosh_model = np.linalg.inv(self.I - self.B)

            # Transpose value added
            va_transposed = np.transpose(self.value_added)

            # Change in Gross output is v' @ Ghosh
            delta_va = va_transposed @ self.ghosh_model

            return delta_va
