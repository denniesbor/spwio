import React from "react";
import { useAppContext } from "../AppContext";

const ImpactLegend = () => {
  const { scenarioEconomicData, sidebarVal, totalGDP, totalPop } =
    useAppContext();

  if (!scenarioEconomicData) {
    return null;
  }

  const calculatePercentage = (value, total) => {
    if (total === 0) return "0%";
    return `(${((value / total) * 100).toFixed(2)}%)`;
  };

  return (
    <div className="impacts-legend">
      <h3>{`Economic Impact ${
        sidebarVal.charAt(0).toUpperCase() + sidebarVal.slice(1)
      }`}</h3>
      <ul>
        <li>Business Count (M): {scenarioEconomicData.totalBusCount}</li>
        <li>
          Daily GDP: {scenarioEconomicData.totalDailyGDP.toFixed(2)}{" "}
          {calculatePercentage(scenarioEconomicData.totalDailyGDP, totalGDP)}
        </li>
        <li>
          Population 2020: {scenarioEconomicData.totalPopulation2020}{" "}
          {calculatePercentage(
            scenarioEconomicData.totalPopulation2020,
            totalPop
          )}
        </li>
        <li>
          Direct Impact ($Bn): {scenarioEconomicData.totalDirect.toFixed(2)}{" "}
          {calculatePercentage(scenarioEconomicData.totalDirect, totalGDP)}
        </li>
        <li>
          Indirect-demand ($Bn): {scenarioEconomicData.totalLeon.toFixed(2)}{" "}
          {calculatePercentage(scenarioEconomicData.totalLeon, totalGDP)}
        </li>
        <li>
          Indirect-supply ($Bn): {scenarioEconomicData.totalGDP.toFixed(2)}{" "}
          {calculatePercentage(scenarioEconomicData.totalGDP, totalGDP)}
        </li>
        <li>
          Total Impact ($Bn): {scenarioEconomicData.total.toFixed(2)}{" "}
          {calculatePercentage(scenarioEconomicData.total, totalGDP)}
        </li>
      </ul>
    </div>
  );
};

export default ImpactLegend;
