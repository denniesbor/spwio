import React from "react";
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
} from "chart.js";
import {
  BarWithErrorBarsController,
  BarWithErrorBar,
} from "chartjs-chart-error-bars";
import { Bar } from "react-chartjs-2";
import { useAppContext } from "../AppContext";

ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  BarWithErrorBarsController, 
  BarWithErrorBar,
  Title,
  Tooltip,
  Legend
);

const BarComponent = () => {
  const { economic_data, selectedEconomicData } = useAppContext();

  const prepareDataWithErrors = (data) => {
    return data.map((value) => ({
      y: value,
      yMin: value * 0.8, // 10% less than the actual value
      yMax: value * 1.2, // 10% more than the actual value
    }));
  };

  const gridData = {
    labels: Object.keys(economic_data[selectedEconomicData]),
    datasets: [
      {
        label: "FERC Regions",
        data: prepareDataWithErrors(
          Object.values(economic_data[selectedEconomicData])
        ),
        backgroundColor: "rgba(54, 162, 235, 0.5)",
        borderColor: "rgba(54, 162, 235, 1)",
        borderWidth: 2,
        errorBarColor: "#eb4034",
        errorBarLineWidth: 2,
        type: "barWithErrorBars",
      },
    ],
  };

  const getTitleAndLabel = (selectedEconomicData) => {
    switch (selectedEconomicData) {
      case "EST":
        return {
          title: "Business Establishments (Millions)",
          xAxisLabel: "FERC Order 1000 Regional Operators",
        };
      case "POP20":
        return {
          title: "2020 Decennial Census Population",
          xAxisLabel: "FERC Order 1000 Regional Operators",
        };
      case "DailyGDP":
        return {
          title: "Daily GDP ($ billion)",
          xAxisLabel: "FERC Order 1000 Regional Operators",
        };
      default:
        return { title: "Data Visualization", xAxisLabel: "Categories" };
    }
  };

  const { title, xAxisLabel } = getTitleAndLabel(selectedEconomicData);

  const options = {
    plugins: {
      title: {
        display: true,
        text: title,
        font: { size: 18, color: "#000000" },
      },
    },
    scales: {
      x: {
        title: {
          display: true,
          text: xAxisLabel,
          font: { size: 18, color: "#000000" },
        },
      },
      y: {
        beginAtZero: true,
      },
    },
  };

  return <Bar data={gridData} options={options} />;
};

export default BarComponent;
