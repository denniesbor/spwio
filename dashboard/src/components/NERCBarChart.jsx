import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
} from "chart.js";
import { Bar } from "react-chartjs-2";
import { useAppContext } from "../AppContext";

ChartJS.register(
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend
);

const BarComponent = () => {
  const { economic_data, selectedEconomicData } = useAppContext();

  const gridData = {
    labels: Object.keys(economic_data[selectedEconomicData]),
    datasets: [
      {
        label: "FERC Regions",
        data: Object.values(economic_data[selectedEconomicData]),
        backgroundColor: "rgba(54, 162, 235, 0.5)",
        borderColor: "rgba(54, 162, 235, 1)",
        borderWidth: 1,
      },
    ],
  };

  const getTitleAndLabel = (selectedEconomicData) => {
    switch (selectedEconomicData) {
      case "EST":
        return {
          title: "Business Establishments (millions)",
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
