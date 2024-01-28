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
  const { scenarioData, sidebarVal, estLabels } = useAppContext();
 const { direct, leon, gdp } = scenarioData;
 
  const data = {
    labels: estLabels,
    datasets: [
      {
        label: "Direct",
        data: direct,
        backgroundColor: "rgba(255, 99, 132, 0.5)",
        borderColor: "rgba(255, 99, 132, 1)",
      },
      {
        label: "Indirect (Demand)",
        data: leon,
        backgroundColor: "rgba(54, 162, 235, 0.5)",
        borderColor: "rgba(54, 162, 235, 1)",
      },
      {
        label: "Indirect (Supply)",
        data: gdp,
        backgroundColor: "rgba(75, 192, 192, 0.5)",
        borderColor: "rgba(75, 192, 192, 1)",
      },
    ],
  };

  const options = {
    plugins: {
      title: {
        display: true,
        text: `Direct and Indirect Losses by NAICS ($billion) for Scenario ${
          sidebarVal.charAt(0).toUpperCase() + sidebarVal.slice(1)
        }`,
        font: { size: 18, color: "#000000" },
      },
    },
    responsive: true,
    scales: {
      x: {
        title: {
          display: true,
          text: "NAICS Code",
          font: { size: 18, color: "#000000" },
        },
        stacked: true,
      },
      y: {
        stacked: true,
      },
    },
  };

  return <Bar data={data} options={options} />;
};

export default BarComponent;