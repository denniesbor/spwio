// AppContext.js
import  { createContext, useContext, useState, useEffect } from "react";
import economic_data from "./data/grid_data.json";
import impact_data from "./data/condensed_aggregated_frame.json";

import {
  regionColors,
  regionNames,
  shocksNAICS,
  shocksRegion,
} from "./data/data";

const AppContext = createContext(null);

export const useAppContext = () => {
  const context = useContext(AppContext);
  if (!context) {
    throw new Error("useAppContext must be used within an AppContextProvider");
  }
  return context;
};

//  Side Bar content //

// Menu Items

const menuItems = [
  { name: "Grid Regions", value: "nerc" },
  { name: "Scenario 1", value: "s1" },
  { name: "Scenario 2", value: "s2" },
  { name: "Scenario 3", value: "s3" },
  { name: "Scenario 4", value: "s4" },
];

// Economic Data of NERC Regions

const economicDataOptions = [
  { label: "Business Count", value: "EST" },
  { label: "Daily GDP", value: "DailyGDP" },
  { label: "2020 Population", value: "POP20" },
];

export const AppContextProvider = ({ children }) => {
  let totalGDP = 93; // Daily US gross output
  let totalPop = 335; // US population
  const [sidebarVal, setSidebarVal] = useState("nerc");
  const [user, setUser] = useState(null);
  const [selectedEconomicData, setSelectedEconomicData] = useState("EST");
  const [regionName, setRegionName] = useState("");
  const [regionData, setRegionData] = useState({});
  const [mouseExit, setMouseExit] = useState(true);
  const [mouseLocation, setMouseLocation] = useState({ x: 0, y: 0 });
  const [estLabels, setEstLabels] = useState(
    Object.keys(shocksNAICS["industry"])
  );
  const [dataEst, setDataEst] = useState([]);
  const [scenarioData, setScenarioData] = useState({});
  const [scenarioRegions, setScenarioRegions] = useState([]);
  const [filteredGeoData, setFilteredGeoData] = useState({});

  // geojsonjson data
  const [geoData, setGeoData] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);
  const [isMapLoaded, setMapLoaded] = useState(false);
  const [scenarioEconomicData, setScenarioEconomicData] = useState({}) // count of buysinesses, population and direct impaact in a region incurring economic losses

  // Fetch the GEOJSON data
  useEffect(() => {
    fetch(
      "https://gist.githubusercontent.com/denniesbor/3b994bec0329a8b4792113ee2ccd4ab3/raw/eff6c4acb2c009d176d630b5b2b2ccefe6df5b1b/nerc_gdf.geojson"
    )
      .then((response) => response.json())
      .then((geoData) => {
        setGeoData(geoData);
        setIsLoading(false);
      })
      .catch((error) => {
        setError(error);
        setIsLoading(false);
      });
  }, []);

  //  Handle sidebar content click
  const handleSidebarClick = (value) => {
    setSidebarVal(value);
  };
  // handle sidebar economic data
  const handleSidebarData = (value) => {
    // console.log(value);
    setSelectedEconomicData(value);
  };

  //  Function to parse json impact data
  function extractDataForScenario(jsonData, scenario) {
    // Transform into an array of objects
    const dataArray = Object.keys(jsonData.Scenario).map((key) => {
      return {
        Scenario: jsonData.Scenario[key],
        Type: jsonData.Type[key],
        NAICSIndustries: jsonData.NAICSIndustries[key],
        Output: jsonData.Output[key],
      };
    });

    // Filter for the specific scenario
    const filteredData = dataArray.filter((item) => item.Scenario === scenario);

    // Optionally, group by Type
    const groupedDataByType = filteredData.reduce((acc, item) => {
      acc[item.Type] = acc[item.Type] || [];
      acc[item.Type].push(item);
      return acc;
    }, {});

    return groupedDataByType;
  }

  useEffect(() => {
    // This effect handles updating scenario data
    const groupedData = extractDataForScenario(impact_data, sidebarVal);

    setScenarioData({
      direct: groupedData.Direct?.map((item) => item.Output) || [],
      leon: groupedData.Leon?.map((item) => item.Output) || [],
      gdp: groupedData.GDP?.map((item) => item.Output) || [],
    });
  }, [impact_data, sidebarVal]);

useEffect(() => {
  // This effect handles updating data based on sidebarVal
  if (sidebarVal === "nerc") {
    // If sidebarVal is "nerc", no need to run the below logic
    return;
  }

  const newDataEst = Object.values(shocksNAICS[sidebarVal]);
  if (JSON.stringify(newDataEst) !== JSON.stringify(dataEst)) {
    setDataEst(newDataEst);
  }

  const newScenarioRegions = Object.entries(shocksRegion[sidebarVal])
    .filter(([region, value]) => value > 0)
    .map(([region, _]) => region);

  if (JSON.stringify(newScenarioRegions) !== JSON.stringify(scenarioRegions)) {
    setScenarioRegions(newScenarioRegions); // Update scenarioRegions in context
  }

  const newFilteredGeoData = {
    ...geoData,
    features: geoData.features.filter((feature) =>
      newScenarioRegions.includes(feature.properties.REGIONS)
    ),
  };

  if (JSON.stringify(newFilteredGeoData) !== JSON.stringify(filteredGeoData)) {
    setFilteredGeoData(newFilteredGeoData);
  }
}, [
  sidebarVal,
  geoData,
  dataEst,
  scenarioRegions,
  filteredGeoData,
  mouseLocation
]);


  // NERC choropleth fill color
  const fillColorArray = ["match", ["get", "REGIONS"]];

  Object.entries(regionColors).forEach(([region, color]) => {
    fillColorArray.push(region, color);
  });

  // Add the default color at the end
  fillColorArray.push("#CCCCCC");

  // Get the region Data on Mouse Hover

  const fetchDataForRegion = () => {
    if (!regionName || !economic_data) return null;

    const businessCount = economic_data.EST[regionName];
    const dailyGDP = economic_data.DailyGDP[regionName];
    const population2020 = economic_data.POP20[regionName];

    setRegionData({ businessCount, dailyGDP, population2020 });
  };

  // Get the total number of businesses, and popualation
useEffect(() => {
  if (economic_data && scenarioData) {
    let totalBusCount = 0;
    let totalDailyGDP = 0;
    let totalPopulation2020 = 0;

    const { direct, leon, gdp } = scenarioData;

    scenarioRegions.forEach((regionName) => {
      totalBusCount += economic_data.EST[regionName];
      totalDailyGDP += economic_data.DailyGDP[regionName];
      totalPopulation2020 += economic_data.POP20[regionName];
    });

    const sumArray = (arr) =>
      arr ? arr.reduce((acc, val) => acc + val, 0) : 0;

    const totalDirect = sumArray(direct);
    const totalLeon = sumArray(leon);
    const totalGDP = sumArray(gdp);

    const total = totalDirect + totalLeon + totalGDP;

setScenarioEconomicData({
  totalBusCount: parseFloat((totalBusCount / 1000000).toFixed(2)),
  totalDailyGDP: parseFloat(totalDailyGDP.toFixed(2)),
  totalPopulation2020: parseFloat((totalPopulation2020 / 1000000).toFixed(2)),
  totalDirect: parseFloat(totalDirect.toFixed(2)),
  totalLeon: parseFloat(totalLeon.toFixed(2)),
  totalGDP: parseFloat(totalGDP.toFixed(2)),
  total: parseFloat(total.toFixed(2)),
});

  }
}, [scenarioRegions, economic_data, scenarioData]);


  const contextValue = {
    totalGDP,
    totalPop,
    isLoading,
    geoData,
    error,
    sidebarVal,
    setMapLoaded,
    scenarioRegions,
    isMapLoaded,
    filteredGeoData,
    setSidebarVal,
    handleSidebarClick,
    scenarioData,
    user,
    setUser,
    dataEst,
    estLabels,
    economic_data,
    scenarioEconomicData,
    selectedEconomicData,
    setSelectedEconomicData,
    handleSidebarData,
    menuItems,
    economicDataOptions,
    regionColors,
    fillColorArray,
    regionNames,
    regionName,
    setRegionName,
    regionData,
    setRegionData,
    fetchDataForRegion,
    mouseExit,
    setMouseExit,
    mouseLocation,
    setMouseLocation,
    shocksNAICS
  };

  return (
    <AppContext.Provider value={contextValue}>{children}</AppContext.Provider>
  );
};
