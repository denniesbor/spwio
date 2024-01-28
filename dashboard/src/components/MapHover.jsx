import { useEffect, useCallback } from "react";
import { useAppContext } from "../AppContext";

const MapHover = () => {
  const { regionData, regionName, regionNames, fetchDataForRegion, mouseLocation } =
    useAppContext();

  const fetchData = useCallback(() => {
    if (regionName) {
      fetchDataForRegion();
    }
  }, [regionName]);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  const regionNameFull = regionNames[regionName] || regionName;

 const bannerStyle = {
   position: "absolute",
   left: `${mouseLocation.x + 10}px`,
   top: `${mouseLocation.y - 100}px`, 
 };

  return (
    <div>
      {regionData && (
        <div className="info-banner" style={bannerStyle}>
          <h3>{regionNameFull}</h3>
          <p>Business Count: {regionData.businessCount}</p>
          <p>GDP ($Bn): {parseFloat((regionData.dailyGDP * 365).toFixed(2))}</p>
          <p>2020 Population: {regionData.population2020}</p>
        </div>
      )}
    </div>
  );
};

export default MapHover;
