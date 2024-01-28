import { useRef } from "react";
import { useAppContext } from "../AppContext";
import Legend from "./NERCLegend";

import useMapbox from "./useMapbox";

const MapComponent = () => {
  const { sidebarVal, isMapLoaded } = useAppContext();

  const mapContainerRef = useRef(null);
  const mapOptions = {
    style: "mapbox://styles/mapbox/light-v10",
    center: [-98.5795, 39.8283],
    zoom: 4,
  };

  useMapbox(mapContainerRef, mapOptions, sidebarVal);

  // Conditional rendering based on sidebarVal
  return (
    <div className="map-container">
      <div id="map" ref={mapContainerRef}></div>
      {isMapLoaded && (
        <div className="map-overlay" id="legend">
          <Legend />
        </div>
      )}
    </div>
  );
};

export default MapComponent;
