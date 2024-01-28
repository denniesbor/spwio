// Legend.jsx
import { useAppContext } from "../AppContext";

const Legend = () => {
const { sidebarVal, scenarioRegions, regionColors } = useAppContext();
let filteredRegionColors = {};

if (scenarioRegions) {
  filteredRegionColors = Object.keys(regionColors)
    .filter((key) => scenarioRegions.includes(key))
    .reduce((obj, key) => {
      obj[key] = regionColors[key];
      return obj;
    }, {});

}

return (
  <>
    {Object.entries(
      sidebarVal === "nerc" ? regionColors : filteredRegionColors
    ).map(([region, color]) => (
      <div key={region} className="legend-item">
        <span className="legend-key" style={{ backgroundColor: color }}></span>
        {region}
      </div>
    ))}
  </>
);

};

export default Legend;
