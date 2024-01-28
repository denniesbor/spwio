import MapComponent from "./MapComponent";
import NERCBarComponent from "./NERCBarChart";
import BarComponent from "./ScenarioBar";
import MapHover from "./MapHover";
import ImpactLegend from "./Impacts";
import { useAppContext } from "../AppContext";

function MainContent() {
  const { sidebarVal, mouseExit, isLoading } = useAppContext();

 return (
   <div className="main-content">
     {isLoading ? (
       <div className="loading-container">
         <div className="loader">Loading...</div>
       </div>
     ) : (
       <>
         <MapComponent />
         {sidebarVal === "nerc" ? (
           <div className="nerc-bar-component">
             <NERCBarComponent />
           </div>
         ) : (
           <div className="nerc-bar-component">
             <BarComponent />
             <ImpactLegend />
           </div>
         )}
         {!mouseExit ? <MapHover /> : null}
       </>
     )}
   </div>
 );
}

export default MainContent;
