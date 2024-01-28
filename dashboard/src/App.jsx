import mapboxgl from "mapbox-gl";
import "./App.css";
import { AppContextProvider } from "./AppContext";

// import components
import Sidebar from "./components/Sidebar";
import Navbar from "./components/Navbar";
import MainContent from "./components/MainContent";
import Footer from "./components/footer"

mapboxgl.accessToken =
  "pk.eyJ1IjoibmRhcnVwZXRybyIsImEiOiJjbDFvbmN2djMwNXp3M2NrYjRzM3NsOHJjIn0.3n8aCZ3AcjQZ0ESVA9gTaQ";

function App() {
  return (
    <AppContextProvider>
      <div className="app">
        <Sidebar />
        <div className="content">
          <Navbar />
          <MainContent />
          <Footer />
        </div>
      </div>
    </AppContextProvider>
  );
}

export default App;
