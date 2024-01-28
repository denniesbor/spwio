import { useAppContext } from "../AppContext";

const Sidebar = () => {
  const {
    sidebarVal,
    menuItems,
    handleSidebarClick,
    economicDataOptions,
    selectedEconomicData,
    handleSidebarData,
  } = useAppContext();

  return (
    <div className="sidebar">
      {menuItems.map((item, index) => (
        <button
          key={index}
          className={`menu-item ${sidebarVal === item.value ? "active" : ""}`}
          onClick={() => handleSidebarClick(item.value)}
        >
          {item.name}
        </button>
      ))}

      {sidebarVal === "nerc" && (
        <div className="nerc-data-selection-container">
          <label className="nerc-data-selection-label">
            Select Economic Data to Visualize:
          </label>
          {economicDataOptions.map((option, index) => (
            <button
              key={index}
              className={`menu-item ${
                selectedEconomicData === option.value ? "active" : ""
              }`}
              onClick={() => handleSidebarData(option.value)}
            >
              {option.label}
            </button>
          ))}
        </div>
      )}
    </div>
  );
};

export default Sidebar;
