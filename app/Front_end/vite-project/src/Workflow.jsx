import React, { useState, useEffect } from "react";
import axios from "axios";
import "./App.css";
import parameterOptions from "./multispecies_model_options.json"; // Import JSON

const ToggleButton = ({ label, onToggle }) => {
  const [active, setActive] = useState(false);

  const handleClick = () => {
    const newState = !active;
    setActive(newState);
    onToggle(label, newState);
  };

  return (
    <button className={`toggle-button ${active ? "active" : ""}`} onClick={handleClick}>
      {label}
    </button>
  );
};

const Workflow = () => {
  const [isPopupVisible, setIsPopupVisible] = useState(false);
  const [selectedModels, setSelectedModels] = useState([]);
  const [requiredParameters, setRequiredParameters] = useState({});
  const [additionalParameters, setAdditionalParameters] = useState({});
  const [userInputs, setUserInputs] = useState({});
  const [isMinimized, setIsMinimized] = useState(false);
  const [showAdditionalParams, setShowAdditionalParams] = useState(false);

  // Fetch model parameters from R API whenever selected models change
  useEffect(() => {
    if (selectedModels.length === 0) {
      setRequiredParameters({});
      setAdditionalParameters({});
      return;
    }

    const fetchModelInputs = async () => {
      try {
        const response = await axios.post("http://localhost:8000/getInputs", {
          selected_models: selectedModels,
          parameters: userInputs,
        });

        console.log("API Response:", response.data);

        const requiredList = response.data.required_parameter_list || [];
        const additionalList = response.data.additional_parameter_list || [];

        const matchedRequired = requiredList.reduce((acc, param) => {
          if (parameterOptions[param]) {
            acc[param] = { ...parameterOptions[param], value: userInputs[param] ?? parameterOptions[param].default ?? "" };
          }
          return acc;
        }, {});

        const matchedAdditional = additionalList.reduce((acc, param) => {
          if (parameterOptions[param]) {
            acc[param] = { ...parameterOptions[param], value: userInputs[param] ?? parameterOptions[param].default ?? "" };
          }
          return acc;
        }, {});

        setRequiredParameters(matchedRequired);
        setAdditionalParameters(matchedAdditional);
      } catch (error) {
        console.error("Error fetching input fields:", error);
      }
    };

    fetchModelInputs();
  }, [selectedModels]);


  
  const modelMapping = {
    "Public Datasets": "Public Datasets",
    "OrthoFinder": "OrthoFinder",
    "CDROM": "CDROM",
    "Dn/Ds": "dnds",
    "EVE Expression Shift": "expression_shift",
    "EVE Diversity/Divergence": "diversity_divergence",
    "AlphaFold": "alphafold_db",
    "Post-duplication Fates": "postduplication_fates",
    "Duplication Mechanism": "duplication_mechanism",
    "Pathway Analysis": "pathway"
  };
    const handleToggle = (modelName, isActive) => {
      const apiModelName = modelMapping[modelName]; // Map button label to expected API name
      setSelectedModels((prevModels) =>
        isActive
          ? [...prevModels, apiModelName]
          : prevModels.filter((m) => m !== apiModelName)
      );
    };
    

  useEffect(() => {
    setIsPopupVisible(selectedModels.length > 0);
  }, [selectedModels]);

  // Handle input changes
  const handleInputChange = (param, value) => {
    setUserInputs((prev) => ({
      ...prev,
      [param]: value,
    }));
  };

  // Render dynamic input fields with custom styles
  const renderInputField = (param, details) => {
    const value = userInputs[param] ?? details.value;

    return (
      <div className="parameter-field" key={param}>
        <label>{details.label || param}:</label>
        {details.type === "text" && <input type="text" value={value} onChange={(e) => handleInputChange(param, e.target.value)} />}
        {details.type === "number" && <input type="number" value={value} onChange={(e) => handleInputChange(param, e.target.value)} />}
        {details.type === "boolean" && <input type="checkbox" checked={Boolean(value)} onChange={(e) => handleInputChange(param, e.target.checked)} />}
        {details.type === "select" && (
          <select value={value} onChange={(e) => handleInputChange(param, e.target.value)}>
            {details.choices.map((choice, idx) => (
              <option key={idx} value={choice}>{choice}</option>
            ))}
          </select>
        )}
        {details.type === "file" && <input type="file" multiple={details.multiple ?? false} />}
      </div>
    );
  };

  return (
    <div className="workflow-container">
    {/* <header className="header">
        <h1 className="logo">DuplicA</h1>
        <nav className="nav">
          <a href="#home" className="nav-link">Home</a>
          <a href="#workflow" className="nav-link active">WorkFlow</a>
          <a href="#visualization" className="nav-link">Visualization</a>
          <a href="#hypothesis" className="nav-link">Hypothesis Testing</a>
          <a href="#about" className="nav-link">About</a>
        </nav>
      </header> */}

      <div className="workflow">
        <div className="workflow-content">
          <div className="workflow-row">
            <ToggleButton label="Public Datasets" onToggle={handleToggle} />
            <div className="arrow-line">→</div>
            <ToggleButton label="OrthoFinder" onToggle={handleToggle} />
            <div className="arrow-line">→</div>
          </div>

          <div className="models-wrapper">
            <div className="model-box">
              <h3>Functional Models</h3>
              <div className="button-group">
                <ToggleButton label="AlphaFold" onToggle={handleToggle} />
                <ToggleButton label="CDROM" onToggle={handleToggle} />
                <ToggleButton label="EVE Expression Shift" onToggle={handleToggle} />
                <ToggleButton label="Gene Ontology" onToggle={handleToggle} />
                <ToggleButton label="Pathway Analysis" onToggle={handleToggle} />
                <ToggleButton label="Post-duplication Fates" onToggle={handleToggle} />
              </div>
            </div>

            <div className="model-box">
              <h3>Mechanism Models</h3>
              <div className="button-group">
                <ToggleButton label="Duplication Mechanism" onToggle={handleToggle} />
              </div>
            </div>

            <div className="model-box">
              <h3>Selection Models</h3>
              <div className="button-group">
                <ToggleButton label="Dn/Ds" onToggle={handleToggle} />
                <ToggleButton label="EVE Diversity/Divergence" onToggle={handleToggle} />
              </div>
            </div>
          </div>
        </div>
      </div>

      {isPopupVisible && (
        <div className={`popup-container ${isMinimized ? "minimized" : "visible"}`}>
          <button className="popup-toggle" onClick={() => setIsMinimized(!isMinimized)}>
            {isMinimized ? "▲" : "▼"}
          </button>

          <div className="form-section required-parameters">
            <h3>Required Parameters</h3>
            {Object.entries(requiredParameters).map(([param, details]) => renderInputField(param, details))}
          </div>

          <button
            className="additional-options-button"
            onClick={() => setShowAdditionalParams(!showAdditionalParams)}
          >
            Additional Options
          </button>

          <div className={`form-section additional-parameters ${showAdditionalParams ? "visible" : "hidden"}`}>
            <h3>Additional Parameters</h3>
            {Object.entries(additionalParameters).map(([param, details]) => renderInputField(param, details))}
          </div>
        </div>
      )}
    </div>
  );
};

export default Workflow;
