import React, { useState, useEffect } from "react";
import "./Visualization.css";

const Visualization = () => {
  const jsonFilePath = "/visualization_options.json"; // JSON is in /public/
  const figurePath = "/Figure.png"; // Image is in /public/

  const [options, setOptions] = useState(null);
  const [params, setParams] = useState({});
  const [showAdditionalOptions, setShowAdditionalOptions] = useState(false);
  const [imageSrc, setImageSrc] = useState("");

  // Load JSON options dynamically
  useEffect(() => {
    fetch(jsonFilePath)
      .then((response) => response.json())
      .then((data) => {
        setOptions(data);
        const initialParams = {};
        Object.keys(data).forEach((key) => {
          if (data[key].default) {
            initialParams[key] = data[key].default[0];
          } else {
            initialParams[key] = "";
          }
        });
        setParams(initialParams);
      })
      .catch((error) => console.error("Error loading options:", error));
  }, []);


  const updateVisualization = () => {
    if (!options) return;
  
    // Format parameters correctly for R function
    const formattedParams = {};
  
    Object.keys(options).forEach((key) => {
      let value = params[key];
  
      // If the parameter has a default value, use it
      if (!value && options[key].default) {
        value = options[key].default[0]; 
      }
  
      // If it's a select input and has no value, use the first available option
      if (!value && options[key].type[0] === "select" && options[key].options.length > 0) {
        value = options[key].options[0]; // Use the first option as default
      }
  
      // If still empty and not required, send null
      formattedParams[key] = value === "" ? null : value;
    });
  
    console.log("Sending parameters to R API:", formattedParams); // Debugging
  
    fetch("http://localhost:8000/makeImage", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(formattedParams), // Send formatted params
    })
      .then((response) => response.json()) // Parse response
      .then((data) => {
        console.log("R API Response:", data);
        setTimeout(() => {
          setImageSrc(`/Figure.png?${new Date().getTime()}`); // Force reload
        }, 1500);
      })
      .catch((error) => console.error("Error updating visualization:", error));
  };
  
      
  

    // Handle parameter changes
    const handleChange = (e) => {
      const { name, value, type, checked } = e.target;
      const newValue = type === "checkbox" ? checked : value;
  
      setParams((prev) => ({
        ...prev,
        [name]: newValue,
      }));
  
      updateVisualization();
    };
  
  return (
    <div className="visualization-container">
      {/* Image Display at the Top */}
      <div className="figure-display">
        {imageSrc ? (
          <img
            src={imageSrc}
            alt="Generated Visualization"
            onError={(e) => {
              e.target.style.display = "none";
              console.error("Image failed to load:", imageSrc);
            }}
          />
        ) : (
          <p>Generating visualization...</p>
        )}
      </div>

      {/* Options Layout: Basic on Left, Additional on Right */}
      <div className="options-layout">
        {options ? (
                <div className="options-panel">
                <label>Figure type:</label>
                <select name="figureType" value={params.figureType} onChange={handleChange}>
                  {options.figure_type.options.map((option, index) => (
                    <option key={index} value={option}>{option}</option>
                  ))}
                </select>
        
                <label>x-axis:</label>
                <select name="xAxis" value={params.xAxis} onChange={handleChange}>
                  {options.x_axis.options.map((option, index) => (
                    <option key={index} value={option}>{option}</option>
                  ))}
                </select>
        
                <label>y-axis:</label>
                <select name="yAxis" value={params.yAxis} onChange={handleChange}>
                  {options.y_axis.options.map((option, index) => (
                    <option key={index} value={option}>{option}</option>
                  ))}
                </select>
        
                <label>Separate plot:</label>
                <select name="separateFigure" value={params.separateFigure} onChange={handleChange}>
                  {options.separate_figure.options.map((option, index) => (
                    <option key={index} value={option}>{option}</option>
                  ))}
                </select>
              </div>
        
        ) : (
          <p>Loading options...</p>
        )}

        {/* Show Additional Options on the Right When Toggled */}
        {showAdditionalOptions && options && (
          <div className="additional-options">
            {Object.keys(options).slice(5).map((key) => (
              <div key={key}>
                <label>{options[key].label}</label>
                {options[key].type[0] === "text" || options[key].type[0] === "number" ? (
                  <input type={options[key].type[0]} name={key} value={params[key]} onChange={handleChange} />
                ) : options[key].type[0] === "select" ? (
                  <select name={key} value={params[key]} onChange={handleChange}>
                    {options[key].options.map((option, index) => (
                      <option key={index} value={option}>{option}</option>
                    ))}
                  </select>
                ) : (
                  <input type="checkbox" name={key} checked={params[key]} onChange={handleChange} />
                )}
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Buttons at the Bottom */}
      <div className="buttons">
        <button onClick={() => setShowAdditionalOptions(!showAdditionalOptions)}>
          Additional Options
        </button>
        <button onClick={() => window.open(imageSrc, "_blank")}>Download Figure</button>
      </div>
    </div>
  );
};

export default Visualization;
