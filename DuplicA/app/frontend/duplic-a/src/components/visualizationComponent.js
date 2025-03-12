import React, { useState, useEffect } from "react";
import "../styles/Visualization.css"
import visualizationOptions from './visualization_options.json';

const VisualizationComponent = () => {
    //const jsonFilePath = "./visualization_options.json"; // JSON is in /public/
    const figurePath = "./Figure.png"; // Image is in /public/

    const [options, setOptions] = useState(visualizationOptions);
    const [params, setParams] = useState({});
    const [showAdditionalOptions, setShowAdditionalOptions] = useState(false);
    const [imageSrc, setImageSrc] = useState("");

    // Load JSON options dynamically
    useEffect(() => {
        const initialParams = {};
        Object.keys(visualizationOptions).forEach((key) => {
            if (visualizationOptions[key].default) {
                initialParams[key] = visualizationOptions[key].default[0];
            } else {
                initialParams[key] = "";
            }
        });
        setParams(initialParams);
    }, []);

    // Function to update visualization through R API
    const updateVisualization = (updatedParams) => {
        if (!options) {
            console.error("No options");
            return;
        }

        const formattedParams = {};

        Object.keys(options).forEach((key) => {
            let value = updatedParams[key];

            // Ensure select inputs always have a value
            if (!value && options[key].type[0] === "select" && options[key].options.length > 0) {
                value = options[key].options[0]; // Use the first available option
            }

            // If it's optional and empty, send null
            formattedParams[key] = value === "" ? null : value;
        });

        console.log("Sending parameters to R API:", formattedParams); // Debugging

        fetch("http://localhost:8000/makeImage", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(formattedParams),
        })
            .then((response) => response.json()) // Parse response
            .then((data) => {
                console.log("R API Response:", data);

                // Wait for the R script to generate the new image
                setTimeout(() => {
                    setImageSrc(`/Figure.png?${new Date().getTime()}`); // Force reload of new image
                }, 2000); // Wait 2 seconds to allow image to generate
            })
            .catch((error) => console.error("Error updating visualization:", error));
    };

    // Handle parameter changes
    const handleChange = (e) => {
        const { name, value, type, checked } = e.target;
        const newValue = type === "checkbox" ? checked : value;

        // Update state and trigger API call immediately
        setParams((prev) => {
            const updatedParams = { ...prev, [name]: newValue };
            updateVisualization(updatedParams); // Call R API every time a parameter changes
            return updatedParams;
        });
    };

    // Show loading state if options are not yet loaded
    if (!options) {
        return <p>Loading options...</p>;
    }

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
                <div className="options-panel">
                    {/* Figure Type */}
                    <label>Figure type:</label>
                    <select
                        name="figure_type"
                        value={params.figure_type || options?.figure_type?.options?.[0] || ""}
                        onChange={handleChange}
                    >
                        {options?.figure_type?.options?.map((option, index) => (
                            <option key={index} value={option}>
                                {option}
                            </option>
                        ))}
                    </select>

                    {/* X-Axis */}
                    <label>x-axis:</label>
                    <select
                        name="x_axis"
                        value={params.x_axis || options?.x_axis?.options?.[0] || ""}
                        onChange={handleChange}
                    >
                        {options?.x_axis?.options?.map((option, index) => (
                            <option key={index} value={option}>
                                {option}
                            </option>
                        ))}
                    </select>

                    {/* Y-Axis */}
                    <label>y-axis:</label>
                    <select
                        name="y_axis"
                        value={params.y_axis || options?.y_axis?.options?.[0] || ""}
                        onChange={handleChange}
                    >
                        {options?.y_axis?.options?.map((option, index) => (
                            <option key={index} value={option}>
                                {option}
                            </option>
                        ))}
                    </select>

                    {/* Color Groups */}
                    <label>Color by group:</label>
                    <select
                        name="color_groups"
                        value={params.color_groups || options?.color_groups?.options?.[0] || ""}
                        onChange={handleChange}
                    >
                        {options?.color_groups?.options?.map((option, index) => (
                            <option key={index} value={option}>
                                {option}
                            </option>
                        ))}
                    </select>

                    {/* Separate Figure */}
                    <label>Separate figure:</label>
                    <select
                        name="separate_figure"
                        value={params.separate_figure || options?.separate_figure?.options?.[0] || ""}
                        onChange={handleChange}
                    >
                        {options?.separate_figure?.options?.map((option, index) => (
                            <option key={index} value={option}>
                                {option}
                            </option>
                        ))}
                    </select>
                </div>

                {/* Show Additional Options on the Right When Toggled */}
                {showAdditionalOptions && options && (
                    <div className="additional-options-container">
                        <div className="additional-options-column">
                            {/* Title */}
                            <div className="option-item">
                                <label>Title:</label>
                                <input
                                    type="text"
                                    name="title"
                                    value={params.title || ""}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* X-Axis Label */}
                            <div className="option-item">
                                <label>x-axis label:</label>
                                <input
                                    type="text"
                                    name="x_axis_label"
                                    value={params.x_axis_label || ""}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* Y-Axis Label */}
                            <div className="option-item">
                                <label>y-axis label:</label>
                                <input
                                    type="text"
                                    name="y_axis_label"
                                    value={params.y_axis_label || ""}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* Legend Label */}
                            <div className="option-item">
                                <label>Legend label:</label>
                                <input
                                    type="text"
                                    name="legend_label"
                                    value={params.legend_label || ""}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* Point Size */}
                            <div className="option-item">
                                <label>Point size:</label>
                                <input
                                    type="number"
                                    name="point_size"
                                    value={params.point_size || options?.point_size?.default?.[0] || 3}
                                    onChange={handleChange}
                                />
                            </div>
                        </div>

                        <div className="additional-options-column">
                            {/* Custom Theme */}
                            <div className="option-item">
                                <label>Custom theme:</label>
                                <select
                                    name="custom_theme"
                                    value={params.custom_theme || options?.custom_theme?.options?.[0] || ""}
                                    onChange={handleChange}
                                >
                                    {options?.custom_theme?.options?.map((option, index) => (
                                        <option key={index} value={option}>
                                            {option}
                                        </option>
                                    ))}
                                </select>
                            </div>

                            {/* Log Scale X */}
                            <div className="option-item">
                                <label>Log-scale x:</label>
                                <input
                                    type="checkbox"
                                    name="log_scale_x"
                                    checked={params.log_scale_x || options?.log_scale_x?.default?.[0] || false}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* Log Scale Y */}
                            <div className="option-item">
                                <label>Log-scale y:</label>
                                <input
                                    type="checkbox"
                                    name="log_scale_y"
                                    checked={params.log_scale_y || options?.log_scale_y?.default?.[0] || false}
                                    onChange={handleChange}
                                />
                            </div>

                            {/* Color Set */}
                            <div className="option-item">
                                <label>Color set:</label>
                                <select
                                    name="color_set"
                                    value={params.color_set || options?.color_set?.options?.[0] || ""}
                                    onChange={handleChange}
                                >
                                    {options?.color_set?.options?.map((option, index) => (
                                        <option key={index} value={option}>
                                            {option}
                                        </option>
                                    ))}
                                </select>
                            </div>
                        </div>
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

export default VisualizationComponent;