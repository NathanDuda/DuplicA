import React, { useState, useEffect } from "react";
import "../styles/Visualization.css";
import visualizationOptions from './visualization_options.json';

const VisualizationComponent = () => {
    const [options, setOptions] = useState(visualizationOptions);
    const [params, setParams] = useState({});
    const [showAdditionalOptions, setShowAdditionalOptions] = useState(false);
    const [imageSrc, setImageSrc] = useState("");

    // Load JSON options dynamically
    useEffect(() => {
        const initialParams = {};
        Object.keys(visualizationOptions).forEach((key) => {
            if (visualizationOptions[key].default) {
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
                value = options[key].options[0];
            }

            formattedParams[key] = value === "" ? null : value;
        });

        fetch("http://localhost:8001/makeImage", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(formattedParams),
        })
            .then((response) => response.json())
            .then((data) => {
                setTimeout(() => {
                    setImageSrc(`http://localhost:8002/Figure.png?${new Date().getTime()}`);
                }, 2000);
            })
            .catch((error) => console.error("Error updating visualization:", error));
    };

    // Handle parameter changes
    const handleChange = (e) => {
        const { name, value, type, checked } = e.target;
        const newValue = type === "checkbox" ? checked : value;

        setParams((prev) => {
            const updatedParams = { ...prev, [name]: newValue };
            updateVisualization(updatedParams);
            return updatedParams;
        });
    };

    if (!options) {
        return <p>Loading options...</p>;
    }

    return (
        <div className="visualization-container">
            {/* Chart at the top */}
            <div className="figure-display">
                {imageSrc ? (
                    <img src={imageSrc} alt="Generated Visualization" />
                ) : (
                    <p>Please run a workflow before coming to this page</p>
                )}
            </div>

            {/* Options container below chart */}
            <div className="options-container">
                {/* Main options panel - now with 2 columns */}
                <div className="main-options">
                    <div className="option-section">
                        <h3>Main Options</h3>
                        <div className="main-options-grid">
                            {/* Column 1 */}
                            <div className="options-column">
                                <div className="option-item">
                                    <label>Figure type:</label>
                                    <select name="figure_type" value={params.figure_type} onChange={handleChange}>
                                        {options?.figure_type?.options?.map((option, index) => (
                                            <option key={index} value={option}>{option}</option>
                                        ))}
                                    </select>
                                </div>

                                <div className="option-item">
                                    <label>x-axis:</label>
                                    <select name="x_axis" value={params.x_axis} onChange={handleChange}>
                                        {options?.x_axis?.options?.map((option, index) => (
                                            <option key={index} value={option}>{option}</option>
                                        ))}
                                    </select>
                                </div>

                                <div className="option-item">
                                    <label>Color by group:</label>
                                    <select name="color_groups" value={params.color_groups} onChange={handleChange}>
                                        {options?.color_groups?.options?.map((option, index) => (
                                            <option key={index} value={option}>{option}</option>
                                        ))}
                                    </select>
                                </div>
                            </div>

                            {/* Column 2 */}
                            <div className="options-column">
                                <div className="option-item">
                                    <label>y-axis:</label>
                                    <select name="y_axis" value={params.y_axis} onChange={handleChange}>
                                        {options?.y_axis?.options?.map((option, index) => (
                                            <option key={index} value={option}>{option}</option>
                                        ))}
                                    </select>
                                </div>

                                <div className="option-item">
                                    <label>Separate figure:</label>
                                    <select name="separate_figure" value={params.separate_figure} onChange={handleChange}>
                                        {options?.separate_figure?.options?.map((option, index) => (
                                            <option key={index} value={option}>{option}</option>
                                        ))}
                                    </select>
                                </div>

                                <div className="option-item">
                                    <label>Title:</label>
                                    <input type="text" name="title" value={params.title} onChange={handleChange} />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                {/* Additional options panel */}
                {showAdditionalOptions && (
                    <div className="additional-options">
                        <div className="option-section">
                            <h3>Additional Options</h3>
                            <div className="additional-options-grid">
                                {/* Column 1 */}
                                <div className="options-column">
                                    <div className="option-item">
                                        <label>x-axis label:</label>
                                        <input type="text" name="x_axis_label" value={params.x_axis_label} onChange={handleChange} />
                                    </div>

                                    <div className="option-item">
                                        <label>y-axis label:</label>
                                        <input type="text" name="y_axis_label" value={params.y_axis_label} onChange={handleChange} />
                                    </div>

                                    <div className="option-item">
                                        <label>Legend label:</label>
                                        <input type="text" name="legend_label" value={params.legend_label} onChange={handleChange} />
                                    </div>

                                    <div className="checkbox-item">
                                        <input type="checkbox" name="log_scale_x" checked={params.log_scale_x} onChange={handleChange} />
                                        <label>log-scale x:</label>
                                    </div>
                                </div>

                                {/* Column 2 */}
                                <div className="options-column">
                                    <div className="option-item">
                                        <label>Point size:</label>
                                        <input type="number" name="point_size" value={params.point_size} onChange={handleChange} />
                                    </div>

                                    <div className="option-item">
                                        <label>Custom theme:</label>
                                        <select name="custom_theme" value={params.custom_theme} onChange={handleChange}>
                                            {options?.custom_theme?.options?.map((option, index) => (
                                                <option key={index} value={option}>{option}</option>
                                            ))}
                                        </select>
                                    </div>

                                    <div className="checkbox-item">
                                        <input type="checkbox" name="log_scale_y" checked={params.log_scale_y} onChange={handleChange} />
                                        <label>log-scale y:</label>
                                    </div>

                                    <div className="option-item">
                                        <label>Color set:</label>
                                        <select name="color_set" value={params.color_set} onChange={handleChange}>
                                            {options?.color_set?.options?.map((option, index) => (
                                                <option key={index} value={option}>{option}</option>
                                            ))}
                                        </select>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                )}
            </div>
            <div className="buttons">
                <button onClick={() => setShowAdditionalOptions(!showAdditionalOptions)}>
                    {showAdditionalOptions ? 'Hide Options' : 'Additional Options'}
                </button>
                <button onClick={() => window.open(imageSrc, "_blank")}>Download Figure</button>
            </div>
        </div>
    );
};

export default VisualizationComponent;