import React, { useState } from "react";
import "../styles/home.css";


const dataTypes = [
    { data: "Species Names", models: ["Public Datasets", "OrthoFinder", "CDROM", "Dn/Ds", "Expression Shift", "Diversity / Divergence", "AlphaFold", "Post-Duplication Fates", "Duplication Mechanism", "Pathway", "Gene Ontology"] },
    { data: "Protein Sequences", models: ["OrthoFinder", "CDROM", "Dn/Ds", "Expression Shift", "Diversity / Divergence", "AlphaFold", "Post-Duplication Fates", "Pathway", "Gene Ontology"] },
    { data: "RNAseq Data", models: ["CDROM", "Expression Shift", "Diversity / Divergence", "Post-Duplication Fates"] },
    { data: "Coding DNA Sequences", models: ["Dn/Ds"] }
];

const allModels = ["AlphaFold", "CDROM", "Diversity / Divergence", "Dn/Ds", "Duplication Mechanism", "Expression Shift", "Gene Ontology", "OrthoFinder", "Pathway", "Post-Duplication Fates", "Public Datasets"];

const DataTypeFilter = () => {
    const [selectedTypes, setSelectedTypes] = useState([]);

    // Handle checkbox click
    const handleCheckboxChange = (type) => {
        setSelectedTypes((prev) =>
            prev.includes(type) ? prev.filter((item) => item !== type) : [...prev, type]
        );
    };

    // Get models based on selected data types
    const enabledModels = dataTypes
        .filter((dataType) => selectedTypes.includes(dataType.data))
        .flatMap((dataType) => dataType.models);

    return (
        <div className="container">


            <div className="selection-container">
                <h3>Select Data Type:</h3>
                <div className="data-type-list">
                    {dataTypes.map((type) => (
                        <div key={type.data} className="checkbox-wrapper">

                            <input
                                type="checkbox"
                                checked={selectedTypes.includes(type.data)}
                                onChange={() => handleCheckboxChange(type.data)}
                                className="checkbox-input"
                            />


                            <div className="checkbox-label">{type.data}</div>
                        </div>
                    ))}
                </div>
            </div>

            <div className="model-container">
                <h3>Available Models:</h3>
                <div className="model-list">
                    {allModels.map((model) => (
                        <div
                            key={model}
                            className={`model-item ${enabledModels.includes(model) ? "enabled" : "disabled"}`}
                        >
                            {model}
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
};

export default DataTypeFilter;
