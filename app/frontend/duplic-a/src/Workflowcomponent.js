import React, { useState, useEffect } from "react";
import ReactFlow, { Controls, useNodesState, useEdgesState } from "reactflow";
import axios from "axios";
import "reactflow/dist/style.css";
import CustomNode from "./Customnode";
import MultioptionNode from "./MultioptionNode";
import parameterOptions from "./multispecies_model_options.json";

const nodeTypes = { custom: CustomNode, multiCustom: MultioptionNode };

const initialNodes = [
    { id: "1", type: "custom", position: { x: 100, y: 200 }, data: { title: "Get Public Data", buttonLabel: "Public Datasets" } },
    { id: "2", type: "custom", position: { x: 350, y: 200 }, data: { title: "Detect Duplications", buttonLabel: "OrthoFinder" } },
    { id: "3", type: "multiCustom", position: { x: 600, y: 150 }, data: { title: "Functional Models", buttonLabel: "AlphaFold", buttonLabel1: "CDROM", buttonLabel2: "EVE Expression Shift", buttonLabel3: "Gene Ontology", buttonLabel4: "Pathway Analysis", buttonLabel5: "Post-duplication Fates" } },
    { id: "4", type: "custom", position: { x: 600, y: 550 }, data: { title: "Mechanism Models", buttonLabel: "Duplication Mechanism" } },
    { id: "5", type: "multiCustom", position: { x: 600, y: 750 }, data: { title: "Selection Models", buttonLabel: "Dn/Ds", buttonLabel1: "EVE Diversity/Divergence" } },
];

const initialEdges = [
    { id: "e1-2", source: "1", target: "2" },
    { id: "e2-3", source: "2", target: "3" },
    { id: "e2-4", source: "2", target: "4" },
    { id: "e2-5", source: "2", target: "5" },
];

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

const Workflowcomponent = () => {
    const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
    const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);

    const [selectedModels, setSelectedModels] = useState([]);
    const [requiredParameters, setRequiredParameters] = useState({});
    const [additionalParameters, setAdditionalParameters] = useState({});
    const [userInputs, setUserInputs] = useState({});
    const [isPopupVisible, setIsPopupVisible] = useState(false);
    const [isMinimized, setIsMinimized] = useState(false);
    const [showAdditionalParams, setShowAdditionalParams] = useState(false);

    // Handle node click - select/unselect model
    const onNodeClick = (event, node) => {
        const modelNames = Object.keys(node.data)
            .filter(key => key.includes("buttonLabel") && node.data[key])
            .map(key => node.data[key]);

        modelNames.forEach(modelName => handleToggle(modelName, !selectedModels.includes(modelMapping[modelName])));
    };

    // Handle toggle button click - select/unselect model
    const handleToggle = (modelName, isActive) => {
        const apiModelName = modelMapping[modelName];

        setSelectedModels(prevModels =>
            isActive ? [...prevModels, apiModelName] : prevModels.filter(m => m !== apiModelName)
        );
    };

    // Fetch parameters when selected models change
    useEffect(() => {
        if (selectedModels.length === 0) {
            setRequiredParameters({});
            setAdditionalParameters({});
            setIsPopupVisible(false);
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
                setIsPopupVisible(true);
            } catch (error) {
                console.error("Error fetching input fields:", error);
            }
        };

        fetchModelInputs();
    }, [selectedModels]);

    return (
        <div style={{ width: "100%", height: "700px", background: "#f9f9f9", padding: "20px" }}>
            <ReactFlow
                nodes={nodes}
                edges={edges}
                onNodesChange={onNodesChange}
                onEdgesChange={onEdgesChange}
                nodeTypes={nodeTypes}
                fitView
                onNodeClick={onNodeClick}
            >
                <Controls />
            </ReactFlow>

            <div className="models-wrapper">
                <div className="model-box">
                    <h3>Functional Models</h3>
                    <div className="button-group">
                        {Object.keys(modelMapping).map((modelName, idx) => (
                            <button
                                key={idx}
                                className={`toggle-button ${selectedModels.includes(modelMapping[modelName]) ? "active" : ""}`}
                                onClick={() => handleToggle(modelName, !selectedModels.includes(modelMapping[modelName]))}
                            >
                                {modelName}
                            </button>
                        ))}
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
                        {Object.entries(requiredParameters).map(([param, details]) => (
                            <div key={param}>
                                <label>{details.label || param}:</label>
                                <input
                                    type="text"
                                    value={userInputs[param] ?? details.value}
                                    onChange={(e) => setUserInputs({ ...userInputs, [param]: e.target.value })}
                                />
                            </div>
                        ))}
                    </div>

                    <button onClick={() => setShowAdditionalParams(!showAdditionalParams)}>Additional Options</button>
                    {showAdditionalParams && (
                        <div className="form-section additional-parameters">
                            <h3>Additional Parameters</h3>
                            {Object.entries(additionalParameters).map(([param, details]) => (
                                <div key={param}>
                                    <label>{details.label || param}:</label>
                                    <input
                                        type="text"
                                        value={userInputs[param] ?? details.value}
                                        onChange={(e) => setUserInputs({ ...userInputs, [param]: e.target.value })}
                                    />
                                </div>
                            ))}
                        </div>
                    )}
                </div>
            )}
        </div>
    );
};

export default Workflowcomponent;
