import React, { useState, useEffect, useCallback } from "react";
import ReactFlow, { Controls, useNodesState, useEdgesState } from "reactflow";
import { navigate } from "gatsby";
import "reactflow/dist/style.css";
import CustomNode from "./CustomNode";
import parameterOptions from "../components/multispecies_model_options.json"
import axios from "axios";
import { ResizableBox } from 'react-resizable';


const API_BASE_URL = "http://127.0.0.1:8001";

const nodeTypes = { custom: CustomNode };

const initialNodes = [
    {
        id: "1",
        type: "custom",
        position: { x: -900, y: 380 },
        data: {
            title: "Get Public Data",
            buttonLabel: "Public Datasets",
            onModelSelect: null,
            selectedModels: []
        },
    },
    {
        id: "2",
        type: "custom",
        position: { x: -300, y: 380 },
        data: {
            title: "Detect Duplications",
            buttonLabel: "OrthoFinder",
            onModelSelect: null,
            selectedModels: []
        },
    },
    {
        id: "3",
        type: "custom",
        position: { x: 500, y: 150 },
        data: {
            title: "Functional Models",
            buttonLabel: [
                "AlphaFold",
                "CDROM",
                "EVE Expression Shift",
                "Gene Ontology",
                "Pathway Analysis",
                "Post-duplication Fates"
            ],
            onModelSelect: null,
            selectedModels: []
        },
    },
    {
        id: "4",
        type: "custom",
        position: { x: 500, y: 550 },
        data: {
            title: "Mechanism Models",
            buttonLabel: "Duplication Mechanism",
            onModelSelect: null,
            selectedModels: []
        },
    },
    {
        id: "5",
        type: "custom",
        position: { x: 500, y: 750 },
        data: {
            title: "Selection Models",
            buttonLabel: ["Dn/Ds", "EVE Diversity/Divergence"],
            onModelSelect: null,
            selectedModels: []
        },
    },
];

const initialEdges = [
    { id: "e1-2", source: "1", target: "2", style: { stroke: "#D2D3F8", strokeWidth: 3 } },
    { id: "e2-3", source: "2", target: "3", style: { stroke: "#D2D3F8", strokeWidth: 3 } },
    { id: "e2-4", source: "2", target: "4", style: { stroke: "#D2D3F8", strokeWidth: 3 } },
    { id: "e2-5", source: "2", target: "5", style: { stroke: "#D2D3F8", strokeWidth: 3 } },
];

const Workflowcomponent = () => {
    const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
    const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);
    const [selectedModels, setSelectedModels] = useState([]);
    const [parameters, setParameters] = useState({});
    const [availableOptions, setAvailableOptions] = useState({});
    const [isLoading, setIsLoading] = useState(false);
    const [apiError, setApiError] = useState(null);

    // Handler for model selection that toggles models
    const handleModelSelect = useCallback((model) => {
        setSelectedModels((prevModels) => {
            // If model is already selected, remove it
            if (prevModels.includes(model)) {
                const updatedModels = prevModels.filter(m => m !== model);
                console.log("Selected Models (Updated - Removed):", updatedModels);
                return updatedModels;
            }
            // If model is not selected, add it
            else {
                const updatedModels = [...prevModels, model];
                console.log("Selected Models (Updated - Added):", updatedModels);
                return updatedModels;
            }
        });
    }, []);


    // Update nodes with the model selection handler and selected models
    useEffect(() => {
        setNodes((nds) =>
            nds.map((node) => ({
                ...node,
                data: {
                    ...node.data,
                    onModelSelect: handleModelSelect,
                    selectedModels: selectedModels,
                },
            }))
        );
    }, [setNodes, handleModelSelect, selectedModels]);


    // Fetch parameters from API when selected models change
    useEffect(() => {
        // Only fetch if there are selected models
        if (selectedModels.length > 0) {
            fetchParametersFromAPI();
        } else {
            // Clear parameters if no models selected
            setAvailableOptions({});
            setParameters({});
        }
    }, [selectedModels]);


    // Fetch parameters from API
    const fetchParametersFromAPI = async () => {
        setIsLoading(true);
        setApiError(null);

        try {
            const response = await axios.post(`${API_BASE_URL}/getInputs`, {
                selected_models: selectedModels,
                parameters: parameters,
            });

            console.log("API Response:", response.data);

            const requiredList = response.data.required_parameter_list || [];
            const additionalList = response.data.additional_parameter_list || [];

            const matchedRequired = requiredList.reduce((acc, param) => {
                if (parameterOptions[param]) {
                    acc[param] = {
                        ...parameterOptions[param],
                        value: parameters[param] ?? parameterOptions[param].default ?? "",
                    };
                }
                return acc;
            }, {});

            const matchedAdditional = additionalList.reduce((acc, param) => {
                if (parameterOptions[param]) {
                    acc[param] = {
                        ...parameterOptions[param],
                        value: parameters[param] ?? parameterOptions[param].default ?? "",
                    };
                }
                return acc;
            }, {});

            setParameters({ ...matchedRequired, ...matchedAdditional });
            setAvailableOptions({ ...matchedRequired, ...matchedAdditional });
        } catch (error) {
            console.error("Error fetching parameters:", error);
            setApiError("Failed to fetch parameters from API.");
        } finally {
            setIsLoading(false);
        }
    };



    const handleInputChange = (key, value) => {
        setParameters((prev) => ({
            ...prev,
            [key]: value,
        }));
        setAvailableOptions((prev) => ({
            ...prev,
            [key]: {
                ...prev[key],
                value: value,
            },
        }));
    };

    const handleFileChange = (key, event) => {
        const file = event.target.files[0];

        const reader = new FileReader();
        reader.onload = (e) => {
            const fileContent = e.target.result;

            setParameters((prev) => ({
                ...prev,
                [key]: {
                    name: file.name,
                    content: fileContent,
                    type: file.type,
                },
            }));
            setAvailableOptions((prev) => ({
                ...prev,
                [key]: {
                    ...prev[key],
                    value: {
                        name: file.name,
                        content: fileContent,
                        type: file.type,
                    },
                },
            }));
        };

        reader.readAsDataURL(file);
    };

    // Run workflow function
    const runWorkflow = async () => {
        setIsLoading(true);
        setApiError(null);

        try {
            // Submit workflow to API
            const response = await axios.post(`${API_BASE_URL}/runWorkflow`, {
                selected_models: selectedModels,
                parameters: parameters
            });

            console.log("Workflow response:", response.data);

            // Navigate to results page
            navigate("/Runningworkflow", {
                state: {
                    workflowId: response.data.workflowId,
                    selectedModels: selectedModels
                }
            });

        } catch (error) {
            console.error("Error running workflow:", error);
        } finally {
            setIsLoading(false);
        }
    };

    const [sidebarWidth, setSidebarWidth] = useState(600);


    return (


        <div style={{ display: 'flex', height: '100vh' }}>
            <div style={{ minwidth: "100px", flex: "1", background: "#F3F4F8" }}>
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    nodeTypes={nodeTypes}
                    fitView
                >
                    <Controls />
                </ReactFlow>
            </div>


            {isLoading && (
                <div style={{ textAlign: "center", padding: "20px" }}>
                    <p>Loading...</p>
                </div>
            )}

            {apiError && (
                <div style={{ color: "red", padding: "10px", margin: "10px 0", background: "#ffeeee", borderRadius: "5px" }}>
                    {apiError}
                </div>
            )}

            {Object.keys(availableOptions).length > 0 && (
                <ResizableBox width={sidebarWidth} height={Infinity} axis="x" minConstraints={[150, Infinity]} maxConstraints={[600, Infinity]} resizeHandles={['w']} onResizeStop={(e, data) => setSidebarWidth(data.size.width)}>
                    <div className="param-section" style={{ height: "100%" }} >
                        <h3 >Additional Parameters</h3>
                        <div className="param-types" >
                            {Object.keys(availableOptions).map((key) => {
                                const param = availableOptions[key];

                                if (param.type === "text") {
                                    return (
                                        <div key={key} className="text-param" >
                                            <p>{param.label}:</p>
                                            <input
                                                type="text"
                                                value={parameters[key] || ""}
                                                onChange={(e) => handleInputChange(key, e.target.value)}

                                            />
                                        </div>
                                    );
                                }

                                if (param.type === "select") {
                                    return (
                                        <div key={key} className="select-param">
                                            <p>{param.label}:</p>
                                            <select
                                                value={parameters[key] || ""}
                                                onChange={(e) => handleInputChange(key, e.target.value)}

                                                multiple={param.multiple}
                                            >
                                                {param.choices.map((choice) => (
                                                    <option key={choice} value={choice}>{choice}</option>
                                                ))}
                                            </select>
                                        </div>
                                    );
                                }

                                if (param.type === "number") {
                                    return (
                                        <div key={key} className="number-param">
                                            <p>{param.label}:</p>
                                            <input
                                                type="number"
                                                value={parameters[key] || 0}
                                                min={param.min || 0}
                                                max={param.max || undefined}
                                                step={param.step || 1}
                                                onChange={(e) => handleInputChange(key, parseFloat(e.target.value))}

                                            />
                                        </div>
                                    );
                                }

                                if (param.type === "boolean") {
                                    return (
                                        <div key={key} className="boolean-param">
                                            <p>{param.label}:</p>
                                            <input
                                                type="checkbox"
                                                checked={parameters[key] || false}
                                                onChange={(e) => handleInputChange(key, e.target.checked)}
                                            />
                                        </div>
                                    );
                                }

                                if (param.type === "file" || param.type === "directory") {
                                    return (
                                        <div key={key} className="file-param" >
                                            <p>{param.label}:</p>
                                            <input
                                                type="file"
                                                onChange={(e) => handleFileChange(key, e)}
                                                style={{ width: "100%" }}
                                                multiple={param.multiple}
                                            />
                                            {parameters[key] && (
                                                <p style={{ fontSize: "0.8em", marginTop: "5px" }}>
                                                    Selected: {parameters[key].name}
                                                </p>
                                            )}
                                        </div>
                                    );
                                }

                                return null;
                            })}
                        </div>
                        <div style={{ marginTop: "20px", display: "flex", gap: "10px" }}>
                            <button
                                onClick={runWorkflow}
                                style={{
                                    padding: "10px",
                                    background: "#007bff",
                                    color: "#fff",
                                    border: "none",
                                    borderRadius: "5px",
                                    cursor: "pointer",
                                }}
                                disabled={selectedModels.length === 0 || isLoading}
                            >
                                Run Workflow
                            </button>


                        </div>
                    </div>
                </ResizableBox>
            )
            }






        </div >
    );
};

export default Workflowcomponent;