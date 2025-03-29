import React, { useState, useEffect, useCallback } from "react";
import ReactFlow, { Controls, useNodesState, useEdgesState } from "reactflow";
import { navigate } from "gatsby";
import "reactflow/dist/style.css";
import CustomNode from "./CustomNode";
import parameterOptions from "../components/multispecies_model_options.json"
import axios from "axios";
import { ResizableBox } from 'react-resizable';
import { Background } from 'reactflow';


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
            selectedModels: [],
            hasLeft: false,
            hasRight: true
        },
    },
    {
        id: "2",
        type: "custom",
        position: { x: -450, y: 380 },
        data: {
            title: "Detect Duplications",
            buttonLabel: "OrthoFinder",
            onModelSelect: null,
            selectedModels: [],
            hasLeft: true,
            hasRight: true
        },
    },
    {
        id: "3",
        type: "custom",
        position: { x: 50, y: 150 },
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
            selectedModels: [],
            hasLeft: true,
            hasRight: false
        },
    },
    {
        id: "4",
        type: "custom",
        position: { x: 50, y: 550 },
        data: {
            title: "Mechanism Models",
            buttonLabel: "Duplication Mechanism",
            onModelSelect: null,
            selectedModels: [],
            hasLeft: true,
            hasRight: false
        },
    },
    {
        id: "5",
        type: "custom",
        position: { x: 50, y: 750 },
        data: {
            title: "Selection Models",
            buttonLabel: ["Dn/Ds", "EVE Diversity/Divergence"],
            onModelSelect: null,
            selectedModels: [],
            hasLeft: true,
            hasRight: false
        },
    },
];

const initialEdges = [
    { id: "e1-2", source: "1", target: "2", style: { stroke: "#8430fb", strokeWidth: 3 } },
    { id: "e2-3", source: "2", target: "3", style: { stroke: "#8430fb", strokeWidth: 3 } },
    { id: "e2-4", source: "2", target: "4", style: { stroke: "#8430fb", strokeWidth: 3 } },
    { id: "e2-5", source: "2", target: "5", style: { stroke: "#8430fb", strokeWidth: 3 } },
];

const Workflowcomponent = () => {
    const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
    const [edges, onEdgesChange] = useEdgesState(initialEdges);
    const [selectedModels, setSelectedModels] = useState([]);
    const [parameters, setParameters] = useState({});
    const [availableOptions, setAvailableOptions] = useState({});
    const [isLoading, setIsLoading] = useState(false);
    const [apiError, setApiError] = useState(null);
    const [sidebarWidth, setSidebarWidth] = useState(490);
    const [requiredParams, setRequiredParams] = useState({});
    const [additionalParams, setAdditionalParams] = useState({});
    const [showAdditionalParams, setShowAdditonalParmas] = useState(false);
    const [isReadyToRun, setIsReadyToRun] = useState(false);
    const [isRunningWorkflow, setIsRunningWorkflow] = useState(false);




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
            setRequiredParams({});
            setAdditionalParams({});
        }
    }, [selectedModels]);




    // Fetch parameters from API
    const fetchParametersFromAPI = async (updatedParams = parameters) => {

        setIsLoading(true);
        setApiError(null);

        try {
            const response = await axios.post(`${API_BASE_URL}/getInputs`, {
                selected_models: selectedModels,
                parameters: flattenParameters(updatedParams),

            });

            console.log("API Request Payload:", {
                selected_models: selectedModels,
                parameters: flattenParameters(parameters),
            });
            console.log("API Response:", response.data);

            const requiredList = response.data.required_parameter_list || [];
            const additionalList = response.data.additional_parameter_list || [];

            const allParameters = [...requiredList, ...additionalList];

            const updatedParameters = allParameters.reduce((acc, param) => {
                if (parameterOptions[param]) {
                    acc[param] = {
                        ...parameterOptions[param],
                        value: parameters[param] ?? parameterOptions[param].default ?? "",
                    };
                }
                return acc;
            }, {});

            setParameters(updatedParameters);
            setAvailableOptions(updatedParameters);
            setRequiredParams(requiredList.reduce((acc, param) => {
                if (parameterOptions[param]) {
                    acc[param] = updatedParameters[param];
                }
                return acc;
            }, {}));
            setAdditionalParams(additionalList.reduce((acc, param) => {
                if (parameterOptions[param]) {
                    acc[param] = updatedParameters[param];
                }
                return acc;
            }, {}));
        } catch (error) {
            console.error("Error fetching parameters:", error);
            setApiError("Failed to fetch parameters from API.");
        } finally {
            setIsLoading(false);
        }
    };

    const handleInputChange = (key, value) => {
        // First update the state
        const updatedParams = {
            ...parameters,
            [key]: {
                ...parameters[key],
                value: value,
            },
        };
        setParameters(updatedParams);
        setAvailableOptions((prev) => ({
            ...prev,
            [key]: {
                ...prev[key],
                value: value,
            },
        }));

        fetchParametersFromAPI(updatedParams);
    };


    const flattenParameters = (params) => {
        return Object.keys(params).reduce((acc, key) => {
            acc[key] = typeof params[key] === 'object' ? params[key]?.value : params[key];
            return acc;
        }, {});
    };



    const handleFolderChange = (key, event) => {
        const file = event.target.files;
        if (!file.length) return;

        const firstFile = file[0];
        const fullPath = firstFile.webkitRelativePath;
        const folderName = fullPath.split("/")[0];

        console.log(`Folder Name:`, folderName);

        const updatedParams = {
            ...parameters,
            [key]: {
                ...parameters[key],
                value: folderName,
            },
        };


        setParameters(updatedParams);
        setAvailableOptions((prev) => ({
            ...prev,
            [key]: {
                ...prev[key],
                value: folderName,
            },
        }));
        fetchParametersFromAPI(updatedParams);
    };
    const handleFileChange = (key, event) => {
        const files = event.target.files;

        if (!files.length) return;
        const file = files[0];
        const fileName = file.name;




        console.log("uploaded file", fileName);

        const updatedParams = {
            ...parameters,
            [key]: {
                ...parameters[key],
                value: fileName,
            },
        };
        setParameters(updatedParams);
        setAvailableOptions((prev) => ({
            ...prev,
            [key]: {
                ...prev[key],
                value: fileName,
            },
        }));
        fetchParametersFromAPI(updatedParams)

    };
    // Run workflow function
    const runWorkflow = async () => {
        setIsRunningWorkflow(true);
        setIsLoading(true);
        setApiError(null);

        const submissionTime = new Date().toLocaleString();

        try {
            // Submit workflow to API
            const response = await axios.post(`${API_BASE_URL}/runWorkflow`, {
                selected_models: selectedModels,
                parameters: flattenParameters(parameters),

            });

            console.log("Workflow response:", response.data);

            // Navigate to results page
            navigate("/Runningworkflow", {
                state: {

                    selectedModels: selectedModels,
                    submissionTime: submissionTime,
                },
            });

        } catch (error) {
            console.error("Error running workflow:", error);
        } finally {
            setIsLoading(false);
            setIsRunningWorkflow(false);
        }

    };
    useEffect(() => {
        const allRequiredFilled = Object.keys(requiredParams).every((key) => {
            const paramValue = parameters[key]?.value;
            return paramValue !== undefined && paramValue !== null && paramValue !== "";
        });

        setIsReadyToRun(allRequiredFilled && selectedModels.length > 0);
    }, [parameters, requiredParams, selectedModels]);

    function parmaInput(key, param) {
        if (param.type === "text") {
            return (
                <div key={key} className="text-param" >
                    <p>{param.label}:</p>
                    <input
                        type="text"
                        value={parameters[key]?.value || ""}
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
                        value={parameters[key]?.value ?? ""}
                        onChange={(e) => {
                            const newValue = e.target.value;
                            console.log("Select change:", key, newValue);
                            handleInputChange(key, newValue);
                        }}
                    >
                        <option value="">Select an option</option>
                        {param.choices?.map((choice) => (
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
                <div key={key} className="boolean-param" style={{ display: "flex", alignItems: "center", gap: "8px" }}>
                    <p>{param.label}:</p>
                    <input
                        style={{
                            accentColor: "#8430fb"

                        }} type="checkbox"
                        checked={parameters[key] || false}
                        onChange={(e) => handleInputChange(key, e.target.checked)}
                    />
                </div>
            );
        }

        if (param.type === "directory") {
            return (
                <div key={key} className="folder-param" >
                    <p>{param.label}:</p>
                    <label htmlFor={`folderUpload_${key}`}>
                        Upload Folder
                    </label>
                    <input
                        style={{ display: "none" }}
                        id={`folderUpload_${key}`}

                        type="file"
                        webkitdirectory=""
                        directory=""
                        multiple

                        Copy
                        onChange={(e) => handleFolderChange("input_key", e)} />
                    {parameters[key] && (
                        <p style={{ fontSize: "0.8em", marginTop: "5px" }}>
                            Selected: {parameters[key].name}
                        </p>
                    )}
                </div>
            );
        }

        if (param.type === "file") {
            return (
                <div key={key} className="file-param">
                    <p>{param.label}</p>
                    <label htmlFor="fileUpload"> Upload File</label>
                    <input
                        style={{ display: "none" }}
                        id="fileUpload"
                        type="file"
                        onChange={(e) => handleFileChange("input_key", e)}
                    />
                    {parameters[key] && (<p>selected:{parameters[key].name}</p>
                    )}
                </div>
            );
        }

        return null;

    }


    return (




        <div style={{ display: 'flex', height: '100vh' }}>
            <div style={{
                minwidth: "100px", flex: "1",
                transition: 'width 0.2s ease-in-out'
            }}>
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    nodeTypes={nodeTypes}
                    defaultViewport={{ x: 1000, y: -60, zoom: 1 }}
                    fitView={false}
                >
                    <Background
                        variant="lines"
                        gap={20}
                        size={.5}
                    />
                    <Controls />

                </ReactFlow>
            </div>


            {Object.keys(availableOptions).length > 0 && (
                <ResizableBox width={sidebarWidth} height={Infinity} axis="x" minConstraints={[150, Infinity]} maxConstraints={[600, Infinity]} resizeHandles={['w']} onResizeStop={(e, data) => setSidebarWidth(data.size.width)}>
                    <div className="param-section" style={{ height: "100%" }} >
                        <div className="param-scroll-container">

                            <div className="required-param-section">
                                <h3>Required Inputs</h3>
                                {Object.keys(requiredParams).map((key) => {
                                    const param = requiredParams[key];
                                    return parmaInput(key, param);
                                })}
                            </div>

                            <div className="hide-additional-param-section">
                                <button style={{ background: "none", border: "none", textDecoration: "underline", textDecorationColor: "#8430fb", fontSize: "18px", fontWeight: "bold", marginTop: "10px", textDecorationThickness: "2.5px", marginBottom: "10px" }} onClick={() => setShowAdditonalParmas(!showAdditionalParams)} c> {showAdditionalParams ? "Hide Additional Paramters" : "Additional Paramters > "} </button>
                                {showAdditionalParams && (
                                    <div className="additional-param-section">
                                        <h3>Additional Options </h3>
                                        {Object.keys(additionalParams).map((key) => {
                                            const param = additionalParams[key];
                                            return parmaInput(key, param);
                                        })}
                                    </div>

                                )}

                            </div>

                            <div style={{ marginTop: "20px", marginLeft: "150px" }}>

                                <button className="runworkflow-btn"
                                    onClick={runWorkflow}
                                    type="button"
                                    disabled={!isReadyToRun || isLoading || isRunningWorkflow}
                                >

                                    {isLoading && isRunningWorkflow ? "Submitting..." : "Run Workflow"}

                                </button>
                                {apiError && (
                                    <div  >
                                        {apiError}
                                    </div>
                                )}
                            </div>
                        </div>

                    </div>

                </ResizableBox>
            )
            }


        </div >
    );


};

export default Workflowcomponent;
