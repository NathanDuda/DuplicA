import React, { useState, useEffect } from "react";
import ReactFlow, { Controls, useNodesState, useEdgesState } from "reactflow";
import { navigate } from "gatsby";

import { Link } from "gatsby";
import axios from "axios";
import "reactflow/dist/style.css";
import CustomNode from "./CustomNode";
import MultioptionNode from "./MultioptionNode";
import parameterOptions from "./multispecies_model_options.json";


const nodeTypes = { custom: CustomNode, multiCustom: MultioptionNode };

const initialNodes = [
    {
        id: "1",
        type: "custom",
        position: { x: 100, y: 200 },
        data: { title: "Get Public Data", buttonLabel: "Public Datasets" },
    },
    {
        id: "2",
        type: "custom",
        position: { x: 350, y: 200 },
        data: { title: "Detect Duplications", buttonLabel: "OrthoFinder" },
    },
    {
        id: "3",
        type: "multiCustom",
        position: { x: 600, y: 150 },
        data: {
            title: "Functional Models",
            buttonLabel: "AlphaFold",
            buttonLabel1: "CDROM",
            buttonLabel2: "EVE Expression Shift",
            buttonLabel3: "Gene Ontology",
            buttonLabel4: "Pathway Analysis",
            buttonLabel5: "Post-duplication Fates",
        },
    },
    {
        id: "4",
        type: "custom",
        position: { x: 600, y: 550 },
        data: { title: "Mechanism Models", buttonLabel: "Duplication Mechanism" },
    },
    {
        id: "5",
        type: "multiCustom",
        position: { x: 600, y: 750 },
        data: {
            title: "Selection Models",
            buttonLabel: "Dn/Ds",
            buttonLabel1: "EVE Diversity/Divergence",
        },
    },
];

const initialEdges = [
    { id: "e1-2", source: "1", target: "2" },
    { id: "e2-3", source: "2", target: "3" },
    { id: "e2-4", source: "2", target: "4" },
    { id: "e2-5", source: "2", target: "5" },
];




const Workflowcomponent = () => {
    const [nodes, setNodes, onNodesChange] = useNodesState(initialNodes);
    const [edges, setEdges, onEdgesChange] = useEdgesState(initialEdges);
    const [selectedModels, setSelectedModels] = useState([]);
    const [parameters, setParameters] = useState({});
    const [availableOptions, setAvailableOptions] = useState({});


    const handleNodeClick = (event, node) => {
        const model = node.data.buttonLabel;

        if (!selectedModels.includes(model)) {
            setSelectedModels((prevModels) => {
                const updatedModels = [...prevModels, model];

                console.log("Selected Models (Updated):", updatedModels);
                return updatedModels;
            });


            setAvailableOptions(parameterOptions || {});
            setParameters(initializeParams(parameterOptions));
        }
    };


    useEffect(() => {
        console.log("Updated Selected Models:", selectedModels);
    }, [selectedModels]);



    const initializeParams = (options) => {
        const params = {};
        for (const key in options) {
            const paramType = options[key].type;
            if (paramType === "select" || paramType === "text") {
                params[key] = options[key].default || "";
            } else if (paramType === "boolean") {
                params[key] = options[key].default || false;
            } else if (paramType === "file") {
                params[key] = null;
            } else if (paramType === "number") {
                params[key] = options[key].default || 0;
            }
        }
        return params;
    };


    const handleInputChange = (key, value) => {
        setParameters((prev) => ({
            ...prev,
            [key]: value,
        }));
    };


    const handleFileChange = (key, event) => {
        setParameters((prev) => ({
            ...prev,
            [key]: event.target.files[0],
        }));
    };



    // const runWorkflow = () => {
    //     const formData = new FormData();
    //     formData.append("selected_models", selectedModels);

    //     for (const key in parameters) {
    //         if (parameters[key] instanceof File) {
    //             formData.append(key, parameters[key]);
    //         } else {
    //             formData.append(key, parameters[key]);
    //         }
    //     }

    //     axios.post("/run_workflow", formData)
    //         .then(() => {
    //             navigate("/Runningworkflow");
    //         })
    //         .catch((error) => console.error("Error running workflow:", error));
    // };









    return (


        <div >
            <div style={{
                width: "100%", height: "700px", background: "#F3F4F8"
            }}>
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    nodeTypes={nodeTypes}
                    onNodeClick={handleNodeClick}
                    fitView
                >
                    <Controls />
                </ReactFlow>
            </div>

            <div>
                <h3>Additional Parameters</h3>


                {Object.keys(availableOptions).map((key) => {
                    const param = availableOptions[key];

                    if (param.type === "text") {
                        return (
                            <div key={key}>
                                <p>{param.label}:</p>
                                <input
                                    type="text"
                                    value={parameters[key]}
                                    onChange={(e) => handleInputChange(key, e.target.value)}
                                />
                            </div>
                        );
                    }

                    if (param.type === "select") {
                        return (
                            <div key={key}>
                                <p>{param.label}:</p>
                                <select
                                    value={parameters[key]}
                                    onChange={(e) => handleInputChange(key, e.target.value)}
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
                            <div key={key}>
                                <p>{param.label}:</p>
                                <input
                                    type="number"
                                    value={parameters[key]}
                                    min={param.min || 0}
                                    step={param.step || 1}
                                    onChange={(e) => handleInputChange(key, parseFloat(e.target.value))}
                                />
                            </div>
                        );
                    }

                    if (param.type === "boolean") {
                        return (
                            <div key={key}>
                                <p>{param.label}:</p>
                                <input
                                    type="checkbox"
                                    checked={parameters[key]}
                                    onChange={(e) => handleInputChange(key, e.target.checked)}
                                />
                            </div>
                        );
                    }

                    if (param.type === "file") {
                        return (
                            <div key={key}>
                                <p>{param.label}:</p>
                                <input type="file" onChange={(e) => handleFileChange(key, e)} />
                            </div>
                        );
                    }

                    return null;
                })}
            </div>
            <button
                onClick={() => navigate("/Runningworkflow")}
                style={{
                    marginTop: "20px",
                    padding: "10px",
                    background: "#007bff",
                    color: "#fff",
                    border: "none",
                    cursor: "pointer",
                }}
            >
                Run Workflow
            </button>


        </div>

    );
};

export default Workflowcomponent;
