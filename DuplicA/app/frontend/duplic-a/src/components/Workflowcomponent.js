import React, { useState, useEffect } from "react";
import ReactFlow, { Controls, useNodesState, useEdgesState } from "reactflow";
import { Link } from "gatsby";
import axios from "axios";
import "reactflow/dist/style.css";


import CustomNode from "./Customnode";
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







    return (
        <div style={{ width: "100%", height: "700px", background: "#f9f9f9", padding: "20px" }}>
            {/* React Flow Canvas */}
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

    );
};

export default Workflowcomponent;
