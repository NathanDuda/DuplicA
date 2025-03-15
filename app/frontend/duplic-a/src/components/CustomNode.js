import React, { useState } from "react";
import { Handle, Position } from 'reactflow';
import '../styles/customNode.css';

const CustomNode = ({ data, isConnectable }) => {
    const [activeButton, setActiveButton] = useState(null);

    const handleModelClick = (model) => {

        if (activeButton === model || isModelSelected(model)) {
            setActiveButton(null);
        } else {
            setActiveButton(model);
        }


        if (data.onModelSelect) {
            data.onModelSelect(model);
        }
    };


    const isModelSelected = (model) => {
        return data.selectedModels && data.selectedModels.includes(model);
    };

    return (
        <div className="custom-node">
            <div className="title-section">
                <h3 className="node-title">{data.title}</h3>
            </div>

            <div className="button-container">
                {Array.isArray(data.buttonLabel)
                    ? data.buttonLabel.map((label, index) => (
                        <button
                            className={`node-button ${isModelSelected(label) || activeButton === label ? 'selected' : ''}`}
                            key={index}
                            onClick={() => handleModelClick(label)}
                        >
                            {label}
                        </button>
                    ))
                    : (
                        <button
                            className={`node-button ${isModelSelected(data.buttonLabel) || activeButton === data.buttonLabel ? 'selected' : ''}`}
                            onClick={() => handleModelClick(data.buttonLabel)}
                        >
                            {data.buttonLabel}
                        </button>
                    )
                }
            </div>
            <Handle type="source" position={Position.Right} className="right-handle" isConnectable={isConnectable}>
                <span className="handle-text">{">"}</span>
            </Handle>
            <Handle type="target" position={Position.Left} className="left-handle" isConnectable={isConnectable} >
                <span className="handle-text">{">"}</span>
            </Handle>
        </div>
    );
};

export default CustomNode;