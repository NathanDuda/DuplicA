import React from "react";
import { Handle, Position } from "reactflow";
import "../styles/customNode.css";

const MultioptionNode = ({ data, onModelSelect }) => {

    const buttonKeys = Object.keys(data).filter(
        (key) => key.startsWith("buttonLabel") && data[key]
    );

    return (
        <div className="custom-node" style={{ cursor: "grab" }}>
            <h3 className="node-title" data-drag-handle="true">
                {data.title}
            </h3>
            {buttonKeys.map((key) => (
                <button
                    key={key}
                    className="node-button"
                    onClick={(e) => {
                        e.stopPropagation();
                        if (onModelSelect) {
                            onModelSelect(data[key]);
                        }
                    }}
                >
                    {data[key]}
                </button>
            ))}
            <Handle type="source" position={Position.Right} className="node-handle" />
            <Handle type="target" position={Position.Left} className="node-handle" />
        </div>
    );
};

export default MultioptionNode;
