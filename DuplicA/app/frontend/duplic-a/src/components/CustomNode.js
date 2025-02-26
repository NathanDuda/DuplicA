import React from 'react';
import { Handle, Position } from 'reactflow';
import '../styles/customNode.css';

const CustomNode = ({ data }) => {
    return (
        <div className="custom-node" style={{ cursor: 'grab' }}>
            <h3 className="node-title" data-drag-handle>{data.title}</h3>
            <button className="node-button">{data.buttonLabel}</button>


            <Handle type="source" position={Position.Right} className="node-handle" />
            <Handle type="target" position={Position.Left} className="node-handle" />
        </div>
    );
};

export default CustomNode;
