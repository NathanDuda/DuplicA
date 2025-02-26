import React, { useState } from 'react';
import ReactFlow, { Controls, Background } from 'reactflow';
import 'reactflow/dist/style.css';
import Workflowcomponent from '../components/Workflowcomponent';
import Layout from '../components/Layout';
import CollapsibleSection from '../components/CollapsibleSection';

const WorkflowPage = () => {
    const [isCollapsed, setIsCollapsed] = useState(false);

    return (
        <Layout>

            <div style={{ position: 'relative', width: '100%', height: '500px', background: '#f9f9f9', padding: '20px' }}>

                {/* 
                <div>
                    <CollapsibleSection isCollapsed={isCollapsed} setIsCollapsed={setIsCollapsed} />

                </div> */}


                <Workflowcomponent />
            </div>
        </Layout>
    );
};

export default WorkflowPage;
