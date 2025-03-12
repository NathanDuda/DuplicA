import React from "react";
import "../styles/runningworkflow.css";
import Layout from "../components/Layout";
const WorkflowPage = () => {
    return (
        <div>
            <Layout>
                <h2>Running Workflow ...</h2>

                <div className="container">

                    <div className="workflow-grid">

                        <div className="grid-item">
                            <h3>Date Created</h3>
                            <p>01/20/2025 12:00:00 AM</p>
                        </div>


                        <div className="grid-item">
                            <h3>Status</h3>
                            <p>COMPLETED</p>
                        </div>


                        <div className="grid-item grid-span">
                            <h3>Workflow Progress</h3>
                            <div className="workflow-progress">
                                <label>
                                    <input type="radio" checked readOnly /> Completed
                                </label>
                                <label>
                                    <input type="radio" disabled /> In Progress
                                </label>
                                <label>
                                    <input type="radio" disabled /> Queued
                                </label>
                            </div>
                        </div>


                        <div className="grid-item grid-span">
                            <h3>Workflow Diagram</h3>
                            <div className="workflow-diagram">
                                <div className="workflow-node">OrthoFinder</div>
                                <span> → </span>
                                <div>
                                    <div className="workflow-node">Gene Ontology</div>
                                    <div className="workflow-node">Post-duplication Fates</div>
                                </div>
                            </div>
                        </div>


                        <div className="grid-item grid-span">
                            <h3>Messages</h3>
                            <div className="messages">WORKFLOW FINISHED SUCCESSFULLY!</div>
                        </div>


                        <div className="grid-item grid-span">
                            <button>Download Results</button>
                            <button>Visualize Results</button>
                        </div>
                    </div>
                </div>
            </Layout>
        </div>
    );
};

export default WorkflowPage;
