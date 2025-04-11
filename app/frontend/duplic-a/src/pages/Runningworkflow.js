import React, { useState, useEffect } from "react";
import "../styles/runningworkflow.css";
import Layout from "../components/Layout";
import { useLocation } from "@reach/router";
import StatusMessage from "../components/StatusMessage";
import { navigate } from "gatsby";
import axios from "axios";


const Runningworkflow = () => {

    const location = useLocation();
    const { workflowId, submissionTime, selectedModels = [] } = location.state || {};
    const [workflowCompleted, setWorkflowCompleted] = useState(true);



    //function to check if the workflow is completed 
    const checkWorkflowCompletion = async () => {

        try {

            const response = await axios.get("http://localhost:8002/status.txt", {
                responseType: "text",

            });
            const lines = response.data.split("\n").filter((line) => line.trim() !== "");
            const lastLine = lines[lines.length - 1] || "";
            setWorkflowCompleted(lastLine.includes("Workflow Completed!"))



        } catch (error) {
            console.log("failed to get status:", error);
        }


    };

    useEffect(() => {
        const interval = setInterval(checkWorkflowCompletion, 2000);
        return () => clearInterval(interval);
    }, []);


    //Handle the download button
    const downloadResults = async () => {

        try {
            const response = await axios.get("/api/results/workflow_results.txt", {
                responseType: "blob",
            });

            const url = window.URL.createObjectURL(new Blob([response.data]));
            const link = document.createElement("a");
            link.href = url;
            link.setAttribute("download", "workflow_results.txt");
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        } catch (error) {
            console.error("Error downloading results:", error);
        }
    };

    const anazyleResults = async () => {

        navigate("/Visualization");
    };

    return (
        <div>
            <Layout>

                <div className="container">

                    <div className="workflow-grid">

                        <div className="grid-item">
                            <h3>Date Created</h3>
                            <p>{submissionTime}</p>
                            <h4>Workflow ID</h4>
                            <p>{workflowId}</p>


                        </div>


                        <div className="grid-item">
                            <h3>Workflow Diagram</h3>
                            <div className="workflow-diagram">
                                {selectedModels.map((model, idx) => (
                                    <React.Fragment key={idx}>
                                        <div className="workflow-node">{model}</div>
                                        {idx < selectedModels.length - 1 && <span> → </span>}
                                    </React.Fragment>
                                ))}
                            </div>
                        </div>



                        <div className="grid-item grid-span">
                            <h3>Status Messages</h3>

                            <StatusMessage />

                        </div>




                    </div>
                    <div className="get-results">
                        <button
                            className="options"
                            onClick={downloadResults}
                            disabled={!workflowCompleted}
                            style={{
                                backgroundColor: workflowCompleted ? "#8430fb" : "#ccc",
                                cursor: workflowCompleted ? "pointer" : "not-allowed",
                            }}
                        >
                            Download Results
                        </button>

                        <button className="options" onClick={anazyleResults}>
                            Analyze Results
                        </button>
                    </div>
                </div>
            </Layout>
        </div>
    );
};

export default Runningworkflow;
