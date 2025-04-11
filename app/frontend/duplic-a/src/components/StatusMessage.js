import React, { useState, useEffect } from "react";
import axios from "axios";

const StatusMessage = () => {

    const [statusMessage, setStatusMessage] = useState([]);


    // function to get status messages from API
    const fetchMessages = async () => {
        try {
            const response = await axios.get("http://localhost:8002/status.txt", {
                responseType: "text",
            });
            const lines = response.data.split("\n").filter((line) => line.trim() != "");

            setStatusMessage(lines);


        } catch (error) {
            console.log("Error fetching messages ", error);
        }
    };

    //poll the API for messages every 2 seconds
    useEffect(() => {
        const interval = setInterval(fetchMessages, 2000);
        return () => clearInterval(interval);

    }, []);

    return (
        <div style={{
            padding: "10px",
            border: "1px solid #ccc",
            borderRadius: "5px",
            backgroundColor: "#f9f9f9",
            maxHeight: "300px",
            overflow: 'hidden'

        }}>


            {statusMessage.map((message, index) => (
                <div key={index} style={{ marginBottom: "5px" }}>
                    {message}

                </div>
            ))}







        </div>
    );

}; export default StatusMessage;