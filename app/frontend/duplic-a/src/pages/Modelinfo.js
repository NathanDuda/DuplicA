import React from "react";
import { Router } from "@reach/router";
import ModelInformation from "../components/ModelInformation";
import Layout from "../components/Layout";
import "../styles/modelinfo.css"
import { BiDna } from "react-icons/bi";


const Modelinfo = () => {
    return (
        <Layout>
            <ModelInformation></ModelInformation>

        </Layout>

    );
};

export default Modelinfo;
