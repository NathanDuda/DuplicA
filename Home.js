import React, { useState } from "react";

import "../styles/home.css";
import { Link } from "gatsby";
import { FiInfo } from "react-icons/fi";

import { FiDatabase } from "react-icons/fi";
import { BiObjectsHorizontalCenter } from "react-icons/bi";


const modelDataTypeMap = {
    "Public Datasets": ["Species Names"],
    "OrthoFinder": ["Species Names", "Protein Sequences"],
    "CDROM": ["Species Names", "Protein Sequences", "RNAseq Data"],
    "Dn/Ds": ["Species Names", "Protein Sequences", "Coding DNA Sequences"],
    "EVE Expression Shift": ["Species Names", "Protein Sequences", "RNAseq Data"],
    "EVE Diversity/Divergence": ["Species Names", "Protein Sequences", "RNAseq Data"],
    "AlphaFold": ["Species Names", "Protein Sequences"],
    "Post-Duplication Fates": ["Species Names", "Protein Sequences", "RNAseq Data"],
    "Duplication Mechanism": ["Species Names", "Protein Sequences"],
    "Pathway": ["Species Names", "Protein Sequences"],
    "Gene Ontology": ["Species Names", "Protein Sequences"]
};

const dataTypes = [
    { data: "Species Names" },
    { data: "Protein Sequences" },
    { data: "RNAseq Data" },
    { data: "Coding DNA Sequences" }
];
const modelTypes = [
    { data: "Functional Models", models: ["AlphaFold", "CDROM", "EVE Expression Shift", "Gene Ontology", "Pathway", "Post-Duplication Fates"] },
    { data: "Mechanism Models", models: ["Duplication Mechanism"] },
    { data: "Selection Models", models: ["Dn/Ds", "EVE Diversity/Divergence"] }
];

const funcTypes = [
    { data: "Functional Models", models: ["AlphaFold", "CDROM", "EVE Expression Shift", "Gene Ontology", "Pathway", "Post-Duplication Fates"] },


];
const allModels = [

    {
        title: "AlphaFold",
        subtitle: "Revolutionizing protein structure prediction",

    },
    {
        title: "CDROM",
        subtitle: "Computational Design of Regulatory Modules",

    },
    {
        title: "EVE Expression Shift",
        subtitle: "Evolutionary Variability in Expression",

    },
    {
        title: "Gene Ontology",
        subtitle: "A framework for biological knowledge",

    },
    {
        title: "Pathway",
        subtitle: "Biological pathway modeling",

    },
    {
        title: "Post-Duplication Fates",
        subtitle: "Fate determination of duplicated genes",

    },
    {
        title: "Duplication Mechanism",
        subtitle: "Mechanisms behind gene duplications",

    },
    {
        title: "Dn/Ds",
        subtitle: "Detecting selection pressure",

    },
    {
        title: "EVE Diversity/Divergence",
        subtitle: "Tracking evolutionary changes",

    },
    {
        title: "OrthoFinder",
        subtitle: "Ortholog detection and gene duplication analysis",

    },
    {
        title: "Public Datasets",
        subtitle: "Access curated biological datasets",

    }
];

const Modelcards = ({ title, subtitle }) => {
    console.log("Passing title from home.js: ", title);

    return (
        <div className="card">
            <p className="card-title">{title}</p>
            <p className="card-sub">{subtitle}</p>
            <Link to={`/Modelinfo`} state={{ title }} className="card-arrow">
                <FiInfo />
            </Link>
        </div>
    );
};


const Home = () => {
    const [selectedTypes, setSelectedTypes] = useState([]);
    const [selectedModelTypes, setSelectedModelTypes] = useState([]);

    const handleCheckboxChange = (type) => {
        setSelectedTypes((prev) =>
            prev.includes(type) ? prev.filter((item) => item !== type) : [...prev, type]
        );
    };
    const filteredModels = allModels.filter(({ title }) => {
        //Check if model matches selected data types  
        const dataTypeMatch =
            selectedTypes.length === 0 || (
                modelDataTypeMap[title] &&
                selectedTypes.length === modelDataTypeMap[title].length &&
                selectedTypes.every(type => modelDataTypeMap[title].includes(type))
            );

        //Check if model matches selected model types
        const modelTypeMatch =
            selectedModelTypes.length === 0 ||
            modelTypes.some(modelType =>
                selectedModelTypes.includes(modelType.data) &&
                modelType.models.includes(title)
            );

        //Show model only if it matches both filters  
        return dataTypeMatch && modelTypeMatch;
    });



    const handelModelTypeChange = (type) => {
        setSelectedModelTypes((prev) =>
            prev.includes(type) ? prev.filter((item) => item !== type) : [...prev, type]
        );
    };




    return (
        <div>
            <div>
                <h1 className="home-title"> Toolkit </h1>
            </div>
            <div className="model_s_container">

                <div className="selection-container">
                    <p className="filter-des"> <FiDatabase className="data-icon"></FiDatabase> Filter by Data Type:</p>
                    <div className="data-type-list">
                        {dataTypes.map((type) => (
                            <div key={type.data} className="checkbox-wrapper">
                                <input
                                    type="checkbox"
                                    checked={selectedTypes.includes(type.data)}
                                    onChange={() => handleCheckboxChange(type.data)}
                                    className="checkbox-input"
                                />
                                <div className="checkbox-label">{type.data}</div>
                            </div>
                        ))}
                    </div>
                    <p className="filter-des"> <BiObjectsHorizontalCenter className="data-icon" />Filter by Model Type:</p>

                    <div className="data-type-list">
                        {modelTypes.map((type) => (
                            <div key={type.data} className="checkbox-wrapper">
                                <input
                                    type="checkbox"
                                    checked={selectedModelTypes.includes(type.data)}
                                    onChange={() => handelModelTypeChange(type.data)}
                                    className="checkbox-input"
                                />
                                <div className="checkbox-label">{type.data}</div>
                            </div>
                        ))}

                    </div>






                </div>

                <div className="all-model-section">

                    <div className="grid">
                        {filteredModels.map((model, index) => (
                            <Modelcards key={index} {...model} />
                        ))}
                    </div>
                </div>
            </div>

        </div >



    );
};

export default Home;
