import React, { useState } from "react";

import "../styles/home.css";
import { Link } from "gatsby";
import { FiInfo } from "react-icons/fi";

import { FiDatabase } from "react-icons/fi";
import { BiObjectsHorizontalCenter } from "react-icons/bi";


const modelDataTypeMap = {
    "Public Datasets": ["Species Names"],
    "OrthoFinder": ["Protein Sequences"],
    "CDROM": ["Protein Sequences", "RNAseq Data"],
    "dN/dS": ["Protein Sequences", "Coding DNA Sequences"],
    "EVE Expression Shift": ["Protein Sequences", "RNAseq Data"],
    "EVE Diversity/Divergence": ["Protein Sequences", "RNAseq Data"],
    "AlphaFold Database": ["Species Names", "Protein Sequences"],
    "Post-Duplication Fates": ["Protein Sequences", "RNAseq Data"],
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
    { data: "Functional Models", models: ["AlphaFold Database", "CDROM", "EVE Expression Shift", "Gene Ontology", "Pathway", "Post-Duplication Fates"] },
    { data: "Selection Models", models: ["dN/dS", "EVE Diversity/Divergence"] },
    { data: "Data Prepraration Models", models: ['OrthoFinder', 'Public Datasets'] },
    { data: "Mechanism Models", models: ["Duplication Mechanism"] }
];


const allModels = [

    {
        title: "AlphaFold Database",
        subtitle: "Access Predicted 3D Protein Structures",

    },
    {
        title: "CDROM",
        subtitle: "Classification of Duplicate gene Retention Mechanisms",

    },
    {
        title: "EVE Expression Shift",
        subtitle: "Phylogenetic ANOVA expression-based lineage divergence",

    },
    {
        title: "Gene Ontology",
        subtitle: "Gene Ontology Annotations",

    },
    {
        title: "Pathway",
        subtitle: "Biological Pathway Annotations",

    },
    {
        title: "Post-Duplication Fates",
        subtitle: "Probabilistic Classification of Duplicate gene Retention Mechanisms",

    },
    {
        title: "Duplication Mechanism",
        subtitle: "Exon Structure-based Classification of Duplicate gene Origin",

    },
    {
        title: "dN/dS",
        subtitle: "Detecting selection with non-synonymous to synonymous rate ratio",

    },
    {
        title: "EVE Diversity/Divergence",
        subtitle: "Phylogenetic ANOVA expression-based selection test",

    },
    {
        title: "OrthoFinder",
        subtitle: "Phylogenetic duplicate gene and ortholog inference",

    },
    {
        title: "Public Datasets",
        subtitle: "Retrieval of Reference Sequences",

    }
];

const Modelcards = ({ title, subtitle }) => {
    console.log("Passing title from home.js: ", title);

    return (
        <Link to={`/Modelinfo`} state={{ title }} className="card-link">
            <div className="card">
                <p className="card-title">{title}</p>
                <p className="card-sub">{subtitle}</p>
                <div className="card-arrow">
                <FiInfo />
                </div>
            </div>
        </Link>

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
        // Check if model matches selected data types
        const dataTypeMatch =
          selectedTypes.length === 0 ||
          (modelDataTypeMap[title] &&
           modelDataTypeMap[title].every(type => selectedTypes.includes(type)));
      
        // Check if model matches selected model types
        const modelTypeMatch =
          selectedModelTypes.length === 0 ||
          modelTypes.some(modelType =>
            selectedModelTypes.includes(modelType.data) &&
            modelType.models.includes(title)
          );
      
        return dataTypeMatch && modelTypeMatch;
      });
      



    const handelModelTypeChange = (type) => {
        setSelectedModelTypes((prev) =>
            prev.includes(type) ? prev.filter((item) => item !== type) : [...prev, type]
        );
    };

    const groupedModels = modelTypes.reduce((acc, modelType) => {
        const modelsInThisType = filteredModels.filter(model =>
            modelType.models.includes(model.title)
        );
        if (modelsInThisType.length > 0) {
            acc.push({
                type: modelType.data,
                models: modelsInThisType
            });
        }
        return acc;
    }, []);





    return (
        <div>
            <div>
                <h1 className="home-title">&nbsp;</h1>
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
                    {groupedModels.map((group) => (
                        <div key={group.type} className="model-group">
                            <h3 className="model-type-heading">{group.type}</h3>
                            <div className="grid">
                                {group.models.map((model, index) => (
                                    <Modelcards key={index} {...model} />
                                ))}
                            </div>
                        </div>
                    ))}
                </div>
            </div>

        </div >



    );
};

export default Home;