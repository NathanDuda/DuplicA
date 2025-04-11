import React from "react"
import { useState } from "react";
import Layout from "../components/Layout"


const models = [
    {
        category: "Models",
        title: "AlphaFold",
        subtitle: "Revolutionizing protein structure prediction",
        description: "AlphaFold is an AI system developed by DeepMind that predicts protein structures with high accuracy.",
        customization: "Provides customizable folding simulations and accuracy thresholds."
    },
    {
        category: "Models",
        title: "CDROM",
        subtitle: "Computational Design of Regulatory Modules",
        description: "CDROM models gene regulation and transcription factor interactions.",
        customization: "Allows users to input specific regulatory networks."
    },
    {
        category: "Models",

        title: "EVE Expression Shift",
        subtitle: "Evolutionary Variability in Expression",
        description: "Analyzes gene expression shifts across evolutionary timescales.",
        customization: "Customizable parameters include tissue specificity and timeframes."
    },
    {
        category: "Models",
        title: "Gene Ontology",
        subtitle: "A framework for biological knowledge",
        description: "Gene Ontology provides a structured vocabulary for gene function annotation.",
        customization: "Allows integration with various bioinformatics tools."
    },
    {
        category: "Models",
        title: "Pathway",
        subtitle: "Biological pathway modeling",
        description: "Models biological pathways and interactions between molecular components.",
        customization: "Supports pathway visualization and dynamic modeling."
    },
    {
        category: "Models",
        title: "Post-duplication Fates",
        subtitle: "Fate determination of duplicated genes",
        description: "Examines the functional divergence of gene duplicates.",
        customization: "Users can analyze retention and loss patterns."
    },];

const duplication = [{

    category: "Detect Duplications",
    title: "Duplication Mechanism",
    subtitle: "Mechanisms behind gene duplications",
    description: "Explores various mechanisms of gene duplication and their evolutionary impact.",
    customization: "Options include whole-genome duplication and segmental duplication analysis."
},

{
    category: "Detect Duplications",
    title: "Dn/Ds",
    subtitle: "Detecting selection pressure",
    description: "Computes the ratio of nonsynonymous to synonymous mutations in protein-coding genes.",
    customization: "Supports multiple alignment methods and significance testing."
},
{
    category: "Detect Duplications",
    title: "EVE Diversity/Divergence",
    subtitle: "Tracking evolutionary changes",
    description: "Analyzes genetic diversity and divergence among populations.",
    customization: "Offers user-defined selection criteria for comparison."
},];


const Modelcards = ({ title, subtitle, description, customization }) => {
    return (
        <div className="card">
            <h3 className="card-title">{title}</h3>
            <p className="card-sub">{subtitle}</p>
            <p className="card-des">{description}</p>
            <p className="card-custom">Customization: {customization}</p>
        </div>
    )
}


const Homeextended = () => {


    return (
        <Layout>
            <h2 className="heading ">All Available Models</h2>

            <div className="all-model-section">

                <div className="section">
                    <h2 className="section-title" >Models</h2>
                    <div className="grid">
                        {models.map((model, index) => (
                            <Modelcards key={index} {...model} />
                        ))}
                    </div>
                </div>

                <div className="section">
                    <h2 className="section-title">Detect Duplications</h2>
                    <div className="grid">
                        {duplication.map((dup, index) => (
                            < Modelcards key={index} {...dup} />
                        ))}
                    </div>

                </div>
            </div >




        </Layout >
    )

};
export default Homeextended