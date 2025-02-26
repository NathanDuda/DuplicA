import React from "react";
import { useParams, useNavigate } from "react-router-dom";
import "./App.css";

const modelInfo = {
  "AlphaFold": {
    title: "AlphaFold",
    subtitle: "Revolutionizing protein structure prediction",
    description: "AlphaFold is an AI system developed by DeepMind that predicts protein structures with high accuracy.",
    customization: "Provides customizable folding simulations and accuracy thresholds."
  },
  "CDROM": {
    title: "CDROM",
    subtitle: "Computational Design of Regulatory Modules",
    description: "CDROM models gene regulation and transcription factor interactions.",
    customization: "Allows users to input specific regulatory networks."
  },
  "EVE Expression Shift": {
    title: "EVE Expression Shift",
    subtitle: "Evolutionary Variability in Expression",
    description: "Analyzes gene expression shifts across evolutionary timescales.",
    customization: "Customizable parameters include tissue specificity and timeframes."
  },
  "Gene Ontology": {
    title: "Gene Ontology",
    subtitle: "A framework for biological knowledge",
    description: "Gene Ontology provides a structured vocabulary for gene function annotation.",
    customization: "Allows integration with various bioinformatics tools."
  },
  "Pathway": {
    title: "Pathway",
    subtitle: "Biological pathway modeling",
    description: "Models biological pathways and interactions between molecular components.",
    customization: "Supports pathway visualization and dynamic modeling."
  },
  "Post-duplication Fates": {
    title: "Post-duplication Fates",
    subtitle: "Fate determination of duplicated genes",
    description: "Examines the functional divergence of gene duplicates.",
    customization: "Users can analyze retention and loss patterns."
  },
  "Duplication Mechanism": {
    title: "Duplication Mechanism",
    subtitle: "Mechanisms behind gene duplications",
    description: "Explores various mechanisms of gene duplication and their evolutionary impact.",
    customization: "Options include whole-genome duplication and segmental duplication analysis."
  },
  "Dn/Ds": {
    title: "Dn/Ds",
    subtitle: "Detecting selection pressure",
    description: "Computes the ratio of nonsynonymous to synonymous mutations in protein-coding genes.",
    customization: "Supports multiple alignment methods and significance testing."
  },
  "EVE Diversity/Divergence": {
    title: "EVE Diversity/Divergence",
    subtitle: "Tracking evolutionary changes",
    description: "Analyzes genetic diversity and divergence among populations.",
    customization: "Offers user-defined selection criteria for comparison."
  },
};

const ModelDetail = () => {
  const { modelName } = useParams(); // Get model name from URL
  const navigate = useNavigate();

  console.log("Raw modelName from URL:", modelName);
  const decodedModelName = decodeURIComponent(modelName);
  console.log("Decoded modelName:", decodedModelName);

  // Ensure exact key match (case-sensitive and trimmed)
  const modelData = modelInfo[decodedModelName];

  if (!modelData) {
    console.error("Model data not found for:", decodedModelName);
    return <h2>Model information not found.</h2>;
  }

  return (
    <div className="model-detail-container">
      <h1>{modelData.title}</h1>
      <h3>{modelData.subtitle}</h3>
      <p>{modelData.description}</p>
      <h4>Customization:</h4>
      <p>{modelData.customization}</p>
      <button onClick={() => navigate("/")}>Back to Home</button>
    </div>
  );
};

export default ModelDetail;