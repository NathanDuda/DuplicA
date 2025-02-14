import React, { useState } from "react";
import "./App.css";

const ToggleButton = ({ label }) => {
  const [active, setActive] = useState(false);

  return (
    <button
      className={`toggle-button ${active ? "active" : ""}`}
      onClick={() => setActive(!active)}
    >
      {label}
    </button>
  );
};

const Workflow = () => {
  return (
    <div className="workflow-container">
      <header className="header">
        <h1 className="logo">DuplicA</h1>
        <nav className="nav">
          <a href="#home" className="nav-link">Home</a>
          <a href="#workflow" className="nav-link active">WorkFlow</a>
          <a href="#visualization" className="nav-link">Visualization</a>
          <a href="#hypothesis" className="nav-link">Hypothesis Testing</a>
          <a href="#about" className="nav-link">About</a>
        </nav>
      </header>

      <div className="workflow">
        <div className="workflow-content">
          {/* Left Side - Public Data and OrthoFinder */}
          <div className="workflow-row">
            <ToggleButton label="Public Datasets" />
            <div className="arrow-line">→</div>
            <ToggleButton label="OrthoFinder" />
            <div className="arrow-line">→</div>  {/* Added arrow between OrthoFinder and Models */}
          </div>

          {/* Centered Model Sections */}
          <div className="models-wrapper">
            <div className="model-box">
              <h3>Functional Models</h3>
              <div className="button-group">
                <ToggleButton label="AlphaFold" />
                <ToggleButton label="CDROM" />
                <ToggleButton label="EVE Expression Shift" />
                <ToggleButton label="Gene Ontology" />
                <ToggleButton label="Pathway Analysis" />
                <ToggleButton label="Post-duplication Fates" />
              </div>
            </div>

            <div className="model-box">
              <h3>Mechanism Models</h3>
              <div className="button-group">
                <ToggleButton label="Duplication Mechanism" />
              </div>
            </div>

            <div className="model-box">
              <h3>Selection Models</h3>
              <div className="button-group">
                <ToggleButton label="Dn/Ds" />
                <ToggleButton label="EVE Diversity/Divergence" />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Workflow;
