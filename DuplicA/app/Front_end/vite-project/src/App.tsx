
import React, { useState } from 'react';
import axios from 'axios';
import buttonJson from './multispecies_model_options.json';

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
      <header>
        <h1>DuplicA</h1>
        <nav>
          <a href="#home">Home</a>
          <a href="#workflow" className="active">WorkFlow</a>
          <a href="#visualization">Visualization</a>
          <a href="#hypothesis">Hypothesis Testing</a>
          <a href="#about">About</a>
        </nav>
      </header>
      <main className="workflow">
        <div className="box">
          <h3>Get Public Data</h3>
          <ToggleButton label="Public Datasets" />
        </div>
        <div className="box">
          <h3>Detect Duplications</h3>
          <ToggleButton label="OrthoFinder" />
        </div>
        <div className="models">
          <h3>Functional Models</h3>
          <ToggleButton label="AlphaFold" />
          <ToggleButton label="CDROM" />
          <ToggleButton label="EVE Expression Shift" />
          <ToggleButton label="Gene Ontology" />
          <ToggleButton label="Pathway Analysis" />
          <ToggleButton label="Post-duplication Fates" />
        </div>
        <div className="models">
          <h3>Mechanism Models</h3>
          <ToggleButton label="Duplication Mechanism" />
        </div>
        <div className="models">
          <h3>Selection Models</h3>
          <ToggleButton label="Dn/Ds" />
          <ToggleButton label="EVE Diversity/Divergence" />
        </div>
      </main>
    </div>
  );
};

export default Workflow;

