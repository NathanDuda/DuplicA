
import React, { useState } from 'react';
import axios from 'axios';
import buttonJson from './multispecies_model_options.json';
import { Route,Routes,Link } from 'react-router-dom';
import Visualization from './Visualization.jsx';
import Workflow from './Workflow';
import About from './About';

import "./App.css";
import HypothesisTesting from './HypothesisTesting';
interface ToggleButtonProps{
  label: string;
}
const ToggleButton: React.FC<ToggleButtonProps> = ({ label }) => {
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

const App = () => {
  return (
    //<div className="workflow-container">

    <div className="navBar">
      <header>
        <h1>DuplicAA</h1>
        <Routes>
          <Route path="/">Home</Route>
          <Route path="/Workflow" element="{Workflow />}" />
          <Route path="/Visualization" element="{<Vizualisation />}" />
          <Route path="/hypothesis" element="{<HypothesisTesting />}" />
          <Route path="/about" element="{<About />}" />
        </Routes>
        <nav>
          <Link to="/">Home</Link>
          <Link to="/Workflow">Work Flow</Link>
          <Link to="/Visualization">Visualization</Link>
          <Link to="/hypothesis">Hypothesis Testing</Link>
          <Link to="/about">About</Link>
        </nav>
      </header>
    </div>
      /*<main className="workflow">
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
    </div>*/
  );
};

export default App;
