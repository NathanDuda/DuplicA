import React from "react";
import Home from "./Home";
import Workflow from "./Workflow";
import Visualization from "./Visualization";
import "./App.css";
import { BrowserRouter, Route,Routes,Link } from 'react-router-dom';
import HypothesisTesting from './HypothesisTesting';

import About from './About.jsx';

function App() {
  return (
    //<div className="workflow-container">
    <BrowserRouter>
      <div>
        <header className="header">
          <h1 className="logo">DuplicA</h1>
          <nav className="nav">
            <Link to="/" className="nav-link">Home</Link>
            <Link to="/Workflow" className="nav-link">Work Flow</Link>
            <Link to="/Visualization" className="nav-link">Visualization</Link>
            <Link to="/hypothesis" className="nav-link">Hypothesis Testing</Link>
            <Link to="/about" className="nav-link">About</Link>
          </nav>
        </header>
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/Workflow" element={<Workflow />} />
          <Route path="/Visualization" element={<Visualization />} />
          <Route path="/hypothesis" element={<HypothesisTesting />} />
          <Route path="/about" element={<About />} />
        </Routes>
      </div>
    </BrowserRouter>
  );
}

export default App;
