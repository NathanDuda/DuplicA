import React from "react";
import Home from "./Home";
import Workflow from "./Workflow";
import Visualization from "./Visualization";
import "./App.css";

function App() {
  return (
    <div className="App">
      <header className="header">
        <h1 className="logo">DuplicA</h1>
        <nav className="nav">
          <a href="#home" className="nav-link">Home</a>
          <a href="#workflow" className="nav-link">WorkFlow</a>
          <a href="#visualization" className="nav-link">Visualization</a>
          <a href="#hypothesis" className="nav-link">Hypothesis Testing</a>
          <a href="#about" className="nav-link">About</a>
        </nav>
      </header>
      <Home />
    </div>
  );
}

export default App;
