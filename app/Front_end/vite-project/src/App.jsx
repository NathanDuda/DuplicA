import React from "react";
import { BrowserRouter as Router, Route, Routes, NavLink } from "react-router-dom";
import Workflow from "./Workflow";
import "./App.css";

function App() {
  return (
    <Router>
      <div className="app">
        <header className="header">
          <div className="logo">DuplicA</div>
          <nav className="nav">
            <NavLink to="/" className="nav-link">Home</NavLink>
            <NavLink to="/workflow" className="nav-link">WorkFlow</NavLink>
            <NavLink to="/visualization" className="nav-link">Visualization</NavLink>
            <NavLink to="/hypothesis-testing" className="nav-link">Hypothesis Testing</NavLink>
            <NavLink to="/about" className="nav-link">About</NavLink>
          </nav>
        </header>
        <Routes>
          <Route path="/workflow" element={<Workflow />} />
          {/* Other routes can be added here */}
        </Routes>
      </div>
    </Router>
  );
}

export default App;
