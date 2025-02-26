import React from "react";
import { useNavigate } from "react-router-dom";
import "./App.css";

const models = [
  {
    category: "Functional Models",
    models: ["AlphaFold", "CDROM", "EVE Expression Shift", "Gene Ontology", "Pathway", "Post-duplication Fates"],
  },
  {
    category: "Mechanism Models",
    models: ["Duplication Mechanism"],
  },
  {
    category: "Selection Models",
    models: ["Dn/Ds", "EVE Diversity/Divergence"],
  },
];

const Home = () => {
  const navigate = useNavigate();

  return (
    <div className="home-container">
      <h2>Choose a Model</h2>
      {models.map((category, index) => (
        <div key={index} className="category-section">
          <h3 className="section-header">{category.category}</h3>
          <div className="models-flex-grid">
            {category.models.map((model, i) => (
              <div
                key={i}
                className="model-card"
                onClick={() => navigate(`/model/${encodeURIComponent(model)}`)}
              >
                <h3>{model}</h3>
                <p>Click for more info</p>
              </div>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
};

export default Home;
