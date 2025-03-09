import React from "react";
import "../styles/home.css";

const DataTypeFilter = () => {
    return (
        <div className="container">


            <div className="selection-container">
                <h3>Select Data Type</h3>
                <div className="data-type-list">
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Expression Data
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        OrthoFinder Data
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Duplicate Gene Data
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Nucleotide CDS Sequence
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Protein Sequences
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Population Duplicate Gene Data
                    </label>
                    <label className="checkbox-label">
                        <input type="checkbox" />
                        Population Nucleotide CDS Sequences
                    </label>
                </div>
            </div>


            <div className="model-container">
                <h3>Available Models:</h3>
                <div className="model-list">
                    <div className="model-item enabled">AlphaFold</div>
                    <div className="model-item disabled">ROOT</div>
                    <div className="model-item disabled">EVE Expression Shift</div>
                    <div className="model-item disabled">Gene Ontology</div>
                    <div className="model-item disabled">Pathway</div>
                    <div className="model-item enabled">Post-duplication Fates</div>
                    <div className="model-item disabled">Duplication Mechanism</div>
                    <div className="model-item disabled">EVE Diversity/Divergence</div>
                    <div className="model-item disabled">Diversity/Divergence</div>



                </div>
            </div>
        </div>
    );
};

export default DataTypeFilter;

