import React from "react";
import "../styles/about.css";

const AboutComponent = () => {
  return (
    <div className="about-container">
    <div className="about-content">
      <h3>What is DuplicA?</h3>
      <p>
        Understanding duplicate genes in assembled genomes is essential for uncovering how phenotypic variation arises and functional diversity evolves.
      </p>
      <br />
      <p>
        We developed DuplicA, Duplicate Gene Analysis, as a user-friendly software solution with an intuitive graphical interface that eliminates the need for
        coding. DuplicA provides powerful tools for assessing selective pressures, analyzing coding sequence divergence, and evaluating gene expression differences.
      </p>
      <br />
      <p>
        By streamlining these complex analyses into an accessible platform, DuplicA empowers researchers across disciplines to explore and interpret genomic data with ease.
        Our goal is to accelerate discoveries in evolutionary genomics by making sophisticated bioinformatics tools more widely available.
      </p>
    </div>
    </div>
  );
};

export default AboutComponent;
