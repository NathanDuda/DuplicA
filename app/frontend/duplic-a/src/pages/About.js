import React from "react";
import Layout from "../components/Layout";
import "../styles/about.css";
import { LuDna } from "react-icons/lu";

const About = () => {
    return (
        <Layout>
            <div className="about-wrapper">
                <div className="title-section">


                    <h1>   <LuDna className="nav-logo"></LuDna> DuplicA</h1>
                    <p>Phylogenetic toolkit to analyze duplicated genes</p>

                </div>
                <hr className="divider" />

                <div className="about-section">
                    <h2>About DuplicA</h2>
                    <p>Understanding how duplicate genes evolve is key to uncovering the origins of phenotypic diversity and functional innovation across species. While many models exist to study gene duplication, they're often inaccessible - either unpublished, poorly documented, or buried in code meant for computational biologists.</p>
                    <p>This app bridges that gap. It offers a user-friendly platform for exploring a suite of published models that analyze the fate, function, and evolution of duplicate genes. We've standardized, expanded, and optimized these tools so researchers of any background can easily apply them directly to their data.</p>
                    <p>We've added new features, customization options, and performance improvements beyond what was available in the original publications - making the models both more powerful and efficient. In addition, this app offers interactive tools for creating visualizations, allowing users to explore results, compare models, and uncover insights without writing any code.</p>
                    <p>Whether you're studying evolutionary innovation, expression divergence, or structural effects of duplication, this unified toolkit empowers you to uncover meaningful insights using well-established scientific methods - all from a single, unified platform.</p>

                    <p></p>
                    <p></p>
                    <h2>Questions or Suggestions?</h2>
                    <p>Reach out to <a href="mailto:nathan.duda@temple.edu">nathan.duda@temple.edu</a></p>

                </div>

            </div>
        </Layout>

    );
}; export default About;