import React from "react";
import "../styles/modelinfo.css"
import { BiDna } from "react-icons/bi";
import { useLocation } from "@reach/router";


const modelInformation = [
    {
      category: "Public Datasets",
      title: "Public Datasets",
      subtitle: "Retrieval of Reference Sequences",
      paper: "Smedley, D., Haider, S., Ballester, B., Holland, R., London, D., Thorisson, G., & Kasprzyk, A. (2009). BioMart–biological queries made easy. BMC genomics, 10, 1-12.",
      description: "Fetches protein, coding sequence (CDS), or genome data for selected species using BioMart. This tool streamlines the process of gathering reference sequences, ensuring easy access to consistent and up-to-date biological sequences."
    },
    {
      category: "Detect Duplications",
      title: "OrthoFinder",
      subtitle: "Phylogenetic duplicate gene and ortholog inference",
      paper: "Emms, D. M., & Kelly, S. (2019). OrthoFinder: phylogenetic orthology inference for comparative genomics. Genome biology, 20, 1-14.",
      description: "Identifies gene duplication events by clustering genes into orthogroups and reconstructing gene and species trees to determine where duplications have occurred. This process reveals duplicate genes and their orthologs, enabling downstream analysis of gene function, expression divergence, and evolutionary dynamics.",
      enhancements: [
        "Integrated Duplicate Extraction",
        "Automatically parses Orthofinder outputs to extract duplicate gene pairs and their orthologs across all species."
      ]
    },
    {
      category: "Models",
      title: "CDROM",
      subtitle: "Classification of Duplicate gene Retention Mechanisms",
      paper: "Assis, R., & Bachtrog, D. (2013). Neofunctionalization of young duplicate genes in Drosophila. Proceedings of the National Academy of Sciences of the United States of America, 110(43), 17409–17414. https://doi.org/10.1073/pnas.1313759110",
      description:
        "The CDROM model classifies the evolutionary fate of duplicate genes based on gene expression divergence. By comparing the expression profiles of ancestral and duplicate gene copies across multiple tissues, CDROM distinguishes between neofunctionalization, subfunctionalization, conservation, and specialization. This model is particularly useful for studying recently duplicated genes, as it reveals how expression patterns evolve post-duplication and helps identify potential functional innovation. Users may find this model valuable for exploring how new gene functions emerge or how gene dosage is preserved.",
      enhancements: [
        "Pseudofunctionalization Detection",
        "Introduces a new functional category to capture pseudofunctionalization - when one gene copy becomes nonfunctional after duplication. This improves resolution by distinguishing inactive duplicates that would otherwise be labeled as neofunctionalized.",
        "Missing Expression Handling",
        "If enabled, genes with missing expression data will be flagged as pseudogenes, offering an intuitive way to handle unmapped RNAseq reads in high-confidence datasets.",
        "Multi-Species Comparisons",
        "Automatically performs pairwise comparisons between duplicate genes in one species and their ancestral counterparts in another, across all species pairs. This expands analysis capabilities beyond just two species.",
        "Species Pair Filter",
        "Allows filtering out species pairs that have fewer than a specified number of duplicate genes and ancestral counterparts."
      ]
    },
    {
      category: "Models",
      title: "dN/dS",
      subtitle: "Detecting selection with non-synonymous to synonymous rate ratio",
      paper: "Li, W. H. (1993). Unbiased estimation of the rates of synonymous and nonsynonymous substitution. Journal of molecular evolution, 36, 96-99.",
      description: "This model estimates evolutionary rates between duplicate gene pairs using the ratio of nonsynonymous (dN) to synonymous (dS) substitutions. The calculation is performed as a pairwise calculation between the two duplicate gene copies. A dN/dS ratio >1 suggests positive selection, while a ratio <1 indicates purifying selection. This method is a classic approach to understanding the selective pressures acting on duplicate genes. Users can apply this model to investigate whether duplicate gene copies are under relaxed constraint, evolving neutrally, or undergoing adaptive divergence.",
      enhancements: [
        "Automated Preprocessing",
        "Streamlines extracting duplicate gene pairs per species, aligning their protein sequences (aligner can be customized), generating codon-aware alignments, and calculating dN/dS values."
      ]
    },
    {
      category: "Models",
      title: "EVE Expression Shift",
      subtitle: "Phylogenetic ANOVA expression-based lineage divergence",
      paper: "Rohlfs, R. V., & Nielsen, R. (2015). Phylogenetic ANOVA: the expression variance and evolution model for quantitative trait evolution. Systematic biology, 64(5), 695-708.",
      description:
        "Use phylogenetic ANOVA to detect lineage-specific shifts in gene expression variance after gene duplication. Test whether the duplicated lineage has evolved under a distinct expression optimum than the rest of the phylogeny. A significant result suggests expression or regulatory divergence in the duplicate lineage. ",
      enhancements: [
        "Gene Tree Support",
        "Supports gene tree-based analysis, increasing model specificity. This allows analysis of any duplicate gene orthogroups, rather than only lineages with a whole-genome duplication.",
        "Multi-Tissue Testing",
        "Supports testing for expression shift across multiple tissues, allowing users to identify tissue-specific divergence patterns.",
        "Complex Duplication Events",
        "Allows analysis of duplications resulting in any number of gene copies - not just pairs. (Applicable when running the model independently).",
        "Optional Single-Copy Requirement in Non-Duplicated Species",
        "Enables custom filtering criteria for orthogroups based on non-duplicated species. If TRUE, only orthogroups with exactly one copy in all non-duplicated species are kept. If FALSE, orthogroups where non-duplicated species have either one or zero copies are included."
      ]
    },
    {
      category: "Detect Duplications",
      title: "EVE Diversity/Divergence",
      subtitle: "Phylogenetic ANOVA expression-based selection test",
      paper: "Rohlfs, R. V., & Nielsen, R. (2015). Phylogenetic ANOVA: the expression variance and evolution model for quantitative trait evolution. Systematic biology, 64(5), 695-708.",
      description:
        "Examines whether duplicated genes exhibit the same expression variance patterns as their singleton orthologs across species. This model is valuable for identifying cases where duplicated genes show increased expression variability, which may indicate relaxed evolutionary constraint or adaptive divergence.",
      enhancements: [
        "Gene Tree Support",
        "Supports gene tree-based analysis, increasing model specificity. This allows analysis of any duplicate gene orthogroups, rather than only lineages with a whole-genome duplication.",
        "Multi-Tissue Testing",
        "Supports testing for expression shift across multiple tissues, allowing users to identify tissue-specific divergence patterns.",
        "Complex Duplication Events",
        "Allows analysis of duplications resulting in any number of gene copies - not just pairs. (Applicable when running the model independently).",
        "Optional Single-Copy Requirement in Non-Duplicated Species",
        "Enables custom filtering criteria for orthogroups based on non-duplicated species. If TRUE, only orthogroups with exactly one copy in all non-duplicated species are kept. If FALSE, orthogroups where non-duplicated species have either one or zero copies are included."
      ]
    },
    {
      category: "Models",
      title: "AlphaFold Database",
      subtitle: "Access Predicted 3D Protein Structures",
      paper: "Varadi, M., Anyango, S., Deshpande, M., Nair, S., Natassia, C., Yordanova, G., ... & Velankar, S. (2022). AlphaFold Protein Structure Database: massively expanding the structural coverage of protein-sequence space with high-accuracy models. Nucleic acids research, 50(D1), D439-D444.",
      description:
        "Search the AlphaFold database for each duplicate gene and retrieve its predicted 3D protein structure. Users can download these structural models to explore potential structural or functional consequences of gene duplication and divergence.",
      enhancements: [
        "Automated Database Querying",
        "Retrieves AlphaFold structures for each duplicate gene automatically. Only genes present in the AlphaFold database will return results."
        ]
    },
    {
      category: "Models",
      title: "Post-Duplication Fates",
      subtitle: "Probabilistic Classification of Duplicate gene Retention Mechanisms",
      paper: "Kalhor, R., Beslon, G., Lafond, M., & Scornavacca, C. (2023, April). Classifying the post-duplication fate of paralogous genes. In RECOMB International Workshop on Comparative Genomics (pp. 1-18). Cham: Springer Nature Switzerland.",
      description: "Classify the evolutionary fate of paralogous genes based on their expression profiles relative to the ancestral gene. It calculates probabilistic scores for multiple possible outcomes, including neofunctionalization, subfunctionalization, specialization, conservation, pseudofunctionalization, and double neofunctionalization. Rather than assigning a single fate, the model quantifies the likelihood of each divergence class. ",
      enhancements: [
        "Continuous quantity-based classification",
        "Enables functional classification based on continuous gene measurements (e.g. expression) for likelihood classification, rather than relying on discrete gene ontology terms.",
        "Customizable thresholds",
        "Allows adjustment of the thresholds for detecting functional novelty and pseudogenization. Great for tailoring the sensitivity of the model to different datasets or hypotheses."
      ]
    },
    {
      category: "Detect Duplications",
      title: "Duplication Mechanism",
      subtitle: "Exon Structure-based Classification of Duplicate gene Origin",
      paper: "Assis, R., & Bachtrog, D. (2013). Neofunctionalization of young duplicate genes in Drosophila. Proceedings of the National Academy of Sciences of the United States of America, 110(43), 17409–17414. https://doi.org/10.1073/pnas.1313759110",
      description:
        "Infers whether a gene duplication event was DNA-mediated or RNA-mediated by comparing the number of exons in each duplicate copy and the ancestral ortholog. This classification provides insight into the molecular origin of gene duplications.",
      enhancements: [
        "Custom stringency options (STANDARD)",
        "Classifies duplications as DNA-mediated when both duplicates have multiple exons, and as RNA-mediated when one copy has a single exon and the other has multiple.",
        "Custom stringency options (CONSERVATIVE)",
        "Requires that the ancestral copy has more than one exon to classify the mechanism. This helps avoid false RNA-mediated calls when the ancestral gene likely had only one exon.",
        "Custom stringency options (ULTRA-CONSERVATIVE)",
        "In addition to requiring the ancestral copy to have more than one exon, RNA-mediated duplication is only inferred when one duplicate copy has a single exon and the other duplicate has exactly the same number of exons as the ancestral copy.",
        "Automated Exon Data Retrieval",
        "If exon count data is not provided but the species-of-interest are provided, the data is automatically retrieved from BioMart."
      ]
    },
    {
      category: "Models",
      title: "Pathway",
      subtitle: "Biological Pathway Annotations",
      paper: "Smedley, D., Haider, S., Ballester, B., Holland, R., London, D., Thorisson, G., & Kasprzyk, A. (2009). BioMart–biological queries made easy. BMC genomics, 10, 1-12.",
      description:
        "Provides known pathway annotations for each duplicate gene using publicly available databases. This helps users understand the biological pathways in which duplicate genes may be involved.",
      enhancements: [
        "Automated Database Querying",
        "Automatically processes all duplicate genes and returns associated terms in a clean format."
      ]
    },
    {
      category: "Models",
      title: "Gene Ontology",
      subtitle: "Gene Ontology Annotations",
      paper: "Smedley, D., Haider, S., Ballester, B., Holland, R., London, D., Thorisson, G., & Kasprzyk, A. (2009). BioMart–biological queries made easy. BMC genomics, 10, 1-12.",
      description:
        "Returns Gene Ontology (GO) terms for biological processes, molecular functions, and cellular components associated with each duplicate gene. Useful for identifying potential roles and functional categories impacted by gene duplication.",
      enhancements: [
        "Automated Database Querying",
        "Automatically processes all duplicate genes and returns associated terms in a clean format."
      ]
    }
  ];


const ModelInformation = () => {
const location = useLocation();
const { title } = location.state || {};

console.log("Passing title from modelinformation.js: ", title);
const model = modelInformation.find((item) => item.title === title);

if (!model) {
    return <div className="info-wrapper">Model not found</div>;
}

return (
    <div className="info-wrapper">
    {/* Main Header */}
    <div className="model-header">
        <BiDna className="dna-icon" />
        <h2 className="model-name">{model.title}</h2>
    </div>

    {/* Subtitle and Description */}
    <h4 className="model-sub">{model.subtitle}</h4>
    <p className="model-des">{model.description}</p>

    {/* Model Enhancements Section */}
    {Array.isArray(model.enhancements) && model.enhancements.length > 0 && (
        <div className="model-custom">
            <strong>Model Enhancements</strong> {model.customization}
            
        <ul>
            {model.enhancements.map((item, index) => {
            if (index % 2 === 0) {
                return (
                <li key={index}>
                    {item}
                    {model.enhancements[index + 1] && (
                    <ul>
                        <li>{model.enhancements[index + 1]}</li>
                    </ul>
                    )}
                </li>
                );
            } else {
                return null;
            }
            })}
        </ul>
        </div>
    )}


    
    <h4 className="model-sub">Model Reference:</h4>
    <p className="model-des">{model.paper}</p>

    </div>
);
};

export default ModelInformation;
  