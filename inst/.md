---
title: "UML"
output: html_document
---

## UML Diagram

The main intention of this UML diagram is to introduce and facilitate understanding of the design behind the
`msb` package. 

Copy and paste the code below to <a href="https://mermaid.live" target="_blank">Mermaid Live Editor</a> to view it as a UML graph.

```mermaid
flowchart TD
    subgraph A[do_simulation]
        B[gen_data]
        C[maic]
        D[stc]
        E[butcher]
        F[calc_true]
        
        B --> C
        B --> D
        B --> E
        B --> F
    end
    
    A --> G[analyse_simulations]
    A --> H[facet_coverage_plot]
    
    G --> I[facet_bias_plot]
    G --> J[facet_se_plot]
    G --> K[format_table]
    
    L[facet_overlap_plot]
    
    style A fill:#e1f5fe
    style G fill:#fff3e0
    style H fill:#fce4ec
    style I fill:#fce4ec
    style J fill:#fce4ec
    style K fill:#fce4ec
    style L fill:#fce4ec
```
