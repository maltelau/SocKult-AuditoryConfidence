library(DiagrammeR)

plate <- grViz("
    digraph G {

        subgraph cluster_dyads {
      label = 'Dyads';
      
      
      
      subgraph cluster_vowel {
      
      label = 'per vowel';
      
      
      node [shape = circle, style = solid]
      d_intercept_vowel [label = Intercept]; d_slope_vowel[label = Slope];
      
      }
      
      
      subgraph cluster_dyad {
      
      label = 'per dyad'
      
      
      node [shape = circle, style = solid]
      d_intercept_dyad[label = Intercept]; 
      d_slope_dyad[label = Slope, peripheries = 2];
      
      }
      
      




      subgraph cluster_trial_i {
      
      label = 'per trial'
      
      node [shape = square, colour = grey, style = filled]
      d_k[label = k]; d_n[label = n];
      
      node [shape = circle, style = solid]
      d_theta[label = Theta]; d_intercept[label = Intercept]; d_slope[label = Slope];
      
      d_k->d_n [style=invis];
      d_n->d_k; d_theta->d_k;
      d_intercept->d_theta; d_slope->d_theta;
      
      d_intercept_vowel->d_intercept; d_slope_vowel->d_slope;
      d_intercept_dyad->d_intercept; d_slope_dyad->d_slope;
      }
      
      
        }

        subgraph cluster_dyads_i {
      label = 'Individuals';
      
      
      
      
      subgraph cluster_vowel_i {
      
      label = 'per vowel';
      
      
      node [shape = circle, style = solid]
      i_intercept_vowel[label = Intercept]; i_slope_vowel[label = Slope]
      
      }
      
      subgraph cluster_dyad {
      
      label = 'per dyad'
      
      
      node [shape = circle, style = solid]
      i_intercept_dyad[label = Intercept]; i_slope_dyad[label = Slope];
      
      }
      

      subgraph cluster_trial {
      
      label = 'per trial'
      
      node [shape = square, colour = grey, style = filled]
      i_k[label = k]; i_n[label = n];
      
      node [shape = circle, style = solid]
      i_theta[label = Theta]; i_intercept[label = Intercept]; i_slope[label = Slope];
      
      i_k->i_n [style=invis];
      i_n->i_k; i_theta->i_k;
      i_intercept->i_theta; i_slope->i_theta;

      i_intercept_vowel->i_intercept; i_slope_vowel->i_slope;
      i_intercept_dyad->i_intercept; i_slope_dyad->i_slope;
      }
      
      
        }

subgraph cluster_collective {
label = 'Collective Benefit model'
      node [colour = grey, style = filled]
      local_confidence_alignment[label = 'Local Confidence Alignment'];
      
      node [shape = circle, style = solid]

    collective_benefit[label = 'Collective benefit'];alpha[label = Intercept];b1[label = Slope];
    # col_mu[label = 'µ']; col_sigma[label = 'σ'];
    col_mu[label = 'Mean', peripheries = 2]; col_sigma[label = 'Sigma'];
    

    d_slope_dyad->collective_benefit [style = invis];
    d_slope_dyad->collective_benefit [style = invis];
    collective_benefit->d_slope_dyad;
    i_slope_dyad->collective_benefit;
    b1->col_mu;
    alpha->col_mu;local_confidence_alignment->col_mu;
    col_mu->collective_benefit; col_sigma->collective_benefit;


}

    }")


plate
