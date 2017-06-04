library(DiagrammeR)

plate <- grViz("
    digraph G {

        subgraph cluster_dyads {
      label = 'dyads';
      
      
      
      
      d_cov_matrix_vowel; d_Rho_vowel;
      d_Rho_vowel->d_cov_matrix_vowel;d_cov_matrix_vowel->d_slope_vowel;d_cov_matrix_vowel->d_intercept_vowel;
      
      
      subgraph cluster_vowel {
      
      label = 'per vowel';
      
      
      node [shape = circle, style = solid]
      d_intercept_vowel; d_slope_vowel
      
      }
      
      
      d_cov_matrix_dyad; d_Rho_dyad;
      d_Rho_dyad->d_cov_matrix_dyad;d_cov_matrix_dyad->d_slope_dyad;d_cov_matrix_dyad->d_intercept_dyad;
      
      subgraph cluster_dyad {
      
      label = 'per dyad'
      
      
      node [shape = circle, style = solid]
      d_intercept_dyad; d_slope_dyad;
      
      }
      
      




      subgraph cluster_trial_i {
      
      label = 'per trial'
      
      node [shape = square, colour = grey, style = filled]
      d_k; d_n;
      
      node [shape = circle, style = solid]
      d_theta; d_intercept; d_slope;
      
      d_k->d_n [style=invis];
      d_n->d_k; d_theta->d_k;
      d_intercept->d_theta; d_slope->d_theta;
      
      d_intercept_vowel->d_intercept; d_slope_vowel->d_slope;
      d_intercept_dyad->d_intercept; d_slope_dyad->d_slope;
      }
      
      
        }

        subgraph cluster_dyads_i {
      label = 'individuals';
      
      
      
      
      i_cov_matrix_vowel; i_Rho_vowel;
      i_Rho_vowel->i_cov_matrix_vowel;i_cov_matrix_vowel->i_slope_vowel;i_cov_matrix_vowel->i_intercept_vowel;
      
      
      subgraph cluster_vowel_i {
      
      label = 'per vowel';
      
      
      node [shape = circle, style = solid]
      i_intercept_vowel; i_slope_vowel
      
      }
      
      
      i_cov_matrix_dyad; i_Rho_dyad;
      i_Rho_dyad->i_cov_matrix_dyad;i_cov_matrix_dyad->i_slope_dyad;i_cov_matrix_dyad->i_intercept_dyad;
      
      subgraph cluster_dyad {
      
      label = 'per dyad'
      
      
      node [shape = circle, style = solid]
      i_intercept_dyad; i_slope_dyad;
      
      }
      
      
      subgraph cluster_trial {
      
      label = 'per trial'
      
      node [shape = square, colour = grey, style = filled]
      i_k; i_n;
      
      node [shape = circle, style = solid]
      i_theta; i_intercept; i_slope;
      
      i_k->i_n [style=invis];
      i_n->i_k; i_theta->i_k;
      i_intercept->i_theta; i_slope->i_theta;
      
      i_intercept_vowel->i_intercept; i_slope_vowel->i_slope;
      i_intercept_dyad->i_intercept; i_slope_dyad->i_slope;
      }
      
      
        }


      node [colour = grey, style = filled]
      linguistic_alignment;
      
      node [shape = circle, style = solid]

    collective_benefit;alpha;b1;
    
    

    d_slope->collective_benefit;i_slope->collective_benefit;

    alpha->collective_benefit;b1->collective_benefit;linguistic_alignment->collective_benefit;




    }")


plate
