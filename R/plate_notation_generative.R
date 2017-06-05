library(DiagrammeR)

plate <- grViz("
               digraph G {
               
               subgraph cluster_dyads {
               label = 'dyads';
               
               
               
               
               
               subgraph cluster_vowel {
               
               label = 'per vowel';
               
               
               node [shape = circle, style = solid]
               d_intercept_vowel;
               d_slope_vowel;
               
               }
               
                              subgraph cluster_invis_2 {
               style = invis;
               d_intercept; d_slope;
               d_intercept_dyad->d_intercept [style = invis]; 
               d_slope_vowel->d_slope [style = invis]; 


               }
               subgraph cluster_dyad {
               
               label = 'per dyad'
               
               
               node [shape = circle, style = solid]
               d_intercept_dyad; 
               d_slope_dyad  [peripheries = 2];
               d_slope->d_slope_dyad; d_slope_vowel->d_slope_dyad; i_slope_dyad->d_slope_dyad;
               
               }
               

               
               subgraph cluster_trial_d {
               
               label = 'per trial'
               
               node [shape = square, colour = grey, style = filled]
               d_k; d_n;
               
               node [shape = circle, style = solid]
               d_theta; d_intercept; 
               d_slope;
               
               d_k->d_n [style=invis];
               d_n->d_k; d_theta->d_k;
               
               
               d_intercept->d_intercept_trial;
               d_slope->d_slope_trial;
               d_intercept_trial->d_theta; 
               d_slope_trial->d_theta; 

               d_intercept_dyad->d_intercept_trial; 
               d_intercept_vowel->d_intercept_trial; 
               d_slope_vowel->d_slope_trial;
               d_slope_dyad->d_slope_trial;

               }
               
               
               }
               
               subgraph cluster__i {
               label = 'individuals';
               
               
               
               
               
               subgraph cluster_vowel_i {
               
               label = 'per vowel';
               
               
               node [shape = circle, style = solid]
               i_intercept_vowel; i_slope_vowel
               
               }
               
               
               subgraph cluster_dyad_i {
               
               label = 'per dyad'
               
               
               node [shape = circle, style = solid]
               i_intercept_dyad; i_slope_dyad;
               
               }
               
               subgraph cluster_invis_1 {
               style = invis;
               i_intercept; i_slope;
               i_intercept_dyad->i_intercept [style = invis]; 
               i_slope_vowel->i_slope [style = invis]; 


               }
               
               subgraph cluster_trial_i {
               
               label = 'per trial'
               
               node [shape = square, colour = grey, style = filled]
               i_k; i_n;
               
               node [shape = circle, style = solid]
               i_theta; i_intercept_trial; i_slope;
               
               i_k->i_n [style=invis];
               i_n->i_k; i_theta->i_k;
               
               
               i_intercept->i_intercept_trial;
               i_slope->i_slope_trial;
               i_intercept_trial->i_theta; 
               i_slope_trial->i_theta; 


               i_intercept_dyad->i_intercept_trial; 
               i_intercept_vowel->i_intercept_trial; 
               i_slope_vowel->i_slope_trial;
               i_slope_dyad->i_slope_trial;
               }
               
               
               }
               
               
               node [colour = grey, style = filled]
               linguistic_alignment;
               
               node [shape = circle, style = solid]
               
               collective_benefit;alpha;b_align;
               
               
               collective_benefit->d_slope;
               i_slope->d_slope;
               
               alpha->collective_benefit;
               b_align->collective_benefit;linguistic_alignment->collective_benefit;
               
               
               
               
               }")


plate
