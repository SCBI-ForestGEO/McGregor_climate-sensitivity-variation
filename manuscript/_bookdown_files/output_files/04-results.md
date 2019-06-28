

# Results

## Model runs
When including only biophysical traits, trees' resistance value (on a per-species basis) is explained best by crown position and height, with codominant trees being the most resistant to drought. This follows on work done by [@bennett_larger_2015] [and others?] which show that larger trees suffer more during drought, and confirms that this susceptibility can be seen in tree ring analyses. Adding in crown position with the leaf hydraulic traits yields a slightly worse predictive model for drought tolerance, with height remaining as the only significant biophysical variable.



<table class="table" style="width: auto !important; float: left; margin-right: 10px;">
<caption>Biophysical Model</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> coefficients </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> (Intercept) </td>
   <td style="text-align:left;"> 4.5959863 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> position_alldominant </td>
   <td style="text-align:left;"> -0.0443306 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> position_allintermediate </td>
   <td style="text-align:left;"> -0.0393504 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> position_allsuppressed </td>
   <td style="text-align:left;"> -0.0455848 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> height.ln.m </td>
   <td style="text-align:left;"> -0.0824031 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> year </td>
   <td style="text-align:left;"> -0.0017463 </td>
  </tr>
</tbody>
</table>

<table class="table" style="width: auto !important; margin-right: 0; margin-left: auto">
<caption>Full Model</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> coefficients </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> (Intercept) </td>
   <td style="text-align:left;"> 4.3699798 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> rpring </td>
   <td style="text-align:left;"> 0.1317385 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> rpsemi-ring </td>
   <td style="text-align:left;"> 0.2535448 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> PLA_dry_percent </td>
   <td style="text-align:left;"> -0.0075767 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> LMA_g_per_m2 </td>
   <td style="text-align:left;"> -0.0042186 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> WD_g_per_cm3 </td>
   <td style="text-align:left;"> -0.3024895 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean_TLP_Mpa </td>
   <td style="text-align:left;"> -0.2988857 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> height.ln.m </td>
   <td style="text-align:left;"> -0.0592808 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> year </td>
   <td style="text-align:left;"> -0.0018175 </td>
  </tr>
</tbody>
</table>

From [@kannenberg_linking_2019], species with diffuse porous wood anatomy (*Liriodendron*) are more sensitive to drought, whereas ring-porous are not as affected because they more easily rebuild structures for hydraulic conductivity. This paper mentions it would be good to have this data with respect to latent affects from drought.


### Vertical Profiles

![Climate variables with height](tables_figures/NEON_vertical_profiles.png){width=500px}

### Traits to height
![Leaf hydraulic traits with log-values of height](tables_figures/traits_vs_traits.png){width=500px}
