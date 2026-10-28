const translations={"Analyse scientifique": "Scientific analysis", "Votre analyse": "Your analysis", "Import → Conditions → Fenêtres → Résultats": "Import → Conditions → Windows → Results", "Fonction de coût": "Loss function", "Graine reproductible": "Random seed", "Modèle : Richard · Optimisation : évolution différentielle": "Model: Richard · Optimizer: differential evolution", "Lancer l’analyse": "Run analysis", "Annuler": "Cancel", "Le temps conserve l’unité du fichier. Les blancs sont soustraits selon le calcul historique.": "Excel times are converted to minutes. Blanks are subtracted using the historical calculation.", "Fichiers et préparation": "Files and preparation", "Aperçu": "Overview", "Exporter l’aperçu PDF": "Download overview PDF", "Réplicats et blancs": "Replicates and blanks", "Fenêtres": "Time windows", "Sélectionnez une zone du graphique ou saisissez ses bornes": "Select a region on the graph or enter its limits", "Résultats": "Results", "Courbes": "Curves", "Vue": "View", "Paramètres": "Parameters", "Paramètre": "Parameter", "Afficher": "Show", "Ajustement": "Fit", "Moyenne": "Mean", "Écart-type (halo)": "Standard deviation (ribbon)", "Écart-type (barres)": "Standard deviation (bars)", "Zoom sur la zone sélectionnée": "Zoom to selected region", "Qualité": "Quality", "Données": "Data", "Aperçu limité aux 200 premières lignes ; export complet disponible.": "Preview limited to the first 200 rows; full export available.", "Résultats et reproductibilité": "Results and reproducibility", "Données CSV": "Data CSV", "Paramètres CSV": "Parameters CSV", "Qualité CSV": "Quality CSV", "Configuration YAML": "Settings YAML", "Format de figure": "Figure format", "Largeur (cm)": "Width (cm)", "Hauteur (cm)": "Height (cm)", "Résolution (dpi)": "Resolution (dpi)", "Exporter la figure": "Download figure", "Aide": "Help", "Un parcours guidé": "A guided workflow", "Ajoutez chaque signal, affectez les puits aux conditions, puis choisissez les fenêtres avant de lancer le calcul. Une erreur ne remplace pas les résultats précédents.": "Import the signals, assign wells to conditions, then choose time windows before running the analysis. An error does not replace previous results.", "Les paramètres de calcul et la graine sont exportables. Les fichiers restent dans la session ; aucune sauvegarde permanente n’est effectuée.": "Analysis settings and the seed can be exported. Files remain in the session; no permanent storage is performed.", "L’interface historique reste accessible avec CuRveR::run_curver(legacy = TRUE). Les options historiques incomplètes ne sont pas présentées comme des modèles validés dans cette interface.": "The historical interface remains available through CuRveR::run_curver(legacy = TRUE). Incomplete historical options are not presented as validated models here.", "Importer des fichiers": "Upload files", "Fichier à analyser": "File to analyze", "Feuille OD": "OD sheet", "Feuille signal": "Signal sheet", "Colonne temps": "Time column", "Les puits sont détectés automatiquement, sans les colonnes de température. Les temps Excel sont exprimés en minutes écoulées depuis la première mesure de chaque feuille.": "Wells are detected automatically; temperature columns are excluded. Excel times are shown in minutes elapsed since the first measurement in each sheet.", "Importer OD + signal": "Import OD + signal", "Nouvelle analyse": "New analysis", "Afficher dans le tableau": "Table preview", "Aucun signal ajouté.": "No signals imported.", "Nom": "Name", "Réplicats": "Replicates", "Blancs": "Blanks", "Enregistrer / renommer": "Save / rename", "Nouvelle condition": "New condition", "Supprimer": "Delete", "Importer une configuration YAML": "Import conditions YAML", "Exporter YAML": "Download YAML", "Puits": "Well", "Appliquer à tous les puits et signaux": "Apply to all wells and signals", "Début": "Start", "Fin": "End", "Appliquer la fenêtre": "Apply time window", "Prêt. Ajoutez un signal pour commencer.": "Ready. Import signals to begin.", "Aucune donnée": "No data", "Analyse en cours dans un processus séparé…": "Analysis running in a separate process…", "Analyse terminée.": "Analysis complete.", "Analyse annulée.": "Analysis cancelled.", "Analyse non lancée.": "Analysis not started.", "Échec de l’analyse ; résultats précédents conservés.": "Analysis failed; previous results retained.", "Paramètres modifiés : relancez l’analyse avant export.": "Settings changed: rerun analysis before exporting.", "OD et signal importés. Vous pouvez définir les conditions.": "OD and signal imported. You can now define conditions.", "Une analyse est déjà en cours.": "An analysis is already running.", "Choisissez deux feuilles distinctes.": "Choose two different sheets.", "Graine invalide.": "Invalid seed.", "Nommez la condition.": "Enter a condition name.", "Ce nom existe déjà.": "This name already exists.", "Le début doit précéder la fin.": "Start must be before end.", "Nouvelle": "New", "Browse...": "Browse..."};

translations["Triplet blanc commun"]="Shared blank triplet";
translations["Aucun"]="None";
translations["Soustrait à toutes les conditions, séparément pour chaque signal. Le triplet blanc est exclu des ajustements."]="Subtracted from every condition, separately for each signal. The blank triplet is excluded from fitting.";
translations["Groupe blanc commun"]="Shared blank group";
translations["Renommez le groupe et ajustez ses puits, puis enregistrez. Un puits ajouté est déplacé depuis son ancien groupe."]="Rename the group and adjust its wells, then save. Added wells are moved from their previous group.";
translations["La sélection sur le graphique est enregistrée automatiquement. Pour des bornes saisies au clavier, cliquez sur Appliquer."]="Graph selections are saved automatically. For limits entered by keyboard, click Apply.";
translations["La sélection graphique et les bornes saisies sont enregistrées automatiquement. Cocher la case applique immédiatement la fenêtre affichée à tous les puits et signaux."]="Graph selections and typed limits are saved automatically. Checking the box immediately applies the displayed window to all wells and signals.";
translations["Analyse — Modèle de Richard (4 paramètres)"]="Analysis — Richard model (4 parameters)";
translations["Modèle : Richard · Optimisation : évolution différentielle · Coût : LAD"]="Model: Richard · Optimizer: differential evolution · Loss: LAD";
translations["Options du graphique"]="Plot options";
translations["Valeurs résumées et qualité de l’ajustement"]="Summarized values and fit quality";
translations["Données complètes"]="Complete data";
translations["Lignes par page"]="Rows per page";
translations["Modèle : Richard · Optimisation : évolution différentielle et raffinement borné · Coût : LAD"]="Model: Richard · Optimizer: differential evolution and bounded refinement · Loss: LAD";
(()=>{
let language='en';
const original=new WeakMap();
function translate(){
 const selector=document.getElementById('language');language=selector?.value||'en';
 document.documentElement.lang=language;
 const walker=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT);
 let node;
 while(node=walker.nextNode()){
  if(node.parentElement.closest('script,style,textarea,td'))continue;
  const text=node.textContent.trim();
  const count=text.match(/^(\d+) signal\(s\), (\d+) (puits|wells)\.$/);
  if(count){const next=`${count[1]} signal(s), ${count[2]} ${language==='en'?'wells':'puits'}.`;if(text!==next)node.textContent=next;continue;}

  let fr=original.get(node);
  if(!fr || (text!==fr && text!==translations[fr])){
   if(translations[text]) fr=text;
   else {const entry=Object.entries(translations).find(([a,b])=>b===text);fr=entry?.[0];}
   if(fr)original.set(node,fr);
  }
  if(fr){const next=language==='en'?translations[fr]:fr;if(text!==next)node.textContent=node.textContent.replace(text,next);}
 }
}
function parameterTitles(){
 const definitions={};document.querySelectorAll('#parameter_help abbr').forEach(e=>definitions[e.textContent.trim()]=e.title);
 document.querySelectorAll('#parameters th,#metric option,#metric + .selectize-control .item,#features label').forEach(e=>{const key=e.textContent.trim();if(definitions[key])e.title=definitions[key]});
}
function start(){translate();parameterTitles();document.addEventListener('change',e=>{if(e.target.id==='language')translate()});
 new MutationObserver(()=>{translate();parameterTitles()}).observe(document.body,{childList:true,subtree:true,characterData:true});}
if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',start);else start();
})();

$(document).on('shiny:connected',()=>{
 Shiny.addCustomMessageHandler('curver-overview-resize',(message)=>{setTimeout(()=>window.dispatchEvent(new Event('resize')),200)});
});

// Capture the series at edit time: delayed edits cannot leak into another well.
(()=>{
 const pending=new Map();
 function capture(){
  const left=document.getElementById('windows-left'),right=document.getElementById('windows-right');
  if(!left||!right||left.value===''||right.value==='')return null;
  return {left:Number(left.value),right:Number(right.value),
   signal:document.getElementById('windows-signal')?.value,
   well:document.getElementById('windows-well')?.value,
   all:document.getElementById('windows-all')?.checked===true,nonce:Date.now()};
 }
 function send(edit){if(edit?.signal&&edit?.well&&window.Shiny)Shiny.setInputValue('windows-bounds_edit',edit,{priority:'event'});}
 document.addEventListener('input',event=>{
  if(!event.isTrusted||!['windows-left','windows-right'].includes(event.target.id))return;
  const edit=capture();if(!edit)return;const key=edit.signal+'|'+edit.well;
  clearTimeout(pending.get(key));pending.set(key,setTimeout(()=>{pending.delete(key);send(edit)},400));
 });
 document.addEventListener('change',event=>{
  if(event.target.id==='windows-all'){
   for(const timer of pending.values())clearTimeout(timer);pending.clear();
   const edit=capture();if(edit)send(edit);
  }
 });
})();
