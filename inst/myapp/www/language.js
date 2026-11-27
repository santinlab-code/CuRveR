const translations={"Analyse scientifique": "Scientific analysis", "Votre analyse": "Your analysis", "Import → Conditions → Fenêtres → Résultats": "Import → Conditions → Windows → Results", "Fonction de coût": "Loss function", "Graine reproductible": "Random seed", "Modèle : Richard · Optimisation : évolution différentielle": "Model: Richard · Optimizer: differential evolution", "Lancer l’analyse": "Run analysis", "Annuler": "Cancel", "Le temps conserve l’unité du fichier. Les blancs sont soustraits selon le calcul historique.": "Excel times are converted to minutes. Blanks are subtracted using the historical calculation.", "Fichiers et préparation": "Files and preparation", "Aperçu": "Overview", "Exporter l’aperçu PDF": "Download overview PDF", "Réplicats et blancs": "Replicates and blanks", "Fenêtres": "Time windows", "Sélectionnez une zone du graphique ou saisissez ses bornes": "Select a region on the graph or enter its limits", "Résultats": "Results", "Courbes": "Curves", "Vue": "View", "Paramètres": "Parameters", "Paramètre": "Parameter", "Afficher": "Show", "Ajustement": "Fit", "Moyenne": "Mean", "Écart-type (halo)": "Standard deviation (ribbon)", "Écart-type (barres)": "Standard deviation (bars)", "Zoom sur la zone sélectionnée": "Zoom to selected region", "Qualité": "Quality", "Données": "Data", "Aperçu limité aux 200 premières lignes ; export complet disponible.": "Preview limited to the first 200 rows; full export available.", "Résultats et reproductibilité": "Results and reproducibility", "Données CSV": "Data CSV", "Paramètres CSV": "Parameters CSV", "Qualité CSV": "Quality CSV", "Configuration YAML": "Settings YAML", "Format de figure": "Figure format", "Largeur (cm)": "Width (cm)", "Hauteur (cm)": "Height (cm)", "Résolution (dpi)": "Resolution (dpi)", "Exporter la figure": "Download figure", "Aide": "Help", "Un parcours guidé": "A guided workflow", "Ajoutez chaque signal, affectez les puits aux conditions, puis choisissez les fenêtres avant de lancer le calcul. Une erreur ne remplace pas les résultats précédents.": "Import the signals, assign wells to conditions, then choose time windows before running the analysis. An error does not replace previous results.", "Les paramètres de calcul et la graine sont exportables. Les fichiers restent dans la session ; aucune sauvegarde permanente n’est effectuée.": "Analysis settings and the seed can be exported. Files remain in the session; no permanent storage is performed.", "L’interface historique reste accessible avec CuRveR::run_curver(legacy = TRUE). Les options historiques incomplètes ne sont pas présentées comme des modèles validés dans cette interface.": "The historical interface remains available through CuRveR::run_curver(legacy = TRUE). Incomplete historical options are not presented as validated models here.", "Importer des fichiers": "Upload files", "Fichier à analyser": "File to analyze", "Feuille OD": "OD sheet", "Feuille signal": "Signal sheet", "Colonne temps": "Time column", "Les puits sont détectés automatiquement, sans les colonnes de température. Les temps Excel sont exprimés en minutes écoulées depuis la première mesure de chaque feuille.": "Wells are detected automatically; temperature columns are excluded. Excel times are shown in minutes elapsed since the first measurement in each sheet.", "Importer OD + signal": "Import OD + signal", "Nouvelle analyse": "New analysis", "Afficher dans le tableau": "Table preview", "Aucun signal ajouté.": "No signals imported.", "Nom": "Name", "Réplicats": "Replicates", "Blancs": "Blanks", "Enregistrer / renommer": "Save / rename", "Nouvelle condition": "New condition", "Supprimer": "Delete", "Importer une configuration YAML": "Import conditions YAML", "Exporter YAML": "Download YAML", "Puits": "Well", "Appliquer à tous les puits et signaux": "Apply to all wells and signals", "Début": "Start", "Fin": "End", "Appliquer la fenêtre": "Apply time window", "Prêt. Ajoutez un signal pour commencer.": "Ready. Import signals to begin.", "Aucune donnée": "No data", "Analyse en cours dans un processus séparé…": "Analysis running in a separate process…", "Analyse terminée.": "Analysis complete.", "Analyse annulée.": "Analysis cancelled.", "Analyse non lancée.": "Analysis not started.", "Échec de l’analyse ; résultats précédents conservés.": "Analysis failed; previous results retained.", "Paramètres modifiés : relancez l’analyse avant export.": "Settings changed: rerun analysis before exporting.", "OD et signal importés. Vous pouvez définir les conditions.": "OD and signal imported. You can now define conditions.", "Une analyse est déjà en cours.": "An analysis is already running.", "Choisissez deux feuilles distinctes.": "Choose two different sheets.", "Graine invalide.": "Invalid seed.", "Nommez la condition.": "Enter a condition name.", "Ce nom existe déjà.": "This name already exists.", "Le début doit précéder la fin.": "Start must be before end.", "Nouvelle": "New", "Browse...": "Browse..."};

translations["Analyse — Richard, régression segmentée et statistiques"]="Analysis — Richard, segmented regression and statistics";
translations["Options de la régression segmentée"]="Segmented regression options";
translations["Échelle"]="Scale";
translations["Auto : log pour l’OD, linéaire pour les autres signaux"]="Auto: log for OD, linear for other signals";
translations["Logarithmique pour tous les signaux"]="Logarithmic for all signals";
translations["Linéaire pour tous les signaux"]="Linear for all signals";
translations["Points de rupture"]="Breakpoints";
translations["Automatique (BIC)"]="Automatic (BIC)";
translations["Nombre fixe"]="Fixed number";
translations["Guidé (temps initiaux)"]="Guided (initial times)";
translations["Maximum de ruptures"]="Maximum breakpoints";
translations["Nombre de ruptures"]="Number of breakpoints";
translations["Temps initiaux (min, séparés par des virgules)"]="Initial times (min, comma-separated)";
translations["Ajuster aussi Richard puits par puits (nécessaire aux statistiques sur Richard, plus long)"]="Also fit Richard well by well (needed for Richard statistics, slower)";
translations["Segments"]="Segments";
translations["Statistiques"]="Statistics";
translations["Paramètre testé"]="Tested parameter";
translations["Condition de référence"]="Reference condition";
translations["Comparaisons"]="Comparisons";
translations["Contre la référence"]="Against reference";
translations["Toutes les paires"]="All pairs";
translations["Comparaison des modèles (Richard vs segments)"]="Model comparison (Richard vs segments)";
translations["ΔAICc = AICc(Richard) − AICc(segments) : > 10 favorise les segments, < −10 favorise Richard. Résidus structurés : test des runs p < 0,01 et écart systématique maximal (résidu moyen par temps) > 5 % de l’amplitude du signal, ce qui indique que le modèle rate une phase. Les deux modèles sont évalués sur les mêmes points, en unités du signal."]="ΔAICc = AICc(Richard) − AICc(segments): > 10 favours segments, < −10 favours Richard. Structured residuals: runs test p < 0.01 and largest systematic deviation (mean residual per time) > 5% of the signal amplitude, meaning the model misses a phase. Both models are evaluated on the same points, in signal units.";
translations["Phases de la régression segmentée"]="Segmented regression phases";
translations["Statistiques entre conditions"]="Statistics between conditions";
translations["n = nombre de puits. Des puits d’une même plaque sont des réplicats techniques : les tests décrivent alors la variabilité technique, pas la reproductibilité biologique. IC à 95 % (loi de Student), test de Welch, correction de Holm, g de Hedges."]="n = number of wells. Wells from one plate are technical replicates: tests then describe technical variability, not biological reproducibility. 95% CI (Student), Welch test, Holm correction, Hedges' g.";
translations["Comparaison des modèles"]="Model comparison";
translations["Phases segmentées"]="Segmented phases";
translations["Paramètres par puits"]="Parameters by well";
translations["Tests statistiques"]="Statistical tests";
translations["Feuille de signal facultative"]="Optional signal sheet";
translations["Importer les données"]="Import data";
translations["OD importée. Vous pouvez définir les conditions."]="OD imported. You can now define conditions.";
translations["Nombre de réplicats techniques"]="Number of technical replicates";
translations["Changer ce nombre recrée les groupes par ligne et conserve le groupe blanc sélectionné."]="Changing this number regenerates groups by row and keeps the selected blank group.";
translations["Aucun"]="None";
translations["Le groupe blanc est exclu des ajustements. Sa moyenne est soustraite à chaque condition pour chaque signal."]="The blank group is excluded from fitting. Its mean is subtracted from each condition for each signal.";
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
translations["Afficher les valeurs corrigées négatives"]="Show negative corrected values";
translations["Enregistrer le graphique"]="Save plot";
translations["Options de l’image"]="Image options";
translations["Tableaux de résultats"]="Results tables";
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
function start(){translate();document.addEventListener('change',e=>{if(e.target.id==='language')translate()});
 new MutationObserver(()=>{translate()}).observe(document.body,{childList:true,subtree:true,characterData:true});}
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
