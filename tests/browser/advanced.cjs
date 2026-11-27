// End-to-end check of the CuRveR 2.1 modules with inst/extdata/test_plate_2.1.xlsx (OD + GFP).
// Start the app on port 3839, then: node tests/browser/advanced.cjs [screenshot-dir]
const {chromium}=require('playwright');const path=require('path');
const file=path.join(__dirname,'..','..','inst','extdata','test_plate_2.1.xlsx');const shots=process.argv[2];
(async()=>{const b=await chromium.launch({headless:true,args:['--no-sandbox']});
const page=await b.newPage({viewport:{width:1440,height:1100}});
const set=(id,v)=>page.evaluate(([id,v])=>{const e=document.querySelector(id);if(e.selectize)e.selectize.setValue(v);else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}))}},[id,v]);const errors=[];page.on('pageerror',e=>errors.push(e.message));
await page.goto('http://127.0.0.1:3839');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
await page.waitForTimeout(1500);await page.locator('#import-files').setInputFiles(file);
await page.waitForFunction(()=>document.querySelector('#import-time')?.options.length>0);await page.waitForTimeout(1000);
await set('#import-mch','GFP');await page.waitForTimeout(800);
await page.locator('#import-pair').click();await page.waitForFunction(()=>/2 sign/.test(document.querySelector('#import-status').innerText));
await page.locator('.navbar a[data-value="Conditions"]').click();
await page.waitForFunction(()=>document.querySelector('#conditions-summary').innerText.includes('E1-E3'));
await set('#conditions-global_blank','E1-E3');await page.waitForTimeout(500);
for(const [old,name] of [['A1-A3','Sigmoid'],['B1-B3','Diauxie'],['C1-C3','Lysis'],['D1-D3','Lysis_delayed']]){
 await set('#conditions-selected',old);
 await page.waitForFunction(o=>document.querySelector('#conditions-name').value===o,old);
 await page.locator('#conditions-name').fill(name);await page.locator('#conditions-save').click();
 await page.waitForFunction(n=>document.querySelector('#conditions-summary').innerText.includes(n),name);}
await page.locator('.navbar a[data-value="Résultats"]').click();
const t0=Date.now();await page.locator('#run').click();
await page.waitForFunction(()=>/terminée|complete/.test(document.querySelector('#status').innerText),null,{timeout:600000});
console.log('analysis seconds:',Math.round((Date.now()-t0)/1000));
await page.locator('#result_plot img').waitFor();
const alert=await page.locator('#model_alert').innerText();console.log('alert:',alert.replace(/\n/g,' '));
if(!/Lysis/.test(alert)||!/Diauxie/.test(alert))throw Error('Expected Richard warnings for Lysis and Diauxie');
if(shots)await page.screenshot({path:path.join(shots,'1_curves.png'),fullPage:false});
await page.locator('#plot_type input[value="segments"]').check();await page.waitForTimeout(2500);
if(shots)await page.locator('#result_plot').screenshot({path:path.join(shots,'2_segments.png')});
await page.locator('#plot_type input[value="stats"]').check();await page.waitForTimeout(1000);
await set('#stat_param','Segmented::phase3_start');
await set('#stat_reference','Lysis');
await page.evaluate(()=>{const s=document.querySelectorAll('details');s.forEach(d=>d.open=true)});
await page.waitForTimeout(2500);
if(shots)await page.locator('#result_plot').screenshot({path:path.join(shots,'3_stats.png')});
const tests=await page.locator('#stat_tests').innerText();console.log('tests:\n'+tests);
if(!/Lysis_delayed/.test(tests))throw Error('Missing statistical comparison');
console.log('comparison:\n'+(await page.locator('#comparison').innerText()).split('\n').slice(0,6).join('\n'));
await set('#signal','GFP');
await page.locator('#plot_type input[value="segments"]').check();await page.waitForTimeout(2500);
const dl=page.waitForEvent('download');await page.locator('.navbar a[data-value="Export"]').click();await page.locator('#stats_export').click();
const d=await dl;console.log('download:',d.suggestedFilename());
await page.locator('#language').selectOption('en');await page.locator('.navbar a[data-value="Résultats"]').click();await page.waitForTimeout(800);
if(!/Segmented regression options/.test(await page.locator('.segment-options summary').innerText()))throw Error('Missing translation');
if(errors.length)throw Error(errors.join('\n'));console.log('OK');await b.close();})().catch(e=>{console.error(e);process.exit(1)});
