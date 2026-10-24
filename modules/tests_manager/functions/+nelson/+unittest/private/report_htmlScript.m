%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlScript(results)
  embeddedJson = report_htmlEscape(jsonencode(results));
  lines = {
    ['<script type="application/json" id="nelson-report-data">', embeddedJson, '</script>']
    '<script>'
    'const q=document.getElementById("q");const o=document.getElementById("outcome");let quick="";'
    'const tbody=document.querySelector("tbody");'
    'function caseRows(){return Array.from(document.querySelectorAll(".case-row"));}'
    'function detailsFor(row){const next=row.nextElementSibling;'
    'return next&&next.classList.contains("details-row")?next:null;}'
    'function clearModuleRows(){document.querySelectorAll(".module-row").forEach(row=>row.remove());}'
    'function addModuleRow(before,module){const tr=document.createElement("tr");tr.className="module-row";'
    'tr.dataset.module=module;const td=document.createElement("td");td.colSpan=7;'
    'td.textContent=module;tr.appendChild(td);'
    'before.parentNode.insertBefore(tr,before);}'
    'function quickMatch(row){if(!quick)return true;const outcome=row.dataset.outcome;'
    'if(quick==="failed")return outcome==="failed"||outcome==="timeout"||outcome==="error";'
    'if(quick==="skipped")return outcome==="skipped";if(quick==="bench")return outcome==="bench";'
    'if(quick==="slow")return Number(row.dataset.duration)>1;return true;}'
    'function rowMatches(row){const query=q.value.toLowerCase();const outcome=o.value;'
    'const okText=!query||row.dataset.text.includes(query);const okOutcome=!outcome||row.dataset.outcome===outcome;'
    'return okText&&okOutcome&&quickMatch(row);}'
    'function setVisible(row,show){row.classList.toggle("hidden",!show);const details=detailsFor(row);'
    'const toggle=row.querySelector(".details-toggle");'
    'const expanded=toggle&&toggle.getAttribute("aria-expanded")==="true";'
    'if(details)details.classList.toggle("hidden",!show||!expanded);}'
    'function applyFilter(){'
    'clearModuleRows();let currentModule="";caseRows().forEach(row=>{'
    'const show=rowMatches(row);setVisible(row,show);'
    'const module=row.dataset.module||"files";'
    'if(show&&module!==currentModule){addModuleRow(row,module);currentModule=module;}'
    '});}'
    'function sortRows(key,direction){const rows=caseRows();rows.sort((a,b)=>{'
    'let av=a.dataset[key]||"";let bv=b.dataset[key]||"";'
    'if(key==="duration"||key==="index"){av=Number(av);bv=Number(bv);return direction==="asc"?av-bv:bv-av;}'
    'return direction==="asc"?av.localeCompare(bv):bv.localeCompare(av);});'
    'clearModuleRows();rows.forEach(row=>{const details=detailsFor(row);'
    'tbody.appendChild(row);if(details)tbody.appendChild(details);});'
    'applyFilter();}'
    'document.querySelectorAll(".details-toggle").forEach(button=>{'
    'button.addEventListener("click",()=>{'
    'const details=document.getElementById(button.dataset.detailsId);'
    'const expanded=button.getAttribute("aria-expanded")==="true";'
    'button.setAttribute("aria-expanded",String(!expanded));'
    'button.textContent=expanded?"+":"-";'
    'if(details)details.classList.toggle("hidden",expanded);'
    '});});'
    'document.querySelectorAll("th[data-sort]").forEach(header=>{header.addEventListener("click",()=>{'
    'const key=header.dataset.sort;let direction=header.dataset.direction;'
    'direction=direction==="asc"?"desc":(key==="duration"?"desc":"asc");'
    'document.querySelectorAll("th[data-sort]").forEach(h=>h.removeAttribute("data-direction"));'
    'header.dataset.direction=direction;sortRows(key,direction);});});'
    'document.querySelectorAll(".quick-filter").forEach(button=>{button.addEventListener("click",()=>{'
    'const value=button.dataset.quick;quick=quick===value?"":value;o.value="";'
    'document.querySelectorAll(".quick-filter").forEach(b=>'
    'b.classList.toggle("active",b.dataset.quick===quick));'
    'applyFilter();});});'
    'q.addEventListener("input",applyFilter);o.addEventListener("change",applyFilter);'
    'applyFilter();'
    '</script>'
  };
  lines = lines(:);
end
%=============================================================================
