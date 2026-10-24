%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlCaseRows(cases)
  lines = cell(0, 1);
  previousModule = '';
  for k = 1:length(cases)
    outcome = report_caseOutcome(cases(k));
    moduleName = report_htmlModuleNameOrFiles(cases(k));
    kind = report_htmlCaseKind(cases(k));
    duration = report_caseDuration(cases(k));
    className = report_htmlStatusClass(outcome);
    text = lower([report_caseName(cases(k)), ' ', report_htmlCaseModule(cases(k)), ...
      ' ', kind, ' ', report_htmlCaseTags(cases(k))]);
    if ~strcmp(previousModule, moduleName)
      lines{end + 1} = report_htmlModuleRow(moduleName);
      previousModule = moduleName;
    end
    expanded = report_htmlDetailsExpanded(outcome);
    lines{end + 1} = report_htmlCaseRowStart(cases(k), k, className, outcome, text);
    lines{end + 1} = report_htmlCaseIndexCell(k, expanded);
    lines{end + 1} = report_htmlOutcomeCell(outcome, className, kind, duration);
    lines{end + 1} = ['<td>', report_htmlEscape(moduleName), '</td>'];
    lines{end + 1} = ['<td>', report_htmlEscape(report_caseName(cases(k))), '</td>'];
    lines{end + 1} = ['<td>', report_htmlEscape(kind), '</td>'];
    lines{end + 1} = ['<td class="num">', report_durationText(duration), '</td>'];
    lines{end + 1} = ['<td>', report_htmlEscape(report_htmlCaseTags(cases(k))), '</td></tr>'];
    details = report_htmlDetails(cases(k), k, expanded);
    lines = [lines(:); details(:)];
  end
  lines = lines(:);
end
%=============================================================================
function moduleName = report_htmlModuleNameOrFiles(testCase)
  moduleName = report_htmlCaseModule(testCase);
  if isempty(moduleName)
    moduleName = 'files';
  end
end
%=============================================================================
function line = report_htmlModuleRow(moduleName)
  line = ['<tr class="module-row" data-module="', report_htmlEscape(moduleName), ...
    '"><td colspan="7">', report_htmlEscape(moduleName), '</td></tr>'];
end
%=============================================================================
function line = report_htmlCaseRowStart(testCase, index, className, outcome, text)
  line = ['<tr class="case-row ', className, '" data-index="', int2str(index), ...
    '" data-outcome="', report_htmlEscape(outcome), '" data-module="', ...
    report_htmlEscape(report_htmlModuleNameOrFiles(testCase)), '" data-name="', ...
    report_htmlEscape(report_caseName(testCase)), '" data-kind="', ...
    report_htmlEscape(report_htmlCaseKind(testCase)), '" data-duration="', ...
    report_durationText(report_caseDuration(testCase)), '" data-text="', ...
    report_htmlEscape(text), '">'];
end
%=============================================================================
function line = report_htmlCaseIndexCell(index, expanded)
  expandedText = 'false';
  symbol = '+';
  if expanded
    expandedText = 'true';
    symbol = '-';
  end
  line = ['<td><button class="details-toggle" type="button" aria-expanded="', ...
    expandedText, '" aria-controls="details-', int2str(index), ...
    '" data-details-id="details-', int2str(index), '">', symbol, ...
    '</button><span class="case-index">', int2str(index), '</span></td>'];
end
%=============================================================================
function line = report_htmlOutcomeCell(outcome, className, kind, duration)
  label = outcome;
  if strcmp(kind, 'bench') || strcmp(outcome, 'bench')
    label = ['bench ', report_durationText(duration), ' s'];
  end
  line = ['<td><span class="badge ', className, '">', ...
    report_htmlEscape(label), '</span></td>'];
end
%=============================================================================
function expanded = report_htmlDetailsExpanded(outcome)
  expanded = strcmp(outcome, 'failed') || strcmp(outcome, 'timeout') || strcmp(outcome, 'error');
end
%=============================================================================
