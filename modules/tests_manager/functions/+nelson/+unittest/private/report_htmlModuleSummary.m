%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlModuleSummary(cases)
  summary = report_htmlBuildModuleSummary(cases);
  lines = {'<h2>Module summary</h2>'; '<table class="compact-table">'; ...
    ['<thead><tr><th>Module</th><th>Tests</th><th>Passed</th><th>Failed</th>', ...
    '<th>Skipped</th><th>Bench</th><th>Duration</th></tr></thead>']; '<tbody>'};
  for k = 1:length(summary.modules)
    lines{end + 1} = report_htmlModuleSummaryRow(summary, k);
  end
  lines{end + 1} = '</tbody></table>';
  lines = lines(:);
end
%=============================================================================
function summary = report_htmlBuildModuleSummary(cases)
  summary = report_htmlEmptyModuleSummary();
  for k = 1:length(cases)
    moduleName = report_htmlModuleNameOrFiles(cases(k));
    index = report_htmlModuleIndex(summary.modules, moduleName);
    if index == 0
      summary = report_htmlAppendModule(summary, moduleName);
      index = length(summary.modules);
    end
    summary = report_htmlUpdateModule(summary, index, cases(k));
  end
end
%=============================================================================
function summary = report_htmlEmptyModuleSummary()
  summary.modules = cell(0, 1);
  summary.tests = [];
  summary.passed = [];
  summary.failed = [];
  summary.skipped = [];
  summary.bench = [];
  summary.duration = [];
end
%=============================================================================
function moduleName = report_htmlModuleNameOrFiles(testCase)
  moduleName = report_htmlCaseModule(testCase);
  if isempty(moduleName)
    moduleName = 'files';
  end
end
%=============================================================================
function index = report_htmlModuleIndex(modules, moduleName)
  index = 0;
  for k = 1:length(modules)
    if strcmp(modules{k}, moduleName)
      index = k;
      return
    end
  end
end
%=============================================================================
function summary = report_htmlAppendModule(summary, moduleName)
  summary.modules{end + 1} = moduleName;
  summary.tests(end + 1) = 0;
  summary.passed(end + 1) = 0;
  summary.failed(end + 1) = 0;
  summary.skipped(end + 1) = 0;
  summary.bench(end + 1) = 0;
  summary.duration(end + 1) = 0;
end
%=============================================================================
function summary = report_htmlUpdateModule(summary, index, testCase)
  outcome = report_caseOutcome(testCase);
  if strcmp(report_htmlCaseKind(testCase), 'bench') || strcmp(outcome, 'bench')
    summary.bench(index) = summary.bench(index) + 1;
  else
    summary.tests(index) = summary.tests(index) + 1;
  end
  summary.duration(index) = summary.duration(index) + report_caseDuration(testCase);
  summary = report_htmlUpdateOutcome(summary, index, outcome);
end
%=============================================================================
function summary = report_htmlUpdateOutcome(summary, index, outcome)
  if strcmp(outcome, 'passed')
    summary.passed(index) = summary.passed(index) + 1;
  elseif strcmp(outcome, 'failed') || strcmp(outcome, 'timeout') || strcmp(outcome, 'error')
    summary.failed(index) = summary.failed(index) + 1;
  elseif strcmp(outcome, 'skipped')
    summary.skipped(index) = summary.skipped(index) + 1;
  end
end
%=============================================================================
function line = report_htmlModuleSummaryRow(summary, index)
  line = ['<tr><td>', report_htmlEscape(summary.modules{index}), '</td><td>', ...
    int2str(summary.tests(index)), '</td><td>', int2str(summary.passed(index)), ...
    '</td><td>', int2str(summary.failed(index)), '</td><td>', ...
    int2str(summary.skipped(index)), '</td><td>', int2str(summary.bench(index)), ...
    '</td><td class="num">', report_durationText(summary.duration(index)), '</td></tr>'];
end
%=============================================================================
