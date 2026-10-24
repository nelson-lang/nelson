%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlSummary(results, cases)
  summary = struct();
  if isfield(results, 'summary')
    summary = results.summary;
  end
  total = length(cases);
  items = {
    'Tests', total
    'Passed', report_summaryValue(summary, 'passed')
    'Failed', report_summaryValue(summary, 'failed')
    'Skipped', report_summaryValue(summary, 'skipped')
    'Timeout', report_summaryValue(summary, 'timeout')
    'Error', report_summaryValue(summary, 'error')
    'Bench', report_summaryValue(summary, 'bench')
    'Duration', [report_durationText(report_summaryValue(summary, 'duration')), ' s']
  };
  lines = {'<section class="summary" aria-label="Summary">'};
  for k = 1:size(items, 1)
    lines{end + 1} = ['<div class="metric"><span>', items{k, 1}, ...
      '</span><strong>', report_htmlEscape(report_htmlValueText(items{k, 2})), ...
      '</strong></div>'];
  end
  lines{end + 1} = '</section>';
  lines = lines(:);
end
%=============================================================================
function txt = report_htmlValueText(value)
  if isnumeric(value)
    txt = num2str(value);
  else
    txt = char(value);
  end
end
%=============================================================================
