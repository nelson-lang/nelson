%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlSlowest(cases)
  lines = {'<h2>Slowest tests / benches</h2>'; '<table class="compact-table">'; ...
    '<thead><tr><th>#</th><th>Module</th><th>Name</th><th>Kind</th><th>Seconds</th></tr></thead>'; ...
    '<tbody>'};
  if ~isempty(cases)
    durations = report_htmlDurations(cases);
    limit = min(10, length(durations));
    while limit > 0
      durationMax = max(durations);
      if isinf(durationMax)
        break
      end
      indexes = find(durations == durationMax);
      index = indexes(1);
      lines{end + 1} = report_htmlSlowestRow(cases(index), index);
      durations(index) = -Inf;
      limit = limit - 1;
    end
  end
  lines{end + 1} = '</tbody></table>';
  lines = lines(:);
end
%=============================================================================
function durations = report_htmlDurations(cases)
  durations = zeros(1, length(cases));
  for k = 1:length(cases)
    durations(k) = report_caseDuration(cases(k));
  end
end
%=============================================================================
function line = report_htmlSlowestRow(testCase, index)
  line = ['<tr><td>', int2str(index), '</td><td>', ...
    report_htmlEscape(report_htmlCaseModule(testCase)), '</td><td>', ...
    report_htmlEscape(report_caseName(testCase)), '</td><td>', ...
    report_htmlEscape(report_htmlCaseKind(testCase)), '</td><td class="num">', ...
    report_durationText(report_caseDuration(testCase)), '</td></tr>'];
end
%=============================================================================
