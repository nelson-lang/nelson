%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tuneWeights_display(report)
  disp(_('Test weight proposal:'));
  displayed = 0;
  for k = 1:length(report.cases)
    testCase = report.cases(k);
    if strcmp(testCase.action, 'unchanged')
      continue
    end
    label = testCase.name;
    if ~isempty(testCase.module)
      label = [testCase.module, '/', label];
    end
    fprintf('  %-48s [%8.3fs] %4d -> %-4d %s\n', label, testCase.duration, ...
      testCase.old_weight, testCase.new_weight, testCase.action);
    displayed = displayed + 1;
  end
  if displayed == 0
    disp(['  ', _('No weight change proposed.')]);
  end
  summary = report.summary;
  fprintf(_('Eligible: %d, add: %d, update: %d, remove: %d, unchanged: %d, written: %d.\n'), ...
    summary.eligible, summary.added, summary.updated, summary.removed, ...
    summary.unchanged, summary.written);
end
%=============================================================================
