%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tuneReuse_display(report)
  disp(_('Process-reuse proposal:'));
  displayed = 0;
  for k = 1:length(report.cases)
    testCase = report.cases(k);
    if strcmp(testCase.action, 'unchanged') && strcmp(testCase.reason, 'certified')
      continue
    end
    label = testCase.name;
    if ~isempty(testCase.module)
      label = [testCase.module, '/', label];
    end
    fprintf('  %-44s [%7.2f%%] %-9s %s\n', label, ...
      testCase.gain * 100, testCase.action, testCase.reason);
    displayed = displayed + 1;
  end
  if displayed == 0
    disp(['  ', _('No process-reuse change proposed.')]);
  end
  summary = report.summary;
  fprintf(_('Selected: %d, eligible: %d, calibrated: %d, excluded: %d, deferred: %d.\n'), ...
    summary.selected, summary.eligible, summary.calibrated, ...
    summary.excluded, summary.deferred);
  fprintf(_('Add: %d, remove: %d, unchanged: %d, written: %d.\n'), ...
    summary.added, summary.removed, summary.unchanged, summary.written);
end
%=============================================================================
