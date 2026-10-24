%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_run_disp_summary(test_suites)
  useEmoji = isunicodesupported();
  fprintf(stdout, newline);
  fprintf(stdout, ['  ===============================================================================', newline]);
  fprintf(stdout, ['  ', _('Summary:'), newline]);
  fprintf(stdout, ['  ', _('Tests:'), '   ', int2str(test_suites.tests), newline]);
  if useEmoji
    fprintf(stdout, ['  ', _('Passed:'), '  ', getStatusCharacter('Pass'), ' ', int2str(test_suites.passed), newline]);
    fprintf(stdout, ['  ', _('Failed:'), '  ', getStatusCharacter('Fail'), ' ', int2str(test_suites.errors), newline]);
    fprintf(stdout, ['  ', _('Skipped:'), ' ', getStatusCharacter('Skip'), ' ', int2str(test_suites.disabled), newline]);
  else
    fprintf(stdout, ['  ', _('Passed:'), '  ', int2str(test_suites.passed), newline]);
    fprintf(stdout, ['  ', _('Failed:'), '  ', int2str(test_suites.errors), newline]);
    fprintf(stdout, ['  ', _('Skipped:'), ' ', int2str(test_suites.disabled), newline]);
  end
  fprintf(stdout, ['  ', _('Benchs:'), '  ', int2str(test_suites.bench), newline]);
  dispTime('Tests time', test_suites.time);
  dispTime('Total time', test_suites.total_time);
  fprintf(stdout, ['  ================================================================================', newline]);
  fprintf(stdout, newline);
end
%=============================================================================
function dispTime(label, value)
  minutes = value / 60;
  if minutes >= 60
    h = floor(minutes / 60);
    m = round(minutes - h * 60);
    fprintf(stdout, ['  ', _(label), ': ', int2str(h), ' h ', int2str(m), ' min', newline]);
  elseif value > 100
    fprintf(stdout, ['  ', _(label), ': ', sprintf('%.1f', minutes), ' min', newline]);
  else
    fprintf(stdout, ['  ', _(label), ': ', sprintf('%.1f', value), ' s', newline]);
  end
end
%=============================================================================
