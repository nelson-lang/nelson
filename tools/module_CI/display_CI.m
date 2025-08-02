%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
MODULE_NAME = getenv('MODULE_NAME');
DESTINATION_DIRECTORY = getenv('DESTINATION_DIRECTORY');
arch_name = computer('arch');
%=============================================================================
test_results = [DESTINATION_DIRECTORY ,'/tests-', MODULE_NAME, '-', arch_name, '.json'];
% display results of .json
try
  if isfile(test_results)
    test_suites = jsondecode(fileread(test_results));
    fprintf(stdout, ['  %==========================================================================', newline]);
    fprintf(stdout, ['  ', _('Summary:'), newline]);
    fprintf(stdout, ['  ', _('Tests:'), ' ', int2str(test_suites.tests), newline]);
    fprintf(stdout, ['  ', _('Passed:'), ' ', int2str(test_suites.passed), newline]);
    fprintf(stdout, ['  ', _('Failed:'), ' ', int2str(test_suites.errors), newline]);
    fprintf(stdout, ['  ', _('Skipped:'), ' ',  int2str(test_suites.disabled), newline]);
    fprintf(stdout, ['  ', _('Benchs:'), ' ', int2str(test_suites.bench), newline]);
    if test_suites.time > 100
      fprintf(stdout, ['  ', _('Tests time:'), ' ', mat2str(test_suites.time * inv(60), 2), ' min', newline]);
    else
      if test_suites.time < 1e-3
        fprintf(stdout, ['  ', _('Tests time:'), ' ', mat2str(test_suites.time * 1e3, 2), ' ms', newline]);
      else
        fprintf(stdout, ['  ', _('Tests time:'), ' ', mat2str(test_suites.time, 2), ' s', newline]);
      end
    end
    if test_suites.total_time > 100
      fprintf(stdout, ['  ', _('Total time:'), ' ', mat2str(test_suites.total_time * inv(60), 2), ' min', newline]);
    else
      if test_suites.total_time < 1e-3
        fprintf(stdout, ['  ', _('Total time:'), ' ', mat2str(test_suites.total_time * 1e3, 2), ' ms', newline]);
      else
        fprintf(stdout, ['  ', _('Total time:'), ' ', mat2str(test_suites.total_time, 2), ' s', newline]);
      end
    end
    fprintf(stdout, ['  %===========================================================================', newline]);
    r = true;
  else
    disp([_('File does not exist:'), ' ', test_results]);
    r = false;
  end
catch
  m = lasterror();
  disp(m.message);
  r = false;
end
exit();
%=============================================================================
