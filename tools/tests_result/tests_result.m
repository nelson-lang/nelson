%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% display results of .json
try
  TESTS_RESULT_DIR = getenv('TESTS_RESULT_DIR');
  if strcmp(TESTS_RESULT_DIR, '') == true
    TESTS_RESULT_DIR = nelsonroot();
  end
  ver_number = version('-number');
  arch_name = computer('arch');
  platform = 'unknow-platform';
  if strcmp(arch_name, 'win64') == true
    platform = 'win-x86-64';
  end
  if strcmp(arch_name, 'win32') == true
    platform = 'win-x86-32';
  end
  if strcmp(arch_name, 'glnxa64') == true
    platform = 'linux-x86-64';
  end
  if strcmp(arch_name, 'glnxa32') == true
    platform = 'linux-x86-32';
  end
  if strcmp(arch_name, 'maci64') == true
    platform = 'mac-x86-64';
  end
  if strcmp(arch_name, 'maci32') == true
    platform = 'mac-x86-32';
  end
  filedest = [TESTS_RESULT_DIR, filesep, 'tests_all-', mat2str(ver_number(1)), '.', mat2str(ver_number(2)), '.', mat2str(ver_number(3)), '.', mat2str(ver_number(4)), '-', platform, '.json'];
  if isfile(filedest)
    test_suites = jsondecode(fileread(filedest));
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
    disp([_('File does not exist:'), ' ', filedest]);
    r = false;
  end
catch
  m = lasterror();
  disp(m.message);
  r = false;
end
exit();
%=============================================================================
