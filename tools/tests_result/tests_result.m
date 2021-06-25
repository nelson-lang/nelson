%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
