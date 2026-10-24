%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tests_list = test_run_get_files_list_by_option(tests_dir, option) %#ok<NLS0026>
  tests_list = [];
  switch option %#ok<NLS0008>
    case 'all'
      unitary_tests = getSortedTests(tests_dir, 'test_*.m');
      nonreg_tests = getSortedTests(tests_dir, 'bug_*.m');
      bench_tests = getSortedTests(tests_dir, 'bench_*.m');
      tests_list = toCell([unitary_tests, nonreg_tests, bench_tests]);
    case 'all_tests'
      unitary_tests = getSortedTests(tests_dir, 'test_*.m');
      nonreg_tests = getSortedTests(tests_dir, 'bug_*.m');
      tests_list = toCell([unitary_tests, nonreg_tests]);
    case 'unitary_tests'
      tests_list = toCell(getSortedTests(tests_dir, 'test_*.m'));
    case 'nonreg_tests'
      tests_list = toCell(getSortedTests(tests_dir, 'bug_*.m'));
    case 'benchs'
      tests_list = toCell(getSortedTests(tests_dir, 'bench_*.m'));
    otherwise
      error('Nelson:tests_manager:invalidArgument', 'Argument not managed.');
  end
end
%=============================================================================
function tests = getSortedTests(tests_dir, pattern)
  tests = dir([tests_dir, pattern]);
  if ~isempty(tests)
    tests = sort(string(strcat(tests_dir, {tests.name})));
  else
    tests = string({});
  end
end
%=============================================================================
function tests = toCell(tests)
  if ~isempty(tests)
    tests(tests == "") = [];
    tests = cell(tests);
  else
    tests = {};
  end
end
%=============================================================================
