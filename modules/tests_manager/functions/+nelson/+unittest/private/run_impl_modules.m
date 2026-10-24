%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suites = run_impl_modules(...
    modules_list, option, nbWorkers, runtimeOptions, stoponfail, nelson_version_str)
  if isempty(modules_list)
    error('Nelson:tests_manager:noValidModule', 'No valid module.');
  end
  test_suites = create_test_suites();
  nbTestSuites = length(modules_list);
  fprintf(stdout, [newline, _('Tests running on %d workers.'), newline], nbWorkers);
  for k = 1:nbTestSuites
    if test_suites.errors ~= 0 && stoponfail
      return
    end
    moduleName = modules_list{k};
    run_impl_print_module_header(k, nbTestSuites, moduleName);
    test_suite = run_impl_module(...
      moduleName, option, nbWorkers, runtimeOptions, stoponfail, nelson_version_str);
    test_suites = run_impl_append_suites(test_suites, test_suite);
  end
end
%=============================================================================
