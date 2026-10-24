%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, raw] = run_runPlan(plan, options)
  raw = create_test_suites();
  beginTime = time();
  internalOptions = run_internalOptions(options);
  versionNumber = version('-number');
  versionText = sprintf('Nelson-%d.%d.%d.%d', versionNumber(1), ...
    versionNumber(2), versionNumber(3), versionNumber(4));
  modules = run_planModuleNames(plan.cases);
  fprintf(stdout, [newline, _('Tests running on %d workers.'), newline], plan.workers);
  for k = 1:length(modules)
    if raw.errors ~= 0 && options.StopOnFail
      break
    end
    moduleName = modules{k};
    run_impl_print_module_header(k, length(modules), moduleName);
    suiteCases = run_planCasesForModule(plan.cases, moduleName);
    testSuite = run_processPlanCases(suiteCases, versionText, moduleName, ...
      plan.workers, internalOptions, options.StopOnFail);
    raw = run_impl_append_suites(raw, testSuite);
  end
  raw.total_time = time() - beginTime;
  status = raw.errors == 0;
end
%=============================================================================
