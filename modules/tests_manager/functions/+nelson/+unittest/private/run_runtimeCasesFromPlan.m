%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [parallelTests, sequentialTests, benchmarkTests] = run_runtimeCasesFromPlan(...
    planCases, versionText, moduleName, runtimeOptions)
  parallelTests = [];
  sequentialTests = [];
  benchmarkTests = [];
  classname = [versionText, '.', moduleName];
  for k = 1:length(planCases)
    parsedTags = run_tagOptionsFromPlanCase(planCases(k));
    test_case = create_test_case(...
      planCases(k).file, classname, runtimeOptions, parsedTags);
    test_case.classname = classname;
    test_case.test_order_index = k;
    if test_case.isbench
      benchmarkTests = [benchmarkTests; test_case];
    elseif needs_sequential_execution(test_case)
      sequentialTests = [sequentialTests; test_case];
    else
      parallelTests = [parallelTests; test_case];
    end
  end
end
%=============================================================================
