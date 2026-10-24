%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_if_no_native_unittest_runner();
%=============================================================================
inlineBase = [[tempdir(), ''], 'test_unittest_worker_pool_inline_failure_', createGUID()];
inlineFiles = {[inlineBase, '_one.m']; [inlineBase, '_missing.m']; [inlineBase, '_three.m']};
filewrite(inlineFiles{1}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});
filewrite(inlineFiles{3}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});

runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0, ...
  'WorkerPoolInline', true);
inlineCases = [initialize_test_case(inlineFiles{1}, '', runtimeOptions); ...
  initialize_test_case(inlineFiles{1}, '', runtimeOptions); ...
  initialize_test_case(inlineFiles{3}, '', runtimeOptions)];
inlineCases(2).filename = inlineFiles{2};
inlineCases(2).name = 'test_unittest_worker_pool_inline_missing';
inlineCases = annotate_worker_pool_eligibility(inlineCases);
inlineResults = process_test_cases_pooled_raw(inlineCases, 1, runtimeOptions);
asserts.isequal(length(inlineResults), 3);
asserts.isequal(inlineResults(1).status, 'Pass');
asserts.isequal(inlineResults(2).status, 'Fail');
asserts.isequal(inlineResults(3).status, 'Pass');
run_removeFile(inlineFiles{1});
run_removeFile(inlineFiles{3});
%=============================================================================
