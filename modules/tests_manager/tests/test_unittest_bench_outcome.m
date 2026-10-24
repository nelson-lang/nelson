%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
benchFile = [[tempdir(), ''], 'bench_unittest_outcome_', createGUID(), '.m'];
cleanupBench = onCleanup(@() evalc(removeCommand(benchFile))); %#ok<NLS0004>

filewrite(benchFile, {
  'tic();'
  'asserts.istrue(true);'
  'toc();'
  });

results = nelson.unittest.run(benchFile, 'Kind', 'bench', 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(results.summary.passed, 0);
asserts.isequal(results.summary.bench, 1);
asserts.isequal(length(results.cases), 1);
asserts.isequal(results.cases(1).outcome, 'bench');
%=============================================================================
