%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The <--TIMEOUT N--> tag overrides the per-file default timeout (140 s test,
% 380 s bench). It is parsed like <--WEIGHT N--> but drives the kill deadline
% instead of the scheduling priority.
%=============================================================================
base = [[tempdir(), ''], 'test_unittest_timeout_tag_', createGUID()];
okFile = [base, '_ok.m'];
badFile = [base, '_bad.m'];
dupFile = [base, '_dup.m'];
cleanupOk = onCleanup(@() run_removeFile(okFile)); %#ok<NLS0004>
cleanupBad = onCleanup(@() run_removeFile(badFile)); %#ok<NLS0004>
cleanupDup = onCleanup(@() run_removeFile(dupFile)); %#ok<NLS0004>
%=============================================================================
filewrite(okFile, {'% <--TIMEOUT 300-->'; 'asserts.istrue(true);'});
tags = __nelson_unittest_parsetags__(okFile); %#ok<NLS0004>
asserts.isequal(tags.timeout, 300);
asserts.isequal(tags.weight, 1);
%=============================================================================
% The tagged value flows through discovery into the case timeout.
suite = nelson.unittest.discover(okFile, 'Kind', 'test');
asserts.isequal(suite.cases(1).timeout, 300);
%=============================================================================
% A non-numeric value is rejected.
filewrite(badFile, {'% <--TIMEOUT abc-->'; 'asserts.istrue(true);'});
message = '';
try
  __nelson_unittest_parsetags__(badFile); %#ok<NLS0004>
catch exception
  message = exception.message;
end
asserts.istrue(contains(message, 'TIMEOUT tag expects a positive integer.'));
%=============================================================================
% A duplicated tag is rejected.
filewrite(dupFile, {'% <--TIMEOUT 100-->'; '% <--TIMEOUT 200-->'; ...
  'asserts.istrue(true);'});
message = '';
try
  __nelson_unittest_parsetags__(dupFile); %#ok<NLS0004>
catch exception
  message = exception.message;
end
asserts.istrue(contains(message, 'Duplicated TIMEOUT tag detected.'));
%=============================================================================
