%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
privateDir = [modulepath('tests_manager'), '/functions/+nelson/+unittest/private'];
savedPath = path();
addpath(privateDir);
cleanupPath = onCleanup(@() path(savedPath)); %#ok<NLS0004>

testFile = [fileparts(nfilename('fullpath')), '/test_unittest_runfile_shadowed_time_case.m'];

result = worker_pool_safe_runfile(testFile, true);
asserts.isequal(result.r, 0);
asserts.istrue(result.timing >= 0);

inlineResult = runfile_inline(testFile, 0);
asserts.isequal(inlineResult.r, 0);
asserts.istrue(inlineResult.timing >= 0);

nestedResult = runfile_nested(testFile, 0);
asserts.isequal(nestedResult.r, 0);
asserts.istrue(nestedResult.timing >= 0);
%=============================================================================
