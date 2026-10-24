%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testFile = [[tempdir(), ''], 'test_unittest_remote_display_skip_', createGUID(), '.m'];
previousRemoteDisplay = getenv('NELSON_REMOTE_DISPLAY_SESSION');
previousForceDisplay = getenv('NELSON_TESTS_FORCE_DISPLAY');

testFileLiteral = ['''', strrep(testFile, '''', ''''''), ''''];
previousRemoteDisplayLiteral = ['''', strrep(previousRemoteDisplay, '''', ''''''), ''''];
previousForceDisplayLiteral = ['''', strrep(previousForceDisplay, '''', ''''''), ''''];
cleanupCommand = ['setenv(''NELSON_REMOTE_DISPLAY_SESSION'', ', previousRemoteDisplayLiteral, ');', newline, ...
  'setenv(''NELSON_TESTS_FORCE_DISPLAY'', ', previousForceDisplayLiteral, ');', newline, ...
  'if isfile(', testFileLiteral, '), rmfile(', testFileLiteral, '), end'];
cleanupTest = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

filewrite(testFile, {'% <--ADV-CLI MODE-->'; 'asserts.istrue(true);'});
setenv('NELSON_TESTS_FORCE_DISPLAY', '1');
setenv('NELSON_REMOTE_DISPLAY_SESSION', '1');

results = nelson.unittest.run(testFile, 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(results.cases(1).outcome, 'skipped');
%=============================================================================
