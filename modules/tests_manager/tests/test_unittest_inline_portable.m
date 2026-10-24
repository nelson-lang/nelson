%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testDirectory = fileparts(mfilename('fullpath'));
fixture = [testDirectory, '/test_portable_inline_fixture.m'];
results = nelson.unittest.run(fixture, 'Workers', 2, 'ExecutionMode', 'inline');

asserts.istrue(results.status);
asserts.isequal(results.summary.tests, 1);
asserts.isequal(results.summary.passed, 1);
asserts.isequal(results.summary.failed, 0);
asserts.isequal(results.registry.workers, 1);
asserts.isequal(results.registry.process_policy, 'inline');
asserts.isequal(results.options.ExecutionMode, 'inline');

if exist('__nelson_unittest_run_processes__', 'builtin') == 0
  asserts.isequal(results.registry.runner_pid, 0);
end

if exist('figure', 'builtin') ~= 0 && exist('findfigs', 'builtin') ~= 0
  identifier = createGUID();
  temporaryDirectory = fullfile(testDirectory, ['temp_inline_graphics_', identifier]);
  mkdir(temporaryDirectory);
  firstFile = fullfile(temporaryDirectory, ['test_inline_graphics_first_', identifier, '.m']);
  secondFile = fullfile(temporaryDirectory, ['test_inline_graphics_second_', identifier, '.m']);
  previousResetGraphics = getenv('NELSON_UNITTEST_RESET_GRAPHICS');
  cleanupCommand = ['setenv(''NELSON_UNITTEST_RESET_GRAPHICS'', ''', ...
    strrep(previousResetGraphics, '''', ''''''), ''');', newline(), ...
    'if isfile(''', strrep(firstFile, '''', ''''''), '''), rmfile(''', ...
    strrep(firstFile, '''', ''''''), '''), end;', newline(), ...
    'if isfile(''', strrep(secondFile, '''', ''''''), '''), rmfile(''', ...
    strrep(secondFile, '''', ''''''), '''), end;', newline(), ...
    'if isfolder(''', strrep(temporaryDirectory, '''', ''''''), ...
    '''), rmdir(''', strrep(temporaryDirectory, '''', ''''''), '''), end;'];
  cleanupPortableFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

  filewrite(firstFile, { ...
    'figure();'; ...
    'asserts.isequal(length(findall(groot(), ''Type'', ''figure'')), 1);'});
  filewrite(secondFile, { ...
    'asserts.istrue(isempty(findall(groot(), ''Type'', ''figure'')));'});
  setenv('NELSON_UNITTEST_RESET_GRAPHICS', '1');
  graphicsResults = nelson.unittest.run({firstFile; secondFile}, ...
    'Workers', 1, 'ExecutionMode', 'inline');
  asserts.istrue(graphicsResults.status);
  asserts.isequal(graphicsResults.summary.passed, 2);

  clear cleanupPortableFiles
end
%=============================================================================
