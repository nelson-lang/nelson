%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'options_entry.m');
filewrite(entry, 'function options_entry(); disp(''OPTIONS_ENTRY''); end');
extension = '';
if ispc()
  extension = '.exe';
end
output = fullfile(directory, ['rejected', extension]);
for options = {[], 1, {true}, struct('unknown', true), repmat(struct('autoDetectDataFiles', true), 1, 2)}
  asserts.checkerror('__nelsonc_build__(entry, output, {}, false, ''installed'', options{1})', 'Invalid application build options.');
  asserts.isfalse(isfile(output));
end
for value = {0, 1, [], 'off', [true false]}
  options = struct('autoDetectDataFiles', value{1});
  asserts.checkerror('__nelsonc_build__(entry, output, {}, false, ''installed'', options)', 'Automatic data detection must be a logical scalar.');
  asserts.isfalse(isfile(output));
end
for value = {true, char([1 2; 3 4]), char(0)}
  options = struct('helpText', value{1});
  asserts.checkerror('__nelsonc_build__(entry, output, {}, false, ''installed'', options)', 'Invalid application help text.');
  asserts.isfalse(isfile(output));
end
legacy = fullfile(directory, ['legacy', extension]);
for value = {0, 1, [], 'off', [true false]}
  options = struct('noConsole', value{1});
  asserts.checkerror('__nelsonc_build__(entry, output, {}, false, ''installed'', options)', 'No-console mode must be a logical scalar.');
  asserts.isfalse(isfile(output));
end
__nelsonc_build__(entry, legacy, {}, false, 'installed', 'LEGACY_BUILD_HELP');
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreEnvironment = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[status, text] = system(['"', legacy, '" --help 2>&1'], 60);
asserts.isequal(status, 0, text);
asserts.istrue(contains(text, 'LEGACY_BUILD_HELP'), text);
asserts.isfalse(contains(text, 'OPTIONS_ENTRY'), text);
asserts.istrue(contains(ncc('--help'), '--no-auto-data'));
asserts.istrue(contains(ncc('--help'), '-X'));
asserts.istrue(contains(ncc('--help'), '--no-console'));
clear restoreEnvironment cleanup;
