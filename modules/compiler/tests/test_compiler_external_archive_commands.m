%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'external_command.m');
filewrite(entry, 'function external_command(); disp(''EXTERNAL_COMMAND_OK''); end');
options = nelson.compiler.BuildOptions(entry, 'EmbedArchive', false, ...
  'OutputDir', fullfile(directory, 'bundled'), 'Mode', 'cli');
asserts.istrue(any(strcmp(__nelsonc_option_arguments__(options), '--external-archive')));
bundled = nelson.compiler.build(options);
asserts.isequal(numel(bundled.Files), 3);
asserts.istrue(endsWith(bundled.Files{2}, 'external_command.nca'));
asserts.istrue(isfolder(bundled.Files{3}));
plan = ncc(entry, '-C', '--runtime', 'installed', '--explain-link');
asserts.isfalse(plan.embedArchive);
extension = '';
if ispc(); extension = '.exe'; end
tool = fullfile(modulepath('nelson', 'bin'), ['nelsonc', extension]);
outputFile = fullfile(directory, ['command', extension]);
[status, output] = system(['"', tool, '" "', entry, '" -C --cli --runtime installed -o "', outputFile, '"'], 90);
asserts.isequal(status, 0, output);
asserts.istrue(isfile(fullfile(directory, 'command.nca')));
previous = getenv('NELSONC_RUNTIME_ROOT');
restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
rmfile(entry);
setenv('NELSONC_RUNTIME_ROOT', '');
[status, output] = system(['"', bundled.Executable, '"'], 60);
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'EXTERNAL_COMMAND_OK'));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[status, output] = system(['"', outputFile, '"'], 60);
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'EXTERNAL_COMMAND_OK'));
clear restore;
clear cleanup;
