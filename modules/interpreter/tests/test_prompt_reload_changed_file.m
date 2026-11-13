%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% A command typed at the prompt runs a function file changed on disk since its last call.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Piping prompt commands to a child process is not available in the portable runtime.');
[nelson_exe, ~] = nelson_cli_command_for_test();
%=============================================================================
T = tempname();
mkdir(T);
mkdir(fullfile(T, 'v2'));
filewrite(fullfile(T, 'prompt_reload_probe.m'), {'function r = prompt_reload_probe()'; 'r = 1;'; 'end'});
filewrite(fullfile(T, 'v2', 'prompt_reload_probe.m'), {'function r = prompt_reload_probe()'; 'r = 33;'; 'end'});
filewrite(fullfile(T, 'prompt_reload_script.m'), {'prompt_reload_value = 1;'});
src = strrep(fullfile(T, 'v2', 'prompt_reload_probe.m'), '''', '''''');
dst = strrep(fullfile(T, 'prompt_reload_probe.m'), '''', '''''');
scriptFile = strrep(fullfile(T, 'prompt_reload_script.m'), '''', '''''');
commands = {['addpath(''', strrep(T, '''', ''''''), ''');'];
  'fprintf(''RESULT A %d\n'', prompt_reload_probe());';
  'prompt_reload_script; fprintf(''RESULT B %d\n'', prompt_reload_value);';
  ['copyfile(''', src, ''', ''', dst, ''', ''f'');'];
  ['filewrite(''', scriptFile, ''', ''prompt_reload_value = 33;'');'];
  'fprintf(''RESULT C %d\n'', prompt_reload_probe());';
  'prompt_reload_script; fprintf(''RESULT D %d\n'', prompt_reload_value);';
  'exit'};
inputFile = fullfile(T, 'commands.txt');
filewrite(inputFile, commands);
[status, msg] = system([nelson_exe, ' -q < "', inputFile, '"'], 120);
asserts.isequal(status, 0);
lines = strtrim(strsplit(msg, newline));
results = lines(startsWith(lines, 'RESULT') | contains(lines, '>> RESULT'));
results = regexprep(results, '^.*RESULT', 'RESULT');
asserts.isequal(results, {'RESULT A 1'; 'RESULT B 1'; 'RESULT C 33'; 'RESULT D 33'}');
%=============================================================================
% without the file watcher, only rehash reloads the changed file
filewrite(fullfile(T, 'prompt_reload_probe.m'), {'function r = prompt_reload_probe()'; 'r = 1;'; 'end'});
commands = {['addpath(''', strrep(T, '''', ''''''), ''');'];
  'fprintf(''RESULT A %d\n'', prompt_reload_probe());';
  ['copyfile(''', src, ''', ''', dst, ''', ''f'');'];
  'fprintf(''RESULT B %d\n'', prompt_reload_probe());';
  'rehash path';
  'fprintf(''RESULT C %d\n'', prompt_reload_probe());';
  'exit'};
filewrite(inputFile, commands);
[status, msg] = system([nelson_exe, ' -q --without-file-watcher < "', inputFile, '"'], 120);
asserts.isequal(status, 0);
lines = strtrim(strsplit(msg, newline));
results = lines(startsWith(lines, 'RESULT') | contains(lines, '>> RESULT'));
results = regexprep(results, '^.*RESULT', 'RESULT');
asserts.isequal(results, {'RESULT A 1'; 'RESULT B 1'; 'RESULT C 33'}');
%=============================================================================
rmdir(T, 's');
%=============================================================================
