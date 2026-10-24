%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
%=============================================================================
% `keyboard` enters the debug prompt (K>>) and reads its commands from the
% standard input: a child nelson-cli is driven with a redirected input file.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
filewrite(fullfile(fixtureDirectory, 'keyboard_pause.m'), ['function r = keyboard_pause(x)', LF, ...
  '  r = x + 1;', LF, ...
  '  disp(''before keyboard'');', LF, ...
  '  keyboard', LF, ...
  '  r = r * 2;', LF, ...
  '  disp(''after keyboard'');', LF, ...
  'end', LF]);
[nelson_cli, executeOption] = nelson_cli_command_for_test();
expectedDbcontStatus = 3;
extraOptions = ' --timeout 60';
if contains(nelson_cli, 'nelson-cli-exec')
  expectedDbcontStatus = 0;
  extraOptions = '';
end
command = [nelson_cli, ' --no-user-startup --no-ipc', extraOptions, executeOption, '"addpath(''', ...
  fixtureDirectory, '''); y = keyboard_pause(1); disp([''result '', num2str(y)]); exit(3)"'];
inputFile = fullfile(fixtureDirectory, 'input.txt');
%=============================================================================
% dbcont resumes the function; commands typed at K>> run in its workspace.
filewrite(inputFile, ['r = r + 100;', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedDbcontStatus);
asserts.istrue(contains(output, 'before keyboard'));
asserts.istrue(contains(output, 'after keyboard'));
asserts.istrue(contains(output, 'result 204'));
%=============================================================================
% dbstep resumes from the keyboard line and pauses on the next statement.
filewrite(inputFile, ['dbstack', LF, 'dbstep', LF, 'dbstack', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedDbcontStatus);
asserts.istrue(contains(output, 'function keyboard_pause (line 4)'));
asserts.istrue(contains(output, 'function keyboard_pause (line 5)'));
asserts.istrue(contains(output, 'result 4'));
%=============================================================================
% dbquit stops the paused execution (the -e command is aborted, then the
% session ends): the K>> line after dbquit is never read.
filewrite(inputFile, ['dbquit', LF, 'disp(''still paused''); exit(4)', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'before keyboard'));
asserts.isfalse(contains(output, 'still paused'));
asserts.isfalse(contains(output, 'after keyboard'));
asserts.isfalse(contains(output, 'result'));
%=============================================================================
% End of input at K>> ends the session, as it does at the regular prompt.
filewrite(inputFile, '');
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'before keyboard'));
asserts.isfalse(contains(output, 'after keyboard'));
%=============================================================================
