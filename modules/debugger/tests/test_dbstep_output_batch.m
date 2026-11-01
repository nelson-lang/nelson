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
% <--ENGLISH IMPOSED-->
%=============================================================================
% Each stop prints "<line>   <source>" for breakpoints, dbstep, dbstep in/out and dbcont.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
filewrite(fullfile(fixtureDirectory, 'outer_batch_fun.m'), ['function r = outer_batch_fun(x)', LF, ...
  '  r = inner_batch_fun(x);', LF, ...
  '  r = r + 1;', LF, ...
  '  disp([''result '', num2str(r)]);', LF, ...
  'end', LF, ...
  'function r = inner_batch_fun(x)', LF, ...
  '  r = x * 2;', LF, ...
  '  r = r + 1;', LF, ...
  'end', LF]);
inputFile = fullfile(fixtureDirectory, 'input.txt');
binpath = modulepath('nelson', 'bin');
nelson_cli_path = [binpath, '/nelson-cli'];
expectedStatus = 3;
executeOption = ' -e ';
extraOptions = ' --timeout 60';
if ~(isfile(nelson_cli_path) || (ispc() && isfile([nelson_cli_path, '.exe'])))
  nelson_cli_path = [binpath, '/nelson-cli-exec'];
  expectedStatus = 0;
  executeOption = ' --execute ';
  extraOptions = '';
end
cli_available = isfile(nelson_cli_path) || (ispc() && isfile([nelson_cli_path, '.exe']));
skip_testsuite(~cli_available, 'Nelson command-line executable is not available.');
nelson_cli = ['"', nelson_cli_path, '"'];
prefix = [nelson_cli, ' --quiet --no-user-startup --no-ipc', extraOptions, executeOption, '"addpath(''', fixtureDirectory, '''); '];
line2 = ['2   ', '  r = inner_batch_fun(x);'];
line3 = ['3   ', '  r = r + 1;'];
line4 = ['4   ', '  disp([''result '', num2str(r)]);'];
line7 = ['7   ', '  r = x * 2;'];
line8 = ['8   ', '  r = r + 1;'];
%=============================================================================
% dbstep shows each line reached
command = [prefix, 'dbstop in outer_batch_fun at 2; y = outer_batch_fun(1); exit(3)"'];
filewrite(inputFile, ['dbstep', LF, 'dbstep', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, line2));
asserts.istrue(contains(output, line3));
asserts.istrue(contains(output, line4));
asserts.istrue(contains(output, 'result 4'));
%=============================================================================
% dbstep in enters the local function, dbstep out comes back to the caller
filewrite(inputFile, ['dbstep in', LF, 'dbstep', LF, 'dbstep out', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, line7));
asserts.istrue(contains(output, line8));
asserts.istrue(contains(output, line3));
asserts.istrue(contains(output, 'result 4'));
%=============================================================================
% a pending step-out is dropped by dbcont: no later stop
command = [prefix, 'dbstop in outer_batch_fun>inner_batch_fun; y = outer_batch_fun(1); z = outer_batch_fun(2); exit(3)"'];
filewrite(inputFile, ['dbstep out', LF, 'dbcont', LF, 'dbcont', LF, 'disp(''should not run'');', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, line7));
asserts.istrue(contains(output, line3));
asserts.istrue(contains(output, 'result 4'));
asserts.istrue(contains(output, 'result 6'));
asserts.isfalse(contains(output, 'should not run'));
%=============================================================================
% dbup then dbcont: the caller scope is restored before resuming
command = [prefix, 'dbstop in outer_batch_fun at 8; y = outer_batch_fun(1); exit(3)"'];
filewrite(inputFile, ['dbup', LF, 'disp([''outer x='', num2str(x)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, line8));
asserts.istrue(contains(output, 'outer x=1'));
asserts.istrue(contains(output, 'result 4'));
filewrite(inputFile, ['dbup', LF, 'dbdown', LF, 'disp([''inner r='', num2str(r)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'inner r=2'));
asserts.istrue(contains(output, 'result 4'));
%=============================================================================
