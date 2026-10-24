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
% dbcont at a breakpoint resumes execution: a child nelson-cli reads the
% K>> commands from a redirected input file.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
filewrite(fullfile(fixtureDirectory, 'dbcont_batch_fun.m'), ['function r = dbcont_batch_fun(x)', LF, ...
  '  r = x + 1;', LF, ...
  '  disp(''before breakpoint'');', LF, ...
  '  r = r * 2;', LF, ...
  '  disp(''after breakpoint'');', LF, ...
  'end', LF]);
inputFile = fullfile(fixtureDirectory, 'input.txt');
filewrite(inputFile, ['r = r + 100;', LF, 'dbcont', LF]);
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
command = [nelson_cli, ' --quiet --no-user-startup --no-ipc', extraOptions, executeOption, '"addpath(''', ...
  fixtureDirectory, '''); dbstop in dbcont_batch_fun at 4; y = dbcont_batch_fun(1); disp([''result '', num2str(y)]); exit(3)"'];
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'before breakpoint'));
asserts.istrue(contains(output, 'after breakpoint'));
asserts.istrue(contains(output, 'result 204'));
%=============================================================================
