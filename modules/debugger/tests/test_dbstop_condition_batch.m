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
% Conditional breakpoints at run time: a child nelson-cli reads the K>> commands from a file.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'cond_batch_fun.m'), ['function r = cond_batch_fun(x)', LF, ...
  '  r = x;', LF, ...
  '  for k = 1:5', LF, ...
  '    r = r + k;', LF, ...
  '  end', LF, ...
  '  disp([''result '', num2str(r)]);', LF, ...
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
%=============================================================================
% true only on the third pass
command = [prefix, 'dbstop in cond_batch_fun at 4 if k == 3; y = cond_batch_fun(1); exit(3)"'];
filewrite(inputFile, ['disp([''k is '', num2str(k)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'k is 3'));
asserts.isfalse(contains(output, 'k is 1'));
asserts.isfalse(contains(output, 'k is 4'));
asserts.istrue(contains(output, ['4   ', '    r = r + k;']));
asserts.istrue(contains(output, 'result 16'));
%=============================================================================
% a condition that fails warns and pauses anyway, at every pass
command = [prefix, 'dbstop in cond_batch_fun at 4 if undefined_var > 1; y = cond_batch_fun(1); exit(3)"'];
filewrite(inputFile, repmat(['dbcont', LF], 1, 5));
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'Breakpoint expression ''undefined_var > 1'' resulted in an error. The error was'));
asserts.istrue(contains(output, 'result 16'));
%=============================================================================
% a non-scalar condition warns and pauses anyway
command = [prefix, 'dbstop in cond_batch_fun at 4 if [0 1]; y = cond_batch_fun(1); exit(3)"'];
filewrite(inputFile, repmat(['dbcont', LF], 1, 5));
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'Breakpoint expression ''[0 1]'' resulted in a non-scalar value.'));
asserts.istrue(contains(output, 'result 16'));
%=============================================================================
% a false scalar never pauses: no K>> command is consumed
command = [prefix, 'dbstop in cond_batch_fun at 4 if k > 10; y = cond_batch_fun(1); exit(3)"'];
filewrite(inputFile, ['disp(''should not run'');', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.isfalse(contains(output, 'should not run'));
asserts.isfalse(contains(output, 'Breakpoint expression'));
asserts.istrue(contains(output, 'result 16'));
%=============================================================================
% a numeric scalar is true when non-zero
command = [prefix, 'dbstop in cond_batch_fun at 4 if k - 4; y = cond_batch_fun(1); exit(3)"'];
filewrite(inputFile, repmat(['disp([''pass '', num2str(k)]);', LF, 'dbcont', LF], 1, 4));
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'pass 1'));
asserts.istrue(contains(output, 'pass 5'));
asserts.isfalse(contains(output, 'pass 4'));
asserts.istrue(contains(output, 'result 16'));
%=============================================================================
