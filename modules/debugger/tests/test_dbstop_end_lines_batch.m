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
% The `end` of a loop pauses at every iteration and the `end` of a function before returning.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'end_batch_fun.m'), ['function r = end_batch_fun(n)', LF, ...
  '  r = 0;', LF, ...
  '  for k = 1:n', LF, ...
  '    r = r + k;', LF, ...
  '  end', LF, ...
  '  m = 0;', LF, ...
  '  while m < 2', LF, ...
  '    m = m + 1;', LF, ...
  '  end', LF, ...
  '  disp([''result '', num2str(r + m)]);', LF, ...
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
% end of a for loop: once per iteration
command = [prefix, 'dbstop in end_batch_fun at 5; y = end_batch_fun(2); exit(3)"'];
filewrite(inputFile, ['disp([''end k='', num2str(k)]);', LF, 'dbcont', LF, 'disp([''end k='', num2str(k)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, ['5   ', '  end']));
asserts.istrue(contains(output, 'end k=1'));
asserts.istrue(contains(output, 'end k=2'));
asserts.istrue(contains(output, 'result 5'));
%=============================================================================
% end of a while loop: once per iteration
command = [prefix, 'dbstop in end_batch_fun at 9; y = end_batch_fun(2); exit(3)"'];
filewrite(inputFile, ['disp([''end m='', num2str(m)]);', LF, 'dbcont', LF, 'disp([''end m='', num2str(m)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, ['9   ', '  end']));
asserts.istrue(contains(output, 'end m=1'));
asserts.istrue(contains(output, 'end m=2'));
asserts.istrue(contains(output, 'result 5'));
%=============================================================================
% end of the function: once, before returning
command = [prefix, 'dbstop in end_batch_fun at 11; y = end_batch_fun(2); exit(3)"'];
filewrite(inputFile, ['disp([''final r='', num2str(r)]);', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, ['11  ', 'end']));
asserts.istrue(contains(output, 'final r=3'));
asserts.istrue(contains(output, 'result 5'));
%=============================================================================
% stepping from the last body line reaches the loop end, then the loop header
command = [prefix, 'dbstop in end_batch_fun at 4; y = end_batch_fun(1); exit(3)"'];
filewrite(inputFile, ['dbstep', LF, 'dbstep', LF, 'dbcont', LF]);
[status, output] = system([command, ' < "', inputFile, '"'], 90);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, ['4   ', '    r = r + k;']));
asserts.istrue(contains(output, ['5   ', '  end']));
asserts.istrue(contains(output, ['6   ', '  m = 0;']));
asserts.istrue(contains(output, 'result 3'));
%=============================================================================
