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
% dbstop if error at run time; both child streams go to a log file (system() returns one).
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
filewrite(fullfile(fixtureDirectory, 'err_batch_fun.m'), ['function r = err_batch_fun(x)', LF, ...
  '  r = x + 1;', LF, ...
  '  r = undefined_thing_xyz + r;', LF, ...
  '  r = r * 2;', LF, ...
  'end', LF]);
inputFile = fullfile(fixtureDirectory, 'input.txt');
logFile = fullfile(fixtureDirectory, 'output.log');
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
suffix = ['" < "', inputFile, '" > "', logFile, '" 2>&1'];
stoppedLine = ['3   ', '  r = undefined_thing_xyz + r;'];
errorText = 'Undefined variable: undefined_thing_xyz';
%=============================================================================
% uncaught error: error shown, then the line, then K>>; dbcont ends without a second display
command = [prefix, 'dbstop if error; y = err_batch_fun(1); disp(''not reached''); exit(3)', suffix];
filewrite(inputFile, ['dbstack', LF, 'disp([''r is '', num2str(r)]);', LF, 'dbcont', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.isfalse(status == expectedStatus);
asserts.istrue(contains(output, 'undefined_thing_xyz'));
asserts.istrue(contains(output, stoppedLine));
asserts.istrue(contains(output, 'err_batch_fun (line 3)'));
asserts.istrue(contains(output, 'r is 2'));
asserts.isfalse(contains(output, 'not reached'));
asserts.isequal(numel(strfind(output, stoppedLine)), 1);
asserts.isequal(numel(strfind(output, errorText)), 1);
%=============================================================================
% dbquit and dbstep also end the session: no resume after an error
filewrite(inputFile, ['dbquit', LF, 'disp(''still paused'');', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.isfalse(status == expectedStatus);
asserts.istrue(contains(output, stoppedLine));
asserts.isfalse(contains(output, 'still paused'));
asserts.isfalse(contains(output, 'not reached'));
asserts.isequal(numel(strfind(output, errorText)), 1);
filewrite(inputFile, ['dbstep', LF, 'disp(''still paused'');', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.isfalse(status == expectedStatus);
asserts.istrue(contains(output, stoppedLine));
asserts.isfalse(contains(output, 'still paused'));
asserts.isfalse(contains(output, 'not reached'));
%=============================================================================
% dbstop if error does not stop on an error caught by try/catch
command = [prefix, 'dbstop if error; try; err_batch_fun(1); catch e; disp([''caught '', e.message]); end; disp(''after''); exit(3)', suffix];
filewrite(inputFile, ['disp(''should not run'');', LF, 'dbcont', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'caught '));
asserts.istrue(contains(output, 'after'));
asserts.isfalse(contains(output, 'should not run'));
asserts.isfalse(contains(output, stoppedLine));
%=============================================================================
% dbstop if caught error stops before the catch runs; dbcont resumes in it
command = [prefix, 'dbstop if caught error; try; err_batch_fun(1); catch e; disp([''caught '', e.message]); end; disp(''after''); exit(3)', suffix];
filewrite(inputFile, ['disp([''r is '', num2str(r)]);', LF, 'dbcont', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.isequal(status, expectedStatus);
asserts.istrue(contains(output, 'Caught-error breakpoint was hit in err_batch_fun at line 3. The error was:'));
asserts.istrue(contains(output, stoppedLine));
asserts.istrue(contains(output, 'r is 2'));
asserts.istrue(contains(output, 'caught '));
asserts.istrue(contains(output, 'after'));
%=============================================================================
% an error typed at the K>> prompt does not open a nested error stop
command = [prefix, 'dbstop if error; dbstop in err_batch_fun at 2; y = err_batch_fun(1); exit(3)', suffix];
filewrite(inputFile, ['undefined_at_prompt + 1', LF, 'disp(''prompt alive'');', LF, 'dbquit', LF]);
status = system(command, 90);
output = fileread(logFile);
asserts.istrue(contains(output, 'undefined_at_prompt'));
asserts.istrue(contains(output, 'prompt alive'));
%=============================================================================
