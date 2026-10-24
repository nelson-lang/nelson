%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
check_runtime_log();

function check_runtime_log()
  ncc('--help');
  directory = [tempname(), ' log ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  entry = fullfile(directory, 'log_entry.m');
  lines = {'function log_entry(kind)', ...
    'fprintf(1, ''LOG_STDOUT:%s\n'', char([28450, 23383, 233]));', ...
    'fprintf(2, ''LOG_STDERR:%s\n'', char([28450, 23383, 233]));', ...
    'if strcmp(kind, ''fail''); error(''LOG_USER_FAILURE''); end', ...
    'if strcmp(kind, ''large''); fprintf(1, ''%s\n'', repmat(''x'', 1, 150000)); end', ...
    'if strcmp(kind, ''large''); fprintf(1, ''%s\n'', repmat(char([8364, 233]), 1, 18000)); end', ...
    'disp(''LOG_COMPLETED'');', 'end'};
  filewrite(entry, strjoin(lines, newline()));
  options = compiler.build.StandaloneApplicationOptions(entry, ...
    'OutputDir', fullfile(directory, 'console'), 'RuntimeLogFile', 'runtime.log');
  asserts.isequal(options.RuntimeLogFile, 'runtime.log');
  result = compiler.build.standaloneApplication(options);
  options.OutputDir = fullfile(directory, 'repeat');
  again = compiler.build.standaloneApplication(options);
  asserts.isequal(sha256(result.Files{1}, '-file'), sha256(again.Files{1}, '-file'));
  asserts.isequal(fileread(fullfile(result.Options.OutputDir, 'buildresult.json')), ...
    fileread(fullfile(again.Options.OutputDir, 'buildresult.json')));
  applications = {result};
  if ispc()
    options.OutputDir = fullfile(directory, 'windowed');
    applications{end + 1} = compiler.build.standaloneWindowsApplication(options);
  end
  for k = 1:numel(applications)
    current = applications{k};
    log = fullfile(current.Options.OutputDir, 'runtime.log');
    asserts.isfalse(isfile(log));
    filewrite(log, ['EXISTING_LOG', newline()]);
    output = execute(current.Files{1}, 'large', 0);
    text = fileread(log, 'char', 'unix', 'UTF-8');
    asserts.istrue(startsWith(text, 'EXISTING_LOG'));
    for marker = {['LOG_STDOUT:', char([28450, 23383, 233])], ...
      ['LOG_STDERR:', char([28450, 23383, 233])], 'LOG_COMPLETED'}
      asserts.istrue(contains(text, marker{1}), ['Missing log marker: ', marker{1}]);
      asserts.istrue(contains(output, marker{1}), ['Missing output marker: ', marker{1}]);
    end
    % stderr may appear between stdout chunks; all bytes must still be present.
    asserts.isequal(sum(text == 'x'), 150000);
    asserts.isequal(sum(output == 'x'), 150000);
    asserts.isequal(sum(text == char(8364)), 18000);
    asserts.isequal(sum(text == char(233)), 18002);
    asserts.isequal(sum(output == char(8364)), 18000);
    asserts.istrue(contains(text, 'Initializing application runtime.'));
    execute(current.Files{1}, 'fail', 1);
    failedText = fileread(log, 'char', 'unix', 'UTF-8');
    asserts.istrue(startsWith(failedText, text));
    asserts.istrue(contains(failedText, 'LOG_USER_FAILURE'));
  end
  check_paths(entry, directory);
  check_bad_options(entry);
  clear restoreRuntime;
  clear cleanup;
end

function check_paths(entry, directory)
  extension = '';
  if ispc(); extension = '.exe'; end
  app = fullfile(directory, ['absolute', extension]);
  log = fullfile(directory, 'absolute.log');
  ncc(entry, '--cli', '--runtime', 'installed', '--runtime-log-file', log, '-o', app);
  execute(app, 'ok', 0);
  asserts.istrue(contains(fileread(log), 'LOG_COMPLETED'));
  tool = fullfile(modulepath('nelson', 'bin'), ['nelsonc', extension]);
  toolApp = fullfile(directory, ['tool_app', extension]);
  [status, output] = system(['"', tool, '" "', entry, ...
    '" --cli --runtime installed --runtime-log-file tool.log -o "', toolApp, '"'], 90);
  asserts.isequal(status, 0, output);
  execute(toolApp, 'ok', 0);
  asserts.istrue(contains(fileread(fullfile(directory, 'tool.log')), 'LOG_COMPLETED'));
  plain = fullfile(directory, ['plain', extension]);
  ncc(entry, '--cli', '--runtime', 'installed', '-o', plain);
  before = dir(directory);
  execute(plain, 'ok', 0);
  after = dir(directory);
  asserts.isequal(numel(after), numel(before) + 1); % Only the redirected test output.
  for target = {fullfile(directory, 'missing', 'runtime.log'), ...
    fullfile(directory, ['self', extension]), directory}
    destination = fullfile(directory, ['self', extension]);
    if isfile(destination); rmfile(destination); end
    ncc(entry, '--cli', '--runtime', 'installed', '--runtime-log-file', target{1}, '-o', destination);
    digest = sha256(destination, '-file');
    output = execute(destination, 'ok', 2);
    asserts.isfalse(contains(output, 'LOG_COMPLETED'));
    asserts.isequal(sha256(destination, '-file'), digest);
  end
end

function check_bad_options(entry)
  for value = {42, {'log'}, ['bad', char(10), 'log'], ['bad', char(0), 'log']}
    rejected(@() compiler.build.StandaloneApplicationOptions(entry, 'RuntimeLogFile', value{1}));
  end
  if ispc()
    rejected(@() compiler.build.StandaloneApplicationOptions(entry, 'RuntimeLogFile', 'C:log.txt'));
    rejected(@() compiler.build.StandaloneApplicationOptions(entry, 'RuntimeLogFile', '\log.txt'));
  end
  options = compiler.build.StandaloneApplicationOptions(entry, 'RuntimeLogFile', "");
  asserts.isequal(options.RuntimeLogFile, '');
  rejected(@() ncc(entry, '--runtime-log-file'));
end

function output = execute(executable, argument, expected)
  redirected = [executable, '.output'];
  [status, message] = system(['"', executable, '" ', argument, ' >"', redirected, '" 2>&1'], 60);
  asserts.istrue(isfile(redirected), message);
  output = fileread(redirected, 'char', 'unix', 'UTF-8');
  asserts.isequal(status, expected, output);
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
