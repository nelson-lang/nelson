%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()) || isempty(which('getdynlibext')), ...
  'Native dynamic-library application startup test is not available.');
gateway = fullfile(modulepath('nelson', 'builtin'), ['libnlsCompiler_builtin', getdynlibext()]);
if ~isfile(gateway)
  error('Nelson:tests_manager:testsuite_skipped', 'Optional compiler module is not built.');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreEnvironment = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
entry = fullfile(directory, 'startup_entry.m');
filewrite(entry, 'function startup_entry(); disp(''STARTUP_ENTRY_EXECUTED''); end');
extension = '';
if ispc()
  extension = '.exe';
end
applications = {fullfile(directory, ['bundled', extension]), fullfile(directory, ['installed', extension])};
for k = 1:numel(applications)
  modes = {'bundled', 'installed'};
  result = ncc(entry, '--cli', '--runtime', modes{k}, '-o', applications{k});
end
runtime = fullfile(directory, 'bundled.runtime');
setenv('NELSONC_RUNTIME_ROOT', runtime);
startup = fullfile(runtime, 'etc', 'startup.m');
finish = fullfile(runtime, 'etc', 'finish.m');
originalStartup = fileread(startup);
originalFinish = fileread(finish);
for k = 1:numel(applications)
  assert_application_status(applications{k}, 0, true);
end

% A late startup error must prevent entry even when shutdown requests success.
filewrite(startup, [originalStartup, newline(), 'error(''Nelson:engine:startupTest'', ''deliberate startup failure'');']);
filewrite(finish, [originalFinish, newline(), 'exit(0);']);
for k = 1:numel(applications)
  output = assert_application_status(applications{k}, 1, false);
  asserts.istrue(contains(output, 'deliberate startup failure'));
end
filewrite(finish, originalFinish);
filewrite(startup, originalStartup);

movefile(startup, [startup, '.saved']);
for k = 1:numel(applications)
  assert_application_status(applications{k}, 1, false);
end
movefile([startup, '.saved'], startup);

% Only the generated runtime is changed, never the host's loaded library.
% The generated runtime stores gateways under bin/<platform>/ where <platform>
% is the real platform name (e.g. "macOS"). Derive it from the runtime itself:
% the host's modulepath('nelson','builtin') basename is "macOS" in a local build
% but "Nelson" in an installed macOS bundle (builtins live in lib/Nelson), so it
% cannot be used to locate the runtime's gateway.
platform = '';
binEntries = dir(fullfile(runtime, 'bin'));
for idx = 1:numel(binEntries)
  entryName = binEntries(idx).name;
  if binEntries(idx).isdir && ~strcmp(entryName, '.') && ~strcmp(entryName, '..')
    platform = entryName;
    break;
  end
end
missingGateway = fullfile(runtime, 'bin', platform, ['libnlsInterpreter_builtin', getdynlibext()]);
movefile(missingGateway, [missingGateway, '.saved']);
for k = 1:numel(applications)
  output = assert_application_status(applications{k}, 1, false);
  asserts.istrue(contains(output, 'libnlsInterpreter_builtin'));
end
movefile([missingGateway, '.saved'], missingGateway);

for code = [0, 7]
  filewrite(startup, [originalStartup, newline(), sprintf('exit(%d);', code)]);
  for k = 1:numel(applications)
    assert_application_status(applications{k}, code, false);
  end
end
filewrite(startup, originalStartup);
for k = 1:numel(applications)
  assert_application_status(applications{k}, 0, true);
end
clear restoreEnvironment cleanup;

function output = assert_application_status(application, expected, entered)
  [status, output] = system(['"', application, '" 2>&1'], 60);
  if status ~= expected
    disp(output);
  end
  asserts.isequal(status, expected);
  asserts.isequal(contains(output, 'STARTUP_ENTRY_EXECUTED'), entered);
end
