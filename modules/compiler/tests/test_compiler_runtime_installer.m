%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 600-->
% <--SEQUENTIAL TEST REQUIRED-->
ncc('--help');
if ~ispc()
  return;
end
__nelsonc_inno_engine__();
try
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
mkdir(source);
firstEntry = fullfile(source, 'shared_a.m');
filewrite(firstEntry, 'function shared_a(); if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; disp(''SHARED_A_OK''); end');
first = compiler.build.standaloneApplication(firstEntry, 'OutputDir', fullfile(directory, 'a'));
secondEntry = fullfile(source, 'shared_b.m');
filewrite(secondEntry, 'function shared_b(); if sin(0) ~= 0; error(''Invalid result.''); end; disp(''SHARED_B_OK''); end');
second = compiler.build.standaloneWindowsApplication(secondEntry, 'OutputDir', fullfile(directory, 'b'));
reports = {fullfile(first.Options.OutputDir, 'buildresult.json'), fullfile(second.Options.OutputDir, 'buildresult.json')};
expected = __nelsonc_distribution_plan__(reports);
firstPlan = __nelsonc_distribution_plan__(first);
asserts.isfalse(any(strcmp(firstPlan.runtime.modules, 'trigonometric_functions')));
asserts.istrue(any(strcmp(expected.runtime.modules, 'trigonometric_functions')));
output = fullfile(directory, 'installers');
compiler.runtime.customInstaller('runtime_a', first, 'RuntimeDelivery', 'installer', 'OutputDir', output);
compiler.runtime.customInstaller('runtime_b', reports{2}, 'RuntimeDelivery', 'installer', ...
  'OptionalDependencies', 'none', 'OutputDir', output, 'PackageType', 'zip');
unzip(fullfile(output, 'runtime_b.zip'), output);
try
  compiler.runtime.customInstaller('runtime_a', first, 'RuntimeDelivery', 'installer', 'OutputDir', output);
  error('Expected existing installer rejection.');
catch ex
  asserts.istrue(contains(ex.message, 'already exists'));
end
applications = fullfile(directory, 'applications');
mkdir(applications);
copyfile(first.Files{1}, applications);
copyfile(second.Files{1}, applications);
rmdir(source, 's');
rmdir(first.Options.OutputDir, 's');
rmdir(second.Options.OutputDir, 's');
local = fullfile(directory, 'local data');
destination = fullfile(local, 'Nelson', 'Runtime', expected.engineFingerprint);
installerA = fullfile(output, 'runtime_a.exe');
installerB = fullfile(output, 'runtime_b.exe');
[status, text] = install_runtime(installerA, destination);
asserts.istrue(status == 0, text);
firstManifest = jsondecode(fileread(fullfile(destination, 'runtime.json')));
asserts.isequal(firstManifest.moduleOrder, firstPlan.runtime.moduleOrder);
asserts.isfalse(isfile(fullfile(destination, 'shared_a.exe')));
old = {getenv('NELSONC_RUNTIME_ROOT'), getenv('NELSON_RUNTIME_PATH'), getenv('PATH'), getenv('LOCALAPPDATA')};
restore = onCleanup(@() restore_environment(old));
setenv('NELSONC_RUNTIME_ROOT', '');
setenv('NELSON_RUNTIME_PATH', '');
setenv('PATH', fullfile(getenv('SystemRoot'), 'System32'));
setenv('LOCALAPPDATA', local);
run_application(fullfile(applications, 'shared_a.exe'), 'SHARED_A_OK');

% A damaged shared runtime must fail before another application's files are added.
entry = find(endsWith({firstManifest.files.relative}, '/libnlsInterpreter.dll'));
interpreter = fullfile(destination, firstManifest.files(entry).relative);
saved = fullfile(directory, 'interpreter.dll');
movefile(interpreter, saved);
filewrite(interpreter, 'changed');
manifestDigest = sha256(fullfile(destination, 'runtime.json'), '-file');
[status, text] = install_runtime(installerB, destination);
asserts.istrue(status ~= 0, text);
asserts.isequal(sha256(fullfile(destination, 'runtime.json'), '-file'), manifestDigest);
delete(interpreter);
movefile(saved, interpreter);
before = dir(interpreter);
[status, text] = install_runtime(installerB, destination);
asserts.istrue(status == 0, text);
after = dir(interpreter);
asserts.isequal(before.datenum, after.datenum);
manifest = jsondecode(fileread(fullfile(destination, 'runtime.json')));
asserts.isequal(manifest.moduleOrder, expected.runtime.moduleOrder);
asserts.isequal(numel(manifest.files), numel(expected.runtime.files));
asserts.isequal(manifest.generated.sha256, sha256(fullfile(destination, 'modules', 'modules.m'), '-file'));
asserts.isfalse(isfolder(fullfile(destination, 'modules', 'compiler')));
asserts.isfalse(isfield(manifest.files, 'source'));
for k = 1:numel(manifest.files)
  item = manifest.files(k);
  asserts.isequal(sha256(fullfile(destination, item.relative), '-file'), item.sha256);
end
run_application(fullfile(applications, 'shared_a.exe'), 'SHARED_A_OK');
run_application(fullfile(applications, 'shared_b.exe'), 'SHARED_B_OK');
[status, text] = install_runtime(installerA, destination);
asserts.istrue(status == 0, text);
asserts.isequal(jsondecode(fileread(fullfile(destination, 'runtime.json'))), manifest);
run_application(fullfile(applications, 'shared_b.exe'), 'SHARED_B_OK');
filewrite(fullfile(destination, 'user-file.txt'), 'preserved');
uninstall = dir(fullfile(destination, 'unins*.exe'));
asserts.isequal(numel(uninstall), 1);
[status, text] = system(['"', fullfile(destination, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
asserts.istrue(status == 0, text);
asserts.istrue(isfile(fullfile(destination, 'user-file.txt')));
asserts.istrue(isfile(fullfile(applications, 'shared_a.exe')));
asserts.isfalse(isfile(interpreter));
asserts.isfalse(isfile(fullfile(destination, 'runtime-install.ini')));
clear restore cleanup;

catch installer_error
  skip_installer_antivirus(installer_error);
end
function [status, text] = install_runtime(installer, destination)
  log = [tempname(), '.log'];
  [status, text] = system(['"', installer, '" -agreeToLicense yes -destinationFolder "', destination, '" -outputFile "', log, '" /CURRENTUSER /PORTABLE=1'], 90);
  if status ~= 0 && isfile(log)
    text = fileread(log);
  end
end

function run_application(application, marker)
  [status, text] = system(['"', application, '"'], 60);
  asserts.istrue(status == 0 && contains(text, marker), text);
end

function restore_environment(old)
  setenv('NELSONC_RUNTIME_ROOT', old{1});
  setenv('NELSON_RUNTIME_PATH', old{2});
  setenv('PATH', old{3});
  setenv('LOCALAPPDATA', old{4});
end
