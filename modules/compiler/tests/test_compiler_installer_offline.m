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
% This is the only runtime-package installation allowed in the parallel block:
% the shared runtime lock rejects a second concurrent installation.
ncc('--help');
if ~ispc()
  return;
end
__nelsonc_inno_engine__();
try
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'offline_app.m');
filewrite(entry, 'function offline_app(); if ~isdeployed() || ismodule(''compiler''); error(''Invalid installed runtime.''); end; disp(''OFFLINE_INSTALL_OK''); end');
result = compiler.build.standaloneWindowsApplication(entry, 'OutputDir', fullfile(directory, 'build'));
options = compiler.package.InstallerOptions(result, 'RuntimeDelivery', 'installer', ...
  'OptionalDependencies', 'none', 'OutputDir', fullfile(directory, 'installer'));
compiler.package.installer(result, 'Options', options);
installer = fullfile(options.OutputDir, [options.InstallerName, '.exe']);
expected = sha256(result.Files{1}, '-file');
delete(entry);
rmdir(result.Options.OutputDir, 's');
destination = fullfile(directory, 'offline target');
[status, output] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', ...
  destination, '" /LOG="', fullfile(directory, 'install.log'), '"'], 90);
if status ~= 0 && isfile(fullfile(directory, 'install.log')); output = fileread(fullfile(directory, 'install.log')); end
asserts.istrue(status == 0, output);
application = fullfile(destination, 'offline_app.exe');
runtime = fullfile(destination, 'offline_app.runtime');
asserts.isequal(sha256(application, '-file'), expected);
asserts.istrue(isfolder(runtime));
asserts.isfalse(isfolder(fullfile(runtime, 'modules', 'compiler')));
manifest = jsondecode(fileread(fullfile(runtime, 'runtime.json')));
asserts.isfalse(any(strcmp(manifest.modules, 'compiler')));
previousRoot = getenv('NELSONC_RUNTIME_ROOT');
previousRuntime = getenv('NELSON_RUNTIME_PATH');
previousPath = getenv('PATH');
restore = onCleanup(@() restore_environment(previousRoot, previousRuntime, previousPath));
setenv('NELSONC_RUNTIME_ROOT', '');
setenv('NELSON_RUNTIME_PATH', '');
setenv('PATH', fullfile(getenv('SystemRoot'), 'System32'));
[status, output] = system(['"', application, '"'], 60);
asserts.istrue(status == 0 && contains(output, 'OFFLINE_INSTALL_OK'), output);

% An adjacent but incompatible runtime must not fall back to another installation.
index = find(endsWith({manifest.files.relative}, '/libnlsInterpreter.dll'));
asserts.isequal(numel(index), 1);
interpreter = fullfile(runtime, manifest.files(index).relative);
saved = fullfile(directory, 'interpreter.dll');
movefile(interpreter, saved);
filewrite(interpreter, 'INVALID ENGINE');
setenv('NELSON_RUNTIME_PATH', modulepath('nelson', 'bin'));
[status, output] = system(['"', application, '"'], 60);
asserts.istrue(status ~= 0 && contains(output, 'adjacent application runtime'), output);
delete(interpreter);
movefile(saved, interpreter);
clear restore;
uninstall = dir(fullfile(destination, 'unins*.exe'));
asserts.isequal(numel(uninstall), 1);
[status, output] = system(['"', fullfile(destination, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
asserts.istrue(status == 0, output);
asserts.isfalse(isfile(application));
% Inno Setup 6.6 keeps the uninstaller stubs after an unattended uninstall; only the payload removal is checked.
asserts.isfalse(isfile(fullfile(runtime, 'runtime.json')));
asserts.isfalse(isfolder(fullfile(runtime, 'bin')));
asserts.isfalse(isfolder(fullfile(runtime, 'modules')));
clear cleanup;

catch installer_error
  skip_installer_antivirus(installer_error);
end
function restore_environment(root, runtime, search)
  setenv('NELSONC_RUNTIME_ROOT', root);
  setenv('NELSON_RUNTIME_PATH', runtime);
  setenv('PATH', search);
end
