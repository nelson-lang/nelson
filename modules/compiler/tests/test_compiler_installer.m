%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
if ~ispc()
  return;
end
__nelsonc_inno_engine__();
try
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'installer_app.m');
filewrite(entry, 'function installer_app(); disp(''INSTALLER_APP_OK''); disp(char([8364, 233])); end');
result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'build'), ...
  'RuntimeLogFile', 'application.log');
options = compiler.package.InstallerOptions(result, 'RuntimeDelivery', 'none', ...
  'ApplicationName', 'Installer probe', 'InstallerName', 'Installer Probe', ...
  'OutputDir', fullfile(directory, 'installer'), 'Shortcut', '');
asserts.isequal(options.ApplicationName, 'Installer probe');
options.AuthorCompany = 'Publisher {draft} "quoted"';
options.Summary = 'Installation test';
notes = fullfile(directory, 'notes.txt');
filewrite(notes, 'INSTALLER_NOTES');
options.AdditionalFiles = notes;
reportFile = fullfile(result.Options.OutputDir, 'buildresult.json');
assert_rejected(@() compiler.package.installer(result));
assert_rejected(@() compiler.package.installer(result, 'Options', options, 'Version', '2.0'));
assert_rejected(@() compiler.package.installer(result.Files, reportFile));
originalReadme = fileread(result.Files{2});
filewrite(result.Files{2}, 'MODIFIED BUILD OUTPUT');
assert_rejected(@() compiler.package.installer(result, 'Options', options));
filewrite(result.Files{2}, originalReadme);
asserts.isfalse(isfolder(options.OutputDir));
delete(entry);
compiler.package.installer(result.Files, reportFile, 'Options', options);
installer = fullfile(options.OutputDir, [options.InstallerName, '.exe']);
asserts.istrue(isfile(installer));
digest = sha256(installer, '-file');
failed = false;
try
  compiler.package.installer(result, 'Options', options);
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(sha256(installer, '-file'), digest);
destination = fullfile(directory, 'installed application');
[status, output] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', ...
  destination, '" /LOG="', fullfile(directory, 'install.log'), '"'], 90);
asserts.istrue(status == 0, output);
application = fullfile(destination, 'installer_app.exe');
asserts.isequal(sha256(application, '-file'), sha256(result.Files{1}, '-file'));
asserts.isequal(fileread(fullfile(destination, 'notes.txt')), 'INSTALLER_NOTES');
asserts.istrue(contains(fileread(fullfile(destination, 'nelson-installation.txt')), options.AuthorCompany));
asserts.isfalse(isfolder(fullfile(destination, 'installer_app.runtime')));
previous = getenv('NELSONC_RUNTIME_ROOT');
restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[status, output] = system(['"', application, '"'], 60);
asserts.istrue(status == 0 && contains(output, 'INSTALLER_APP_OK'), output);
runtimeLog = fullfile(destination, 'application.log');
logged = fileread(runtimeLog, 'char', 'unix', 'UTF-8');
asserts.istrue(contains(logged, 'INSTALLER_APP_OK'));
asserts.istrue(contains(logged, char([8364, 233])));
logDigest = sha256(runtimeLog, '-file');
clear restore;
filewrite(fullfile(destination, 'user-result.txt'), 'KEEP');
uninstall = dir(fullfile(destination, 'unins*.exe'));
asserts.isequal(numel(uninstall), 1);
[status, output] = system(['"', fullfile(destination, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
asserts.istrue(status == 0, output);
asserts.isfalse(isfile(application));
asserts.isequal(fileread(fullfile(destination, 'user-result.txt')), 'KEEP');
asserts.isequal(sha256(runtimeLog, '-file'), logDigest);

options.OutputDir = fullfile(directory, 'zip');
options.PackageType = 'zip';
compiler.package.installer(result, 'Options', options);
archive = fullfile(options.OutputDir, [options.InstallerName, '.zip']);
asserts.istrue(isfile(archive));
asserts.isfalse(isfile(fullfile(options.OutputDir, [options.InstallerName, '.exe'])));
unpacked = fullfile(directory, 'unpacked');
mkdir(unpacked);
unzip(archive, unpacked);
asserts.istrue(isfile(fullfile(unpacked, [options.InstallerName, '.exe'])));
clear cleanup;

catch installer_error
  skip_installer_antivirus(installer_error);
end
function assert_rejected(callback)
  failed = false;
  try
    callback();
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
