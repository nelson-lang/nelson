%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc(); skip_testsuite('Native installer validation requires Windows.'); end
ncc('--help');
__nelsonc_inno_engine__();
try
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
mkdir(source);
A = [1, 2; 3, 4];
native = fullfile(source, 'native.nh5');
classic = fullfile(source, 'classic.mat');
hierarchical = fullfile(source, 'hierarchical.mat');
savenh5(native, 'A');
savemat(classic, '-v7', 'A');
savemat(hierarchical, '-v7.3', 'A');
entry = fullfile(source, 'external_install.m');
lines = {'function external_install()', 'root = fileparts(mfilename(''fullpath''));', ...
  'a = loadnh5(fullfile(root, ''native.nh5''));', ...
  'b = loadmat(fullfile(root, ''classic.mat''));', ...
  'c = loadmat(fullfile(root, ''hierarchical.mat''));', ...
  'assert(isequal(a.A, [1, 2; 3, 4]) && isequal(a, b) && isequal(a, c));', ...
  'disp(''EXTERNAL_INSTALL_OK'');', 'end'};
filewrite(entry, strjoin(lines, newline()));
result = compiler.build.standaloneApplication(entry, 'EmbedArchive', false, ...
  'AdditionalFiles', {native, classic, hierarchical}, 'OutputDir', fullfile(directory, 'build'));
report = fullfile(result.Options.OutputDir, 'buildresult.json');
options = compiler.package.InstallerOptions(result, 'RuntimeDelivery', 'none', ...
  'InstallerName', 'ExternalArchive', 'OutputDir', fullfile(directory, 'installer'), 'Shortcut', '');
rejected(@() compiler.package.installer(result.Files([1, 3]), report, 'Options', options));
asserts.isfalse(isfolder(options.OutputDir));
backup = fullfile(directory, 'original.nca');
copyfile(result.Files{2}, backup);
fid = fopen(result.Files{2}, 'r+b');
fwrite(fid, uint8(0), 'uint8');
fclose(fid);
document = jsondecode(fileread(report));
index = find(endsWith({document.files.relative}, '.nca'));
document.files(index).sha256 = sha256(result.Files{2}, '-file');
alteredReport = fullfile(directory, 'altered-report.json');
filewrite(alteredReport, jsonencode(document));
rejected(@() compiler.package.installer(result.Files, alteredReport, 'Options', options));
asserts.isfalse(isfolder(options.OutputDir));
copyfile(backup, result.Files{2});
rmdir(source, 's');
compiler.package.installer(result.Files, report, 'Options', options);
installer = fullfile(options.OutputDir, 'ExternalArchive.exe');
target = fullfile(directory, 'installed');
[status, output] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', ...
  target, '" /LOG="', fullfile(directory, 'install.log'), '"'], 90);
asserts.isequal(status, 0, output);
executable = fullfile(target, 'external_install.exe');
archive = fullfile(target, 'external_install.nca');
asserts.isequal(sha256(executable, '-file'), sha256(result.Files{1}, '-file'));
asserts.isequal(sha256(archive, '-file'), sha256(result.Files{2}, '-file'));
previous = getenv('NELSONC_RUNTIME_ROOT');
restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[status, output] = system(['"', executable, '"'], 60);
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, 'EXTERNAL_INSTALL_OK'), output);
clear restore;
filewrite(fullfile(target, 'user-output.txt'), 'KEEP');
uninstall = dir(fullfile(target, 'unins*.exe'));
asserts.isequal(numel(uninstall), 1);
[status, output] = system(['"', fullfile(target, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
asserts.isequal(status, 0, output);
asserts.isfalse(isfile(executable));
asserts.isfalse(isfile(archive));
asserts.isequal(fileread(fullfile(target, 'user-output.txt')), 'KEEP');
clear cleanup;

catch installer_error
  skip_installer_antivirus(installer_error);
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
