%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
ncc('--help');
if ~ispc(); return; end
try
check_shortcuts();

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_shortcuts()
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  payload = fullfile(work, 'payload'); mkdir(payload);
  filewrite(fullfile(payload, 'entry.txt'), 'Shortcut target');
  filewrite(fullfile(payload, 'nelson-installation.txt'), 'Isolated shortcut test.');
  options = struct('ApplicationName', 'Installer shortcut test', 'Version', '1.0.0.0', ...
    'InstallerName', 'shortcuts', 'AuthorCompany', '', 'Summary', '', ...
    'DefaultInstallationDir', fullfile(work, 'default'), 'PackageType', 'auto', ...
    'OutputDir', fullfile(work, 'package'), 'Compression', 'normal', 'Verbose', false);
  stage = fullfile(work, 'stage'); mkdir(stage);
  generator = @(output) isolated_script(payload, output, options, work);
  __nelsonc_inno_build__(__nelsonc_inno_engine__(), stage, payload, 'entry.txt', options, generator);
  setup = fullfile(options.OutputDir, 'shortcuts.exe');
  for index = 0:5
    desktop = bitand(index, 1) ~= 0;
    menu = bitand(index, 2) ~= 0;
    target = fullfile(work, ['installed_', int2str(index)]);
    control = fullfile(work, 'settings.txt');
    values = {'false', 'true'};
    filewrite(control, strjoin({'agreeToLicense=yes', ['applicationFolder=', target], ...
      ['desktopShortcut=', values{1 + desktop}], ['startMenuShortcut=', values{1 + menu}]}, newline()));
    command = ['-inputFile "', control, '" /CURRENTUSER'];
    if index == 0
      filewrite(control, strjoin({'agreeToLicense=yes', ['applicationFolder=', target]}, newline()));
    elseif index == 4
      filewrite(control, strjoin({'agreeToLicense=yes', ['applicationFolder=', target], ...
        'desktopShortcut=true', 'startMenuShortcut=true'}, newline()));
      command = [command, ' /PORTABLE=1'];
    elseif index == 5
      command = ['/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /DIR="', target, '"'];
      desktop = false;
      menu = true;
    end
    [status, output] = system(['"', setup, '" ', command], 60);
    asserts.isequal(status, 0, output);
    asserts.isequal(isfile(fullfile(work, 'desktop', 'Installer shortcut test.lnk')), desktop);
    asserts.isequal(isfile(fullfile(work, 'programs', 'Installer shortcut test.lnk')), menu);
    files = dir(fullfile(target, 'unins*.exe')); asserts.isequal(numel(files), 1);
    [status, output] = system(['"', fullfile(target, files.name), ...
      '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 60);
    asserts.isequal(status, 0, output);
    asserts.isfalse(isfile(fullfile(work, 'desktop', 'Installer shortcut test.lnk')));
    asserts.isfalse(isfile(fullfile(work, 'programs', 'Installer shortcut test.lnk')));
  end
  clear cleanup;
end

function lines = isolated_script(payload, output, options, work)
  lines = __nelsonc_inno_script__(payload, output, 'entry.txt', options);
  for k = 1:numel(lines)
    lines{k} = strrep(lines{k}, '{autodesktop}', fullfile(work, 'desktop'));
    lines{k} = strrep(lines{k}, '{autoprograms}', fullfile(work, 'programs'));
    lines{k} = strrep(lines{k}, 'CreateUninstallRegKey=not PortableInstall', 'CreateUninstallRegKey=no');
  end
end
