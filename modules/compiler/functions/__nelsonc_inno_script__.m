%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lines = __nelsonc_inno_script__(payload, output, shortcut, options, images, runtime)
  if nargin < 5
    images = struct('InstallerIcon', '', 'InstallerLogo', '', 'AddRemoveProgramsIcon', '');
  end
  identity = sha256(['Nelson application installer:', options.ApplicationName]);
  runtimeFiles = {}; runtimeCode = {};
  if nargin >= 6
    [runtimeFiles, runtimeCode] = __nelsonc_windows_runtime_script__(runtime, identity);
  end
  directory = literal(strrep(options.DefaultInstallationDir, '/', '\'));
  if startsWith(directory, '%ProgramFiles%\')
    directory = ['{autopf}\', directory(16:end)];
  end
  lines = __nelsonc_inno_setup__(output, options.ApplicationName, options.Version, identity, options.InstallerName, options.Compression);
  lines = [lines, {...
    ['AppPublisher=', literal(options.AuthorCompany)], ...
    ['AppComments=', literal(options.Summary)], ...
    ['DefaultDirName=', directory], ['DefaultGroupName=', literal(options.ApplicationName)], ...
    ['InfoBeforeFile=', fullfile(payload, 'nelson-installation.txt')]}, image_setup(images), {'[Files]'}, runtimeFiles, {...
    ['Source: "', parameter(fullfile(payload, '*')), '"; DestDir: "{code:InstallTargetDir}"; Flags: ignoreversion recursesubdirs createallsubdirs touch']}];
  if ~isempty(shortcut)
    icon = '';
    if ~isempty(images.InstallerIcon)
      icon = '; IconFilename: "{app}\nelson-installer-assets\InstallerIcon.ico"';
    end
    lines = [lines, {'[Icons]', ['Name: "{autoprograms}\', parameter(options.ApplicationName), ...
      '"; Filename: "{app}\', parameter(strrep(shortcut, '/', '\')), '"', icon, '; Check: DeploymentShortcut(False)'], ...
      ['Name: "{autodesktop}\', parameter(options.ApplicationName), '"; Filename: "{app}\', ...
      parameter(strrep(shortcut, '/', '\')), '"', icon, '; Check: DeploymentShortcut(True)']}];
  end
  lines = [lines, {'[Code]', 'function PortableInstall: Boolean;', 'begin', ...
    '  Result := ExpandConstant(''{param:PORTABLE|0}'') = ''1'';', 'end;'}];
  lines{end + 1} = fileread(fullfile(modulepath('compiler'), 'resources', 'installer-shortcuts.iss'));
  lines = [lines, runtimeCode];
end

function lines = image_setup(images)
  lines = {};
  if ~isempty(images.InstallerIcon)
    lines{end + 1} = ['SetupIconFile=', images.InstallerIcon];
  end
  if ~isempty(images.AddRemoveProgramsIcon)
    lines{end + 1} = 'UninstallDisplayIcon={app}\nelson-installer-assets\AddRemoveProgramsIcon.ico';
  end
  if ~isempty(images.InstallerLogo)
    lines = [lines, {['WizardImageFile=', images.InstallerLogo], ...
      'WizardImageStretch=no', 'WizardImageBackColor=$FFFFFF', 'DisableWelcomePage=no'}];
  end
end

function value = literal(value)
  if any(value < char(32))
    error('Nelson:nelsonc:installer:installerScriptValuesCannotContain', 'Installer script values cannot contain control characters.');
  end
  value = strrep(value, '{', '{{');
end

function value = parameter(value)
  value = strrep(literal(value), '"', '""');
end
