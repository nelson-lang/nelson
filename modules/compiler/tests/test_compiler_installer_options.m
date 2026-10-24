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
options = compiler.package.InstallerOptions('ApplicationName', 'Example app');
expected = {'ApplicationName'; 'AdditionalFiles'; 'AddRemoveProgramsIcon'; ...
  'AuthorCompany'; 'AuthorEmail'; 'AuthorName'; 'Compression'; 'DefaultInstallationDir'; ...
  'Description'; 'InstallationNotes'; 'InstallerName'; 'InstallerIcon'; ...
  'InstallerLogo'; 'OptionalDependencies'; 'OutputDir'; 'PackageType'; ...
  'RuntimeDelivery'; 'Shortcut'; 'Summary'; 'Version'; 'Verbose'};
asserts.isequal(sort(properties(options)), sort(expected));
asserts.isequal(sort(fieldnames(struct(options))), sort(expected));
asserts.isequal(options.RuntimeDelivery, 'web');
asserts.isequal(options.PackageType, 'auto');
asserts.isequal(options.OptionalDependencies, 'all');
asserts.isequal(options.Compression, 'normal');
asserts.isequal(options.InstallerName, 'MyAppInstaller');
asserts.isequal(options.Version, '1.0');
asserts.isequal(options.Shortcut, '');
asserts.isequal(options.AdditionalFiles, {});
for image = {'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}
  asserts.isequal(options.(image{1}), '');
  for invalid = {42, {'image.png'}, "missing.png", ['invalid', char(0)]}
    failed = false;
    try; options.(image{1}) = invalid{1}; catch; failed = true; end
    asserts.istrue(failed);
  end
end
asserts.isfalse(options.Verbose);
asserts.istrue(endsWith(options.OutputDir, 'Example appinstaller'));
copy = options;
copy.RuntimeDelivery = 'installer';
copy.PackageType = 'zip';
copy.OptionalDependencies = 'none';
copy.Verbose = 'on';
copy.InstallerName = "Installer with spaces";
asserts.isequal(options.RuntimeDelivery, 'web');
asserts.istrue(copy.Verbose);
asserts.isequal(copy.InstallerName, 'Installer with spaces');
asserts.isequal(struct(copy).PackageType, 'zip');
for pair = {{'RuntimeDelivery', 'automatic'}, {'PackageType', 'tar'}, ...
  {'OptionalDependencies', 'graphics'}, {'Compression', 'ultra'}, {'Version', '1.2.3.4.5'}, ...
  {'ApplicationName', '../escape'}, {'InstallerName', 'NUL'}, ...
  {'AuthorName', ['name', char(10), '[Run]']}, {'Summary', char(0)}, ...
  {'Verbose', 2}, {'DefaultInstallationDir', 'relative/path'}}
  failed = false;
  try
    value = pair{1};
    copy.(value{1}) = value{2};
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
for args = {{}, {'ApplicationName', ''}, {'ApplicationName', 'example', 'InstallerIcon', 'missing.png'}, ...
  {'ApplicationName', 'example', 'UnknownOption', 'value'}}
  failed = false;
  try
    values = args{1};
    compiler.package.InstallerOptions(values{:});
  catch
    failed = true;
  end
  asserts.istrue(failed);
end

% Generated script values remain literal; no inline preprocessing is enabled.
options.AuthorCompany = 'Publisher {# dangerous()} "quoted"';
script = strjoin(__nelsonc_inno_script__('C:/payload', 'C:/output', '', options), char(10));
asserts.istrue(startsWith(script, '#preproc builtin'));
asserts.istrue(contains(script, 'Publisher {{# dangerous()} "quoted"'));
asserts.istrue(contains(script, 'Compression=lzma2/normal'));
options.Compression = 'none';
script = strjoin(__nelsonc_inno_script__('C:/payload', 'C:/output', '', options), char(10));
asserts.istrue(contains(script, 'Compression=none'));
options.Compression = 'fast';
script = strjoin(__nelsonc_inno_script__('C:/payload', 'C:/output', '', options), char(10));
asserts.istrue(contains(script, 'Compression=lzma2/fast'));
options.Compression = 'normal';
if ispc()
  asserts.istrue(contains(script, 'DefaultDirName={autopf}\Example app'));
  previous = pwd();
  restore = onCleanup(@() cd(previous));
  imageRoot = fullfile(modulepath('compiler'), 'examples', 'standalone');
  cd(imageRoot);
  for property = {'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}
    options.(property{1}) = "app_icon.png";
    asserts.isequal(options.(property{1}), strrep(fullfile(imageRoot, 'app_icon.png'), '\', '/'));
  end
  clear restore;
  images = struct('InstallerIcon', 'C:/payload/InstallerIcon.ico', ...
    'InstallerLogo', 'C:/stage/logo.bmp', 'AddRemoveProgramsIcon', 'C:/payload/list.ico');
  script = strjoin(__nelsonc_inno_script__('C:/payload', 'C:/output', 'app.exe', options, images), char(10));
  asserts.istrue(contains(script, 'SetupIconFile=C:/payload/InstallerIcon.ico'));
  asserts.istrue(contains(script, 'WizardImageFile=C:/stage/logo.bmp'));
  asserts.istrue(contains(script, 'UninstallDisplayIcon={app}\nelson-installer-assets\AddRemoveProgramsIcon.ico'));
  asserts.istrue(contains(script, 'IconFilename: "{app}\nelson-installer-assets\InstallerIcon.ico"; Check: DeploymentShortcut(False)'));
end
