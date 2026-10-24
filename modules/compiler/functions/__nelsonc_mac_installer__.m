%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_mac_installer__(stage, payload, shortcut, options, report)
  if ~isempty(shortcut) && ~same_path(shortcut, report.executable)
    error('Nelson:nelsonc:platform:macOSApplicationPackagesSupportApplication', 'macOS application packages support the application shortcut only.');
  end
  if report.runtime.gui
    create_bundle(payload, options, report);
  end
  metadata = fullfile(payload, '.nelson-install'); mkdir(metadata);
  resources = fullfile(modulepath('compiler'), 'resources', 'macos');
  uninstaller = fullfile(metadata, 'uninstall');
  copyfile(fullfile(resources, 'uninstall.sh'), uninstaller);
  set_mode(uninstaller, '755');
  records = __nelsonc_mac_inventory__(payload);
  records = records(~startsWith({records.relative}, '.nelson-install'));
  manifest = manifest_text(records);
  filewrite(fullfile(metadata, 'manifest.tsv'), manifest);
  filewrite(fullfile(metadata, 'identity'), ['NELSON_APPLICATION_INSTALL_1', char(10), ...
    sha256(manifest, '-string'), char(10), sha256(uninstaller, '-file'), char(10)]);
  scripts = fullfile(stage, 'scripts'); mkdir(scripts);
  copyfile(fullfile(resources, 'common.sh'), fullfile(scripts, 'common.sh'));
  copyfile(fullfile(resources, 'preinstall'), fullfile(scripts, 'preinstall'));
  filewrite(fullfile(scripts, 'manifest.tsv'), manifest);
  set_mode(fullfile(scripts, 'preinstall'), '755');
  publish = fullfile(stage, 'publish'); mkdir(publish);
  output = fullfile(publish, [options.InstallerName, '.pkg']);
  digest = sha256(options.ApplicationName, '-string');
  identifier = ['org.nelson-lang.application.', digest(1:24)];
  __nelsonc_mac_package__(payload, scripts, options.DefaultInstallationDir, ...
    identifier, options.Version, output, options.Verbose);
  if ~isfolder(options.OutputDir); mkdir(options.OutputDir); end
  __nelsonc_executable__('publish', publish, options.OutputDir);
end

function create_bundle(payload, options, report)
  [parent, stem] = fileparts(report.executable);
  bundleName = options.ApplicationName;
  if any(double(bundleName) > 127); bundleName = stem; end
  bundle = fullfile(payload, [bundleName, '.app']);
  if isfile(bundle) || isfolder(bundle)
    error('Nelson:nelsonc:installer:applicationInputsCollideMacOSApplication', 'Application inputs collide with the macOS application bundle.');
  end
  executable = fullfile(payload, report.executable);
  binaryRoot = fullfile(bundle, 'Contents', 'MacOS'); mkdir(binaryRoot);
  movefile(executable, fullfile(binaryRoot, stem));
  for extension = {'.nca', '.runtime'}
    source = fullfile(payload, parent, [stem, extension{1}]);
    if isfile(source) || isfolder(source); movefile(source, fullfile(binaryRoot, [stem, extension{1}])); end
  end
  plist = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">', char(10), ...
    '<plist version="1.0"><dict>', ...
    '<key>CFBundleExecutable</key><string>', xml_text(stem), '</string>', ...
    '<key>CFBundleIdentifier</key><string>org.nelson-lang.deployed.', report.engineFingerprint(1:24), '</string>', ...
    '<key>CFBundleDisplayName</key><string>', xml_text(options.ApplicationName), '</string>', ...
    '<key>CFBundleName</key><string>', xml_text(options.ApplicationName), '</string>', ...
    '<key>CFBundlePackageType</key><string>APPL</string>', ...
    '<key>CFBundleShortVersionString</key><string>', xml_text(options.Version), '</string>', ...
    '<key>NSHighResolutionCapable</key><true/>', ...
    '</dict></plist>', char(10)];
  filewrite(fullfile(bundle, 'Contents', 'Info.plist'), plist);
end

function text = manifest_text(records)
  lines = cell(1, numel(records));
  for k = 1:numel(records)
    item = records(k);
    lines{k} = strjoin({item.kind, item.sha256, item.mode, item.relative}, char(9));
  end
  text = [strjoin(lines, char(10)), char(10)];
end

function value = xml_text(value)
  value = strrep(value, '&', '&amp;'); value = strrep(value, '<', '&lt;');
  value = strrep(value, '>', '&gt;'); value = strrep(value, '"', '&quot;');
end

function set_mode(filename, mode)
  [status, output] = system(['/bin/chmod ', mode, ' ', shell_argument(filename)]);
  if status ~= 0; error('Nelson:nelsonc:installer:cannotSetPackageMode', 'Cannot set package mode: %s', output); end
end

function value = shell_argument(value)
  quote = char(39); value = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end

function same = same_path(left, right)
  same = strcmp(strrep(left, '\', '/'), strrep(right, '\', '/'));
end
