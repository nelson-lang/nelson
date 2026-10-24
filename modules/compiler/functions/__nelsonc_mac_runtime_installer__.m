%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_mac_runtime_installer__(stage, payload, plan, options)
  package = fullfile(stage, 'package'); mkdir(package);
  movefile(payload, fullfile(package, 'payload'));
  rmfile(fullfile(package, 'payload', 'runtime.json'));
  rmfile(fullfile(package, 'payload', 'modules', 'modules.m'));
  __nelsonc_mac_runtime_package__(package, plan);
  filewrite(fullfile(package, 'engine.txt'), [plan.engineFingerprint, char(10), plan.architecture, char(10)]);
  scripts = fullfile(stage, 'scripts'); mkdir(scripts);
  resource = fullfile(modulepath('compiler'), 'resources', 'macos', 'runtime-postinstall');
  copyfile(resource, fullfile(scripts, 'postinstall'));
  set_mode(fullfile(scripts, 'postinstall'), '755');
  publish = fullfile(stage, 'publish'); mkdir(publish);
  output = fullfile(publish, [options.InstallerName, '.pkg']);
  digest = sha256(options.InstallerName, '-string');
  location = ['/Library/Application Support/Nelson/.packages/', plan.engineFingerprint, '/', ...
    digest(1:16)];
  identifier = ['org.nelson-lang.runtime.', plan.engineFingerprint(1:24)];
  __nelsonc_mac_package__(package, scripts, location, identifier, ...
    plan.runtimeVersion, output, options.Verbose);
  if ~isfolder(options.OutputDir); mkdir(options.OutputDir); end
  __nelsonc_executable__('publish', publish, options.OutputDir);
end

function set_mode(filename, mode)
  quote = char(39); value = [quote, strrep(filename, quote, char([39 34 39 34 39])), quote];
  [status, output] = system(['/bin/chmod ', mode, ' ', value]);
  if status ~= 0; error('Nelson:nelsonc:installerTool:cannotSetPackageScriptMode', 'Cannot set package script mode: %s', output); end
end
