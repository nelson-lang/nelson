%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function runtime = __nelsonc_windows_runtime_package__(engine, stage, payload, report, plan, options, inventory)
  [parent, name] = fileparts(report.executable);
  runtime = struct('Included', strcmp(options.RuntimeDelivery, 'installer'), ...
    'PrivateDirectory', strrep(fullfile(parent, [name, '.runtime']), '/', '\'), ...
    'Fingerprint', report.engineFingerprint, 'Architecture', report.architecture, 'Package', '');
  reserved = fullfile(payload, '.nelson-install');
  if isfolder(reserved) || isfile(reserved)
    error('Nelson:nelsonc:installer:applicationInputsCollideNelsonInstallDir', 'Application inputs collide with .nelson-install.');
  end
  for k = 1:numel(inventory.files)
    [parent, name, extension] = fileparts(inventory.files(k).relative);
    if strcmpi([name, extension], 'nelson-runtime-package.exe')
      error('Nelson:nelsonc:installer:applicationInputUsesReservedRuntime', 'Application input uses the reserved runtime installer name.');
    end
  end
  if ~runtime.Included; return; end
  packageStage = tempname();
  __application_archive__('directory', packageStage);
  cleanup = onCleanup(@() rmdir(packageStage, 's'));
  source = fullfile(payload, runtime.PrivateDirectory);
  target = fullfile(packageStage, 'payload');
  [ok, message] = movefile(source, target);
  if ~ok; error('Nelson:nelsonc:installer', '%s', message); end
  __nelsonc_runtime_inventory__(plan, fullfile(packageStage, 'runtime-package.ini'));
  child = struct('InstallerName', 'nelson-runtime-package', 'PackageType', 'auto', ...
    'Compression', options.Compression, 'Verbose', options.Verbose, 'OutputDir', fullfile(stage, 'runtime-output'));
  generator = @(output) __nelsonc_runtime_script__(packageStage, target, output, plan, child);
  __nelsonc_inno_build__(engine, packageStage, target, '', child, generator, 'raw');
  runtime.Package = fullfile(child.OutputDir, 'nelson-runtime-package.exe');
  if ~isfile(runtime.Package)
    error('Nelson:nelsonc:installer:embeddedRuntimeInstallerTooLarge', 'An embedded runtime installer must be smaller than 2 GiB.');
  end
  clear cleanup;
end
