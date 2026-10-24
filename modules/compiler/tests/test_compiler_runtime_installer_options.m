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
badCalls = {...
  @() compiler.runtime.customInstaller('../bad', []), ...
  @() compiler.runtime.customInstaller('runtime', [], 'RuntimeDelivery', 'none'), ...
  @() compiler.runtime.customInstaller('runtime', [], 'PackageType', 'zip'), ...
  @() compiler.runtime.customInstaller('runtime', [], 'RuntimeDelivery', 'installer', 'Unsupported', 1), ...
  @() compiler.runtime.customInstaller('runtime', [], 'RuntimeDelivery', 'installer', 'Compression', 'ultra'), ...
  @() compiler.runtime.customInstaller('runtime', [], 'RuntimeDelivery'), ...
  @() compiler.runtime.customInstaller('runtime', [], 'OutputDir', 42)};
for k = 1:numel(badCalls)
  rejected = false;
  try
    feval(badCalls{k});
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
if ~ispc()
  return;
end
try
  compiler.runtime.customInstaller('runtime', []);
  error('Expected unavailable download.');
catch ex
  asserts.istrue(contains(ex.message, 'Automatic runtime download'));
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
plan = struct('architecture', computer('arch'), 'runtimeVersion', '2.0.0', 'engineFingerprint', repmat('a', 1, 64));
options = struct('InstallerName', 'runtime', 'Compression', 'normal');
script = strjoin(__nelsonc_runtime_script__(directory, directory, directory, plan, options), char(10));
asserts.istrue(startsWith(script, '#preproc builtin'));
asserts.istrue(contains(script, 'SetupMutex=Nelson-Runtime-'));
asserts.istrue(contains(script, 'HKA64'));
asserts.istrue(contains(script, 'ChangesEnvironment=no'));
asserts.istrue(contains(script, 'onlyifdoesntexist'));
asserts.isfalse(contains(script, 'RECOVERYPAUSE'));
asserts.isfalse(contains(script, 'RUNTIME_RECOVERY_CODE'));
for kind = {'files', 'directories'}
  for reserved = {'.nelson-runtime-update/journal.ini', '.NELSON-RUNTIME-prepare-0', '.nelson-runtime-recovered-0'}
    candidate = plan;
    candidate.runtime = struct('files', struct('relative', 'data/value.txt', 'sha256', repmat('a', 1, 64)), 'directories', {{}});
    if strcmp(kind{1}, 'files')
      candidate.runtime.files.relative = reserved{1};
    else
      candidate.runtime.directories = reserved;
    end
    filename = fullfile(directory, 'rejected.ini');
    rejected = false;
    try
      __nelsonc_runtime_inventory__(candidate, filename);
    catch ex
      rejected = (strcmp(ex.identifier, 'Nelson:nelsonc:runtimeCollision') || startsWith(ex.identifier, 'Nelson:nelsonc:runtimeCollision:'));
    end
    asserts.istrue(rejected);
    asserts.isfalse(isfile(filename));
  end
end
clear cleanup;
