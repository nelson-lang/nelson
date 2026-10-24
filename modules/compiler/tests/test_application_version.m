%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'version_entry.m');
filewrite(entry, 'function version_entry(); disp(''VERSION_ENTRY_OK''); end');
opts = ncc('options', entry, 'RuntimeMode', 'installed', 'OutputDir', directory);
for version = {'0', '4.0', '1.2.3', '1.2.3.4', '65535.65535.65535.65535', '01.02'}
  __nelsonc_executable__('validate-version', version{1});
  opts.ExecutableVersion = version{1};
  asserts.isequal(opts.ExecutableVersion, version{1});
end
asserts.istrue(contains(ncc('--help'), '--executable-version'));
asserts.checkerror('ncc(entry, ''--executable-version'')', 'Missing value after --executable-version.');
opts.ExecutableVersion = '4.0';
plan = nelson.compiler.analyze(opts);
asserts.isequal(plan.executableVersion, '4.0');
baseline = ncc('options', entry, 'RuntimeMode', 'installed', 'OutputDir', fullfile(directory, 'baseline'));
baselinePlan = nelson.compiler.analyze(baseline);
asserts.isequal(rmfield(plan, 'executableVersion'), rmfield(baselinePlan, 'executableVersion'));
for version = {'65536', '1..2', '1.2.3.4.5', ''}
  destination = fullfile(directory, 'rejected', 'output.exe');
  raw = struct('executableVersion', version{1});
  assert_identifier(@() __nelsonc_build__(entry, destination, {}, false, 'installed', raw), 'Nelson:nelsonc:options');
  asserts.isfalse(isfolder(fileparts(destination)));
end
if ispc()
  launcher = fullfile(modulepath('nelson', 'bin'), 'nelson-app.exe');
  originalDigest = sha256(launcher, '-file');
  original = ncc(baseline);
  versioned = ncc(opts);
  asserts.isfalse(strcmp(original.SHA256, versioned.SHA256));
  asserts.isequal(original.Manifest, versioned.Manifest);
  asserts.isequal(original.RuntimePlan, versioned.RuntimePlan);
  asserts.isequal(versioned.DependencyPlan.executableVersion, '4.0');
  asserts.isequal(versioned.Options.ExecutableVersion, '4.0');
  opts.OutputDir = fullfile(directory, 'repeated');
  repeated = ncc(opts);
  asserts.isequal(versioned.SHA256, repeated.SHA256);
  legacy = ncc(entry, '-o', fullfile(directory, 'legacy.exe'), '--runtime', 'installed', '--executable-version', '4.0');
  asserts.isequal(versioned.SHA256, sha256(legacy, '-file'));
  asserts.isequal(originalDigest, sha256(launcher, '-file'));
  invalid = fullfile(directory, 'invalid.exe');
  filewrite(invalid, 'Not a portable executable');
  originalDigest = sha256(invalid, '-file');
  assert_identifier(@() __nelsonc_executable__('set-version', invalid, '1'), 'Nelson:nelsonc:executable');
  asserts.isequal(originalDigest, sha256(invalid, '-file'));
else
  % The property is retained, but it must not affect non-Windows artifacts.
  original = ncc(baseline);
  versioned = ncc(opts);
  asserts.isequal(original.SHA256, versioned.SHA256);
end
clear cleanup;

function assert_identifier(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(strcmp(identifier, expected) || startsWith(identifier, [expected, ':']));
end
