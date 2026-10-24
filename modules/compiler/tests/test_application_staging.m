%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
source = fullfile(directory, 'snapshot_entry.m');
resource = fullfile(directory, 'input.txt');
write_text(source, sprintf('function value = snapshot_entry()\nvalue = 42;\nend\n'));
write_text(resource, 'expected resource');
plan = jsondecode(__nelsonc_plan__('link', source, {resource}));
payload = fullfile(directory, 'payload');
mkdir(payload);
manifest = __nelsonc_stage_application__(plan, payload, false);
asserts.isequal(manifest.entry, 'roots/1/snapshot_entry.nbc');
asserts.isequal(manifest.entryName, 'snapshot_entry');
asserts.isfalse(isfile(fullfile(payload, 'roots/1/snapshot_entry.m')));
asserts.istrue(contains(__bytecode_artifact__('inspect', fullfile(payload, manifest.entry)), 'functions=1'));
asserts.isequal(fileread(fullfile(payload, 'roots/1/input.txt')), 'expected resource');
asserts.istrue(isfile(source));
asserts.istrue(isfile(resource));
rmdir(payload, 's');
mkdir(payload);
write_text(source, sprintf('function value = snapshot_entry()\nvalue = 99;\nend\n'));
assert_rejected(@() __nelsonc_stage_application__(plan, payload, false), 'Nelson:nelsonc:sourceChanged');
rmdir(payload, 's');
mkdir(payload);
plan = jsondecode(__nelsonc_plan__('link', source, {resource}));
write_text(resource, 'modified resource');
assert_rejected(@() __nelsonc_stage_application__(plan, payload, false), 'Nelson:nelsonc:sourceChanged');
rmdir(payload, 's');
mkdir(payload);
artifact = fullfile(directory, 'snapshot_entry.nbc');
__bytecode_artifact__('compile', source, artifact);
plan = jsondecode(__nelsonc_plan__('link', source, {artifact}));
assert_rejected(@() __nelsonc_stage_application__(plan, payload, false), 'Nelson:nelsonc:inputCollision');
asserts.istrue(isfile(source));
asserts.istrue(isfile(artifact));
rmdir(payload, 's');
mkdir(payload);
packageSource = fullfile(modulepath('nelson', 'root'), 'modules', 'main', ...
  'tests', 'fixtures', 'nelsonc_package', '+ncouter', '+ncinner', 'package_entry.m');
plan = jsondecode(__nelsonc_plan__('link', packageSource, {}));
asserts.istrue(plan.complete);
manifest = __nelsonc_stage_application__(plan, payload, false);
asserts.isequal(manifest.entry, 'roots/1/+ncouter/+ncinner/package_entry.nbc');
asserts.isequal(manifest.entryName, 'ncouter.ncinner.package_entry');
asserts.istrue(isfile(fullfile(payload, manifest.entry)));
rmdir(payload, 's');
mkdir(payload);
for k = 1:numel(plan.files)
  if strcmp(plan.files(k).source, plan.entry)
    plan.files(k).relative = '+bad-name/package_entry.m';
  end
end
assert_rejected(@() __nelsonc_stage_application__(plan, payload, false), 'Nelson:nelsonc:entry:invalidApplicationEntryName');
clear cleanup;

function write_text(filename, text)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  fprintf(fid, '%s', text);
  fclose(fid);
end

function assert_rejected(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, expected);
end
