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
mkdir([directory, '/private']);
mkdir([directory, '/+ncpack']);
write_lines([directory, '/link_entry.m'], {
  'function result = link_entry(value)'
  'import ncpack.*'
  'result = nc_helper(value) + twice(value) + ncpack.leaf(value);'
  'callback = @nc_leaf;'
  'result = result + callback(value) + local_value();'
  'text = fileread(''data.txt'');'
  'fileread data.txt'
  'fid = fopen(''generated-output.txt'', ''w'');'
  'end'
  'function result = local_value()'
  'result = 1;'
  'end'
  });
write_lines([directory, '/nc_helper.m'], {
  'function result = nc_helper(value)'
  'result = private_scale(value) + nc_leaf(value);'
  'end'
  });
write_lines([directory, '/nc_leaf.m'], {
  'function result = nc_leaf(value)'
  'result = sin(value);'
  'end'
  });
write_lines([directory, '/private/private_scale.m'], {
  'function result = private_scale(value)'
  'result = 3 * value;'
  'end'
  });
write_lines([directory, '/+ncpack/twice.m'], {
  'function result = twice(value)'
  'result = 2 * value;'
  'end'
  });
write_lines([directory, '/+ncpack/leaf.m'], {
  'function result = leaf(value)'
  'result = cos(value);'
  'end'
  });
write_lines([directory, '/unused.m'], {
  'function unused()'
  'error(''Packaging must not execute application code.'');'
  'end'
  });
write_lines([directory, '/data.txt'], {'application data'});
entry = [directory, '/link_entry.m'];
beforePath = path();
beforeDirectory = pwd();
encoded = __nelsonc_plan__('link', entry);
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
asserts.isequal(__nelsonc_plan__('link', entry), encoded);
plan = jsondecode(encoded);
__bytecode_artifact__('verify', encoded);
invalidRuntime = plan;
invalidRuntime.engineFingerprint = repmat('0', 1, 64);
assert_runtime_rejected(invalidRuntime);
invalidRuntime = plan;
invalidRuntime.builtinCatalog(1).inputs = -123456;
assert_runtime_rejected(invalidRuntime);
invalidRuntime = plan;
invalidRuntime.builtinCatalogDigest = repmat('0', 1, 64);
assert_runtime_rejected(invalidRuntime);
asserts.istrue(iscell(plan.searchRoots));
asserts.isequal(numel(plan.searchRoots), 1);
for k = 1:numel(plan.files)
  asserts.isequal(plan.files(k).root, plan.searchRoots{1});
  asserts.isfalse(startsWith(plan.files(k).relative, '/'));
end
asserts.isequal(plan.format, 1);
asserts.isequal(plan.entryKind, 'function');
asserts.isfalse(plan.dynamicFallback);
asserts.istrue(plan.complete);
asserts.isequal(numel(plan.files), 7);
sources = string({plan.files.source});
asserts.istrue(any(endsWith(sources, '/private/private_scale.m')));
asserts.istrue(any(endsWith(sources, '/+ncpack/twice.m')));
asserts.istrue(any(endsWith(sources, '/+ncpack/leaf.m')));
asserts.istrue(any(endsWith(sources, '/nc_leaf.m')));
asserts.istrue(any(endsWith(sources, '/data.txt')));
asserts.isfalse(any(endsWith(sources, '/unused.m')));
for k = 1:numel(plan.files)
  asserts.isequal(length(plan.files(k).sha256), 64);
end
modules = string({plan.modules.name});
asserts.istrue(any(modules == "trigonometric_functions"));
asserts.isequal(length(plan.builtinCatalogDigest), 64);
explicit = jsondecode(__nelsonc_plan__('link', entry, {[directory, '/unused.m']}));
asserts.istrue(any(endsWith(string({explicit.files.source}), '/unused.m')));

write_lines([directory, '/dynamic_entry.m'], {
  'function result = dynamic_entry(name, value)'
  'result = feval(name, value);'
  'end'
  });
dynamic = jsondecode(__nelsonc_plan__('link', [directory, '/dynamic_entry.m']));
asserts.istrue(dynamic.dynamicFallback);
asserts.istrue(any(endsWith(string({dynamic.files.source}), '/unused.m')));
asserts.istrue(all([dynamic.modules.complete]));
% The dynamic fallback also scans the interpreter search path, so restrict the
% count to the application's own tree; otherwise unrelated files in the user
% search path (for example a personal ~/Documents/Nelson) would perturb it.
applicationFiles = startsWith(string({dynamic.files.source}), [directory, '/']);
asserts.isequal(sum(applicationFiles), 9);

externalDirectory = tempname();
mkdir(externalDirectory);
externalCleanup = onCleanup(@() rmdir(externalDirectory, 's'));
write_lines([externalDirectory, '/external_callback.m'], {
  'function result = external_callback(value)'
  'result = value + 10;'
  'end'
  });
addpath(externalDirectory);
pathCleanup = onCleanup(@() rmpath(externalDirectory));
external = jsondecode(__nelsonc_plan__('link', [directory, '/dynamic_entry.m']));
asserts.istrue(any(endsWith(string({external.files.source}), '/external_callback.m')));
clear pathCleanup externalCleanup;

write_lines([directory, '/missing_entry.m'], {
  'function missing_entry()'
  'missing_nelsonc_dependency_9843();'
  'end'
  });
missing = jsondecode(__nelsonc_plan__('link', [directory, '/missing_entry.m']));
asserts.isfalse(missing.complete);
asserts.istrue(contains(jsonencode(missing.diagnostics), 'missing_nelsonc_dependency_9843'));
write_lines([directory, '/script_entry.m'], {
  'error(''Must not execute while computing the link plan.'');'
  'value = nc_helper(3);'
  });
script = jsondecode(__nelsonc_plan__('link', [directory, '/script_entry.m']));
asserts.isequal(script.entryKind, 'script');
asserts.istrue(script.complete);

write_lines([directory, '/twice.m'], {
  'function result = twice(value)'
  'result = value + 2;'
  'end'
  });
write_lines([directory, '/scoped_imports.m'], {
  'function result = scoped_imports()'
  'import ncpack.twice'
  'result = twice(2) + unimported();'
  'end'
  'function result = unimported()'
  'result = twice(2);'
  'end'
  });
scoped = jsondecode(__nelsonc_plan__('link', [directory, '/scoped_imports.m']));
asserts.istrue(scoped.complete);
asserts.isequal(numel(scoped.files), 3);
asserts.istrue(any(endsWith(string({scoped.files.source}), '/+ncpack/twice.m')));
globalTwice = strrep([directory, '/twice.m'], '\', '/');
asserts.istrue(any(string({scoped.files.source}) == string(globalTwice)));

mkdir([directory, '/+zchoice']);
mkdir([directory, '/+achoice']);
write_lines([directory, '/+zchoice/pick.m'], {
  'function result = pick()'
  'result = 1;'
  'end'
  });
write_lines([directory, '/+achoice/pick.m'], {
  'function result = pick()'
  'result = 2;'
  'end'
  });
write_lines([directory, '/import_order.m'], {
  'function result = import_order()'
  'import zchoice.*'
  'import achoice.*'
  'result = pick();'
  'end'
  });
ordered = jsondecode(__nelsonc_plan__('link', [directory, '/import_order.m']));
asserts.istrue(ordered.complete);
asserts.isequal(numel(ordered.files), 2);
asserts.istrue(any(endsWith(string({ordered.files.source}), '/+zchoice/pick.m')));
addpath(directory);
pathCleanup = onCleanup(@() rmpath(directory));
asserts.isequal(import_order(), 1);
clear import_order pathCleanup;

write_lines([directory, '/ArtifactClass.m'], {
  'classdef ArtifactClass'
  'properties'
  'value = 1'
  'end'
  'end'
  });
rejected = false;
try
  __bytecode_artifact__('compile', [directory, '/ArtifactClass.m'], [directory, '/ArtifactClass.nbc']);
catch
  rejected = true;
end
asserts.istrue(rejected);
asserts.isfalse(isfile([directory, '/ArtifactClass.nbc']));
__bytecode_artifact__('compile', [directory, '/nc_leaf.m'], [directory, '/nc_leaf.nbc']);
delete([directory, '/nc_leaf.m']);
binary = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(binary.dynamicFallback);
asserts.istrue(any(endsWith(string({binary.files.source}), '/nc_leaf.nbc')));
clear cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  fclose(fid);
end

function assert_runtime_rejected(manifest)
  rejected = false;
  try
    __bytecode_artifact__('verify', jsonencode(manifest));
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
