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
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
previousPath = path();
restorePath = onCleanup(@() path(previousPath));
previousRegistry = getenv('NELSON_EXTERNAL_MODULES_PATH');
restoreRegistry = onCleanup(@() setenv('NELSON_EXTERNAL_MODULES_PATH', previousRegistry));
registryRoot = fullfile(directory, 'registry');
mkdir(registryRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', registryRoot);
package = fullfile(directory, 'package');
other = fullfile(directory, 'package_other');
current = fullfile(directory, 'package_current');
mkdir(package);
mkdir(other);
mkdir(current);
helper = fullfile(package, 'public_package_helper.m');
filewrite(helper, 'function value = public_package_helper(); value = 7; end');
addpath(package);
entry = fullfile(directory, 'package_entry.m');
filewrite(entry, 'function package_entry(); disp(public_package_helper()); end');
simple = fullfile(directory, 'flat_entry.m');
filewrite(simple, 'function flat_entry(); disp(7); end');
flat = jsondecode(__nelsonc_plan__('link', simple));
asserts.istrue(flat.complete);
asserts.isfalse(isfield(flat, 'pathEntries'));
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.istrue(isfield(plan, 'pathEntries'));
asserts.istrue(any(strcmp({plan.pathEntries.relative}, 'package')));
registry = struct('unused', struct('path', other, 'version', '1.0.0'));
asserts.isequal(__nelsonc_included_packages__(plan, registry), {});
versions = [struct('path', package, 'version', '1.0.0'), ...
  struct('path', current, 'version', '2.0.0')];
registry.portable_data = struct('path', current, 'version', '2.0.0', 'versions', versions);
asserts.isequal(__nelsonc_included_packages__(plan, registry), {'portable_data'});
filewrite(fullfile(registryRoot, 'modules.json'), jsonencode(registry));
result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'output'));
asserts.isequal(result.IncludedSupportPackages, {'portable_data'});
asserts.isequal(nmm('list'), registry);
bad = struct('invalid', struct('path', 17));
rejected = false;
try
  __nelsonc_included_packages__(plan, bad);
catch ex
  rejected = strcmp(ex.identifier, 'Nelson:nelsonc:packages:invalidInstalledPackagePath');
end
asserts.istrue(rejected);
clear restoreRegistry;
clear restorePath;
rmdir(package, 's');
delete(entry);
previousRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previousRuntime));
setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
[status, output] = system(['"', result.Files{1}, '"'], 60);
asserts.istrue(status == 0, output);
asserts.istrue(contains(output, '7'), output);
clear restoreRuntime;
clear cleanup;
