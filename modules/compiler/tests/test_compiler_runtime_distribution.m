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
source = fullfile(directory, 'source');
mkdir(source);
entry = fullfile(source, 'distribution_a.m');
filewrite(entry, 'function distribution_a(); disp(''DISTRIBUTION_A_OK''); end');
first = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'a'));
entry = fullfile(source, 'distribution_b.m');
filewrite(entry, 'function distribution_b(); if sin(0) ~= 0; error(''Unexpected value.''); end; disp(''DISTRIBUTION_B_OK''); end');
if ispc()
  second = compiler.build.standaloneWindowsApplication(entry, 'OutputDir', fullfile(directory, 'b'));
else
  second = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'b'));
end
reports = {fullfile(first.Options.OutputDir, 'buildresult.json'), ...
  fullfile(second.Options.OutputDir, 'buildresult.json')};
plan = __nelsonc_distribution_plan__(reports);
asserts.isequal(plan.applications, {'distribution_a'; 'distribution_b'});
asserts.istrue(any(strcmp(plan.runtime.modules, 'trigonometric_functions')));
asserts.isfalse(any(strcmp(plan.runtime.modules, 'compiler')));
asserts.istrue(numel(plan.runtime.modules) < numel(plan.runtime.startupOrder));
asserts.isequal(jsonencode(plan), jsonencode(__nelsonc_distribution_plan__([second, first])));
runtime = fullfile(directory, 'installed-runtime');
__nelsonc_copy_runtime__(plan.runtime, runtime);
inventory = jsondecode(fileread(fullfile(runtime, 'runtime.json')));
asserts.isequal(inventory.moduleOrder, plan.runtime.moduleOrder);
asserts.isequal(numel(inventory.files), numel(plan.runtime.files));
asserts.isfalse(isfield(inventory, 'startupOrder'));
asserts.isfalse(isfield(inventory.files, 'source'));
asserts.isfalse(isfolder(fullfile(runtime, 'modules', 'compiler')));
asserts.isequal(inventory.generated.sha256, sha256(fullfile(runtime, 'modules', 'modules.m'), '-file'));
rmdir(source, 's');
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
setenv('NELSONC_RUNTIME_ROOT', runtime);
applications = {first, second};
markers = {'DISTRIBUTION_A_OK', 'DISTRIBUTION_B_OK'};
for k = 1:numel(applications)
  result = applications{k};
  [status, output] = system(['"', result.Files{1}, '"'], 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, markers{k}), output);
end
clear restoreRuntime cleanup;
