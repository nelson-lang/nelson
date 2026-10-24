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
oldPath = path();
restorePath = onCleanup(@() path(oldPath));
source = fullfile(directory, 'source');
first = fullfile(directory, 'first');
second = fullfile(directory, 'second');
mkdir(source);
mkdir(first);
mkdir(second);
entry = fullfile(source, 'directive_paths.m');
filewrite(fullfile(first, 'directive data.txt'), 'first resource');
filewrite(fullfile(second, 'directive data.txt'), 'second resource');
filewrite(fullfile(first, 'directive_path_choice.m'), 'function value = directive_path_choice(); value = 1; end');
filewrite(fullfile(second, 'directive_path_choice.m'), 'function value = directive_path_choice(); value = 2; end');
addpath(first, '-begin');
addpath(second, '-begin');
asserts.isequal(strrep(fileparts(which('directive_path_choice')), '\', '/'), strrep(second, '\', '/'));
filewrite(entry, strjoin({'%#function "directive data.txt" handle', 'function directive_paths(); end'}, char(10)));
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
resources = plan.files(strcmp({plan.files.representation}, 'resource'));
asserts.isequal(numel(resources), 1);
asserts.istrue(endsWith(resources.source, '/second/directive data.txt'));

filewrite(fullfile(source, 'directive data.txt'), 'local resource');
plan = jsondecode(__nelsonc_plan__('link', entry));
resources = plan.files(strcmp({plan.files.representation}, 'resource'));
asserts.isequal(numel(resources), 1);
asserts.istrue(endsWith(resources.source, '/source/directive data.txt'));

external = fullfile(second, 'external data.txt');
filewrite(external, 'external resource');
filewrite(entry, strjoin({'%#function "external data.txt"', '%#exclude "external data.txt"', 'function directive_paths(); end'}, char(10)));
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete);
asserts.isfalse(any(strcmp({plan.files.representation}, 'resource')));
asserts.istrue(endsWith(plan.exclusions.to, '/second/external data.txt'));
plan = jsondecode(__nelsonc_plan__('link', entry, {external}));
asserts.istrue(plan.complete);
asserts.istrue(plan.exclusions.explicitOverride);
resources = plan.files(strcmp({plan.files.representation}, 'resource'));
asserts.isequal(numel(resources), 1);
asserts.istrue(endsWith(resources.source, '/second/external data.txt'));
clear restorePath cleanup;
