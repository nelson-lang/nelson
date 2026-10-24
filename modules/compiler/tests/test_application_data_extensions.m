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
entry = fullfile(directory, 'extension_entry.m');
A = 17;
savenh5(fullfile(directory, 'native.nh5'), 'A');
savemat(fullfile(directory, 'classic.mat'), '-v7', 'A');
savemat(fullfile(directory, 'hierarchical.mat'), '-v7.3', 'A');
for statement = {'load(''native'');', 'load native', 'value = load("native");'}
  plan = analyze(entry, statement{1});
  assert_resource(plan, 'native.nh5');
end
assert_resource(analyze(entry, 'load classic -mat'), 'classic.mat');
assert_resource(analyze(entry, 'load(''hierarchical'');'), 'hierarchical.mat');

% Exact names win, then the native format, then the MAT format.
A = 31;
savenh5(fullfile(directory, 'choice.nh5'), 'A');
A = 47;
savemat(fullfile(directory, 'choice.mat'), '-v7', 'A');
copyfile(fullfile(directory, 'classic.mat'), fullfile(directory, 'choice'));
loaded = load(fullfile(directory, 'choice'));
asserts.isequal(loaded.A, 17);
plan = analyze(entry, 'load(''choice'');');
assert_resource(plan, 'choice');
asserts.isfalse(any(ismember({plan.files.relative}, {'choice.nh5', 'choice.mat'})));
delete(fullfile(directory, 'choice'));
loaded = load(fullfile(directory, 'choice'));
asserts.isequal(loaded.A, 31);
plan = analyze(entry, 'load(''choice'');');
assert_resource(plan, 'choice.nh5');
asserts.isfalse(any(strcmp({plan.files.relative}, 'choice.mat')));
delete(fullfile(directory, 'choice.nh5'));
loaded = load(fullfile(directory, 'choice'));
asserts.isequal(loaded.A, 47);
assert_resource(analyze(entry, 'load(''choice'');'), 'choice.mat');

mkdir(fullfile(directory, 'nested'));
copyfile(fullfile(directory, 'native.nh5'), fullfile(directory, 'nested', 'values.nh5'));
plan = analyze(entry, 'load(''nested/values'');');
assert_resource(plan, 'nested/values.nh5');

% Other readers and explicit extensions must not acquire load's fallback.
for statement = {'loadnh5(''native'');', 'loadmat(''classic'');', ...
  'fileread(''native'');', 'fopen(''native'');', 'load(''native.dat'');', ...
  'load(''missing'');', 'load(''native.nh5.missing'');', ...
  'load(''native''); fileread(''native'');'}
  plan = analyze(entry, statement{1});
  asserts.isfalse(plan.complete, statement{1});
end
plan = analyze(entry, 'load(''native'');');
disabled = jsondecode(__nelsonc_plan__('link', entry, {}, false));
asserts.istrue(disabled.complete);
asserts.isfalse(any(strcmp({disabled.files.relative}, 'native.nh5')));
asserts.isfalse(isempty(find_edges(disabled, 'kind', 'ignored-data')));
forced = jsondecode(__nelsonc_plan__('link', entry, {fullfile(directory, 'native.nh5')}, false));
assert_resource(forced, 'native.nh5');

% Filename fallback is applied independently in each explicit lookup root.
included = fullfile(directory, 'included');
mkdir(included);
copyfile(fullfile(directory, 'classic.mat'), fullfile(included, 'outside.mat'));
analyze(entry, 'load(''outside'');');
plan = jsondecode(__nelsonc_plan__('link', entry, {included}));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
edge = find_edges(plan, 'name', 'outside');
asserts.isequal(numel(edge), 1);
asserts.istrue(endsWith(strrep(edge{1}.to, '\', '/'), '/included/outside.mat'));

% Exclusion still applies to the resolved filename, not only the literal name.
analyze(entry, sprintf('%%#exclude native.nh5\nload(''native'');'));
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.isfalse(any(strcmp({plan.files.relative}, 'native.nh5')));
asserts.isfalse(isempty(find_edges(plan, 'kind', 'excluded')));
clear cleanup;

function plan = analyze(entry, statement)
  filewrite(entry, sprintf('function extension_entry()\n%s\nend', statement));
  plan = jsondecode(__nelsonc_plan__('link', entry));
end

function assert_resource(plan, name)
  asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
  index = find(strcmp(strrep({plan.files.relative}, '\', '/'), name));
  asserts.isequal(numel(index), 1, name);
  asserts.isequal(plan.files(index).representation, 'resource');
end

function found = find_edges(plan, field, value)
  found = {};
  for k = 1:numel(plan.edges)
    if iscell(plan.edges)
      item = plan.edges{k};
    else
      item = plan.edges(k);
    end
    if strcmp(item.(field), value)
      found{end + 1} = item;
    end
  end
end
