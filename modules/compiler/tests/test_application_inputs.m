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
source = fullfile(directory, 'app');
extra = fullfile(directory, 'extra');
mkdir(source);
mkdir(extra);
mkdir(fullfile(extra, 'nested'));
mkdir(fullfile(extra, 'empty'));
mkdir(fullfile(extra, 'nested', 'private'));
mkdir(fullfile(extra, '+inputpkg'));
write_text(fullfile(source, 'input_entry.m'), sprintf('function result = input_entry()\nresult = nested_value() + inputpkg.value();\nend\n'));
write_text(fullfile(extra, 'nested', 'nested_value.m'), sprintf('function result = nested_value()\nresult = private_value();\nend\n'));
write_text(fullfile(extra, 'nested', 'private', 'private_value.m'), sprintf('function result = private_value()\nresult = 40;\nend\n'));
write_text(fullfile(extra, '+inputpkg', 'value.m'), sprintf('function result = value()\nresult = 2;\nend\n'));
write_text(fullfile(extra, 'nested', 'data.txt'), 'NESTED_DATA');
write_text(fullfile(extra, 'data[1]+(a).txt'), 'LITERAL_PATTERN');
write_text(fullfile(extra, 'data1a.txt'), 'NOT_A_REGEX');
entry = fullfile(source, 'input_entry.m');
beforePath = path();
encoded = __nelsonc_plan__('link', entry, {extra});
plan = jsondecode(encoded);
asserts.istrue(plan.complete);
asserts.isequal(numel(plan.files), 7);
asserts.isequal(numel(plan.directories), 5);
asserts.isequal(path(), beforePath);
asserts.isequal(__nelsonc_plan__('link', entry, {extra, fullfile(extra, 'nested'), extra}), encoded);
asserts.istrue(any(strcmp({plan.files.relative}, 'nested/private/private_value.m')));
asserts.istrue(any(strcmp({plan.pathEntries.relative}, 'nested')));
asserts.isfalse(any(contains(string({plan.pathEntries.relative}), 'private')));
asserts.isfalse(any(contains(string({plan.pathEntries.relative}), '+inputpkg')));
payload = fullfile(directory, 'payload');
manifest = __nelsonc_stage_application__(plan, payload, false);
asserts.istrue(isfolder(fullfile(payload, 'roots/2/empty')));
asserts.isequal(fileread(fullfile(payload, 'roots/2/nested/data.txt')), 'NESTED_DATA');
asserts.istrue(isfile(fullfile(payload, 'roots/2/nested/nested_value.nbc')));
asserts.istrue(any(strcmp(manifest.paths, 'roots/2/nested')));
asserts.isequal(manifest.entry, 'roots/1/input_entry.nbc');
write_text(fullfile(extra, 'late.txt'), 'NOT_IN_SNAPSHOT');
frozen = fullfile(directory, 'frozen');
__nelsonc_stage_application__(plan, frozen, false);
asserts.isfalse(isfile(fullfile(frozen, 'roots/2/late.txt')));
write_text(fullfile(extra, 'nested', 'data.txt'), 'CHANGED');
assert_rejected(@() __nelsonc_stage_application__(plan, fullfile(directory, 'changed'), false), 'changed after analysis');

write_text(entry, sprintf('function result = input_entry()\nresult = 1;\nend\n'));
pattern = jsondecode(__nelsonc_plan__('link', entry, {fullfile(extra, 'data[1]+(?).*')}));
asserts.isequal(numel(pattern.files), 2);
asserts.istrue(any(strcmp({pattern.files.relative}, 'data[1]+(a).txt')));
asserts.istrue(isempty(pattern.directories));
flat = jsondecode(__nelsonc_plan__('link', entry, {fullfile(extra, '*')}));
asserts.isequal(numel(flat.files), 4);
asserts.isfalse(any(contains(string({flat.files.relative}), 'nested/')));
assert_rejected(@() __nelsonc_plan__('link', entry, {fullfile(extra, '*.none')}), 'matched no files');
assert_rejected(@() __nelsonc_plan__('link', entry, {fullfile(extra, '*', '*.txt')}), 'existing parent');
assert_rejected(@() __nelsonc_plan__('link', entry, {fullfile(extra, 'missing.txt')}), 'does not exist');
adversarial = [repmat('*a', 1, 40), 'b.txt'];
assert_rejected(@() __nelsonc_plan__('link', entry, {fullfile(extra, adversarial)}), 'matched no files');
if ispc()
  insensitive = jsondecode(__nelsonc_plan__('link', entry, {fullfile(extra, 'DATA*.TXT')}));
  asserts.isequal(numel(insensitive.files), 3);
else
  assert_rejected(@() __nelsonc_plan__('link', entry, {fullfile(extra, 'DATA*.TXT')}), 'matched no files');
end
write_text(entry, sprintf('%%#exclude data1a.txt\nfunction result = input_entry()\nresult = fileread(''data1a.txt'');\nend\n'));
forced = jsondecode(__nelsonc_plan__('link', entry, {extra}, false));
asserts.istrue(forced.complete);
asserts.istrue(forced.exclusions.explicitOverride);
asserts.istrue(any(strcmp({forced.files.relative}, 'data1a.txt')));
% Explicit resources inside an installed module remain application resources.
resource = fullfile(modulepath('compiler'), 'tests', 'fixtures', 'optional_compiler_install.iss');
installed = jsondecode(__nelsonc_plan__('link', entry, {resource}, false));
index = find(endsWith(string({installed.files.source}), '/optional_compiler_install.iss'), 1);
asserts.istrue(~isempty(index));
asserts.isequal(installed.files(index).module, '');
asserts.isequal(installed.files(index).representation, 'resource');
% A selected ancestor keeps the entry and sibling resources in one tree.
tree = fullfile(directory, 'tree');
mkdir(fullfile(tree, 'code'));
mkdir(fullfile(tree, 'data'));
treeEntry = fullfile(tree, 'code', 'tree_entry.m');
write_text(treeEntry, sprintf('function value = tree_entry()\nvalue = 1;\nend\n'));
write_text(fullfile(tree, 'data', 'sibling.txt'), 'SIBLING');
common = jsondecode(__nelsonc_plan__('link', treeEntry, {tree}));
asserts.isequal(numel(common.searchRoots), 1);
asserts.istrue(any(strcmp({common.files.relative}, 'code/tree_entry.m')));
asserts.istrue(any(strcmp({common.files.relative}, 'data/sibling.txt')));
commonPayload = fullfile(directory, 'common');
commonManifest = __nelsonc_stage_application__(common, commonPayload, false);
asserts.isequal(commonManifest.entry, 'roots/1/code/tree_entry.nbc');
asserts.istrue(any(strcmp(commonManifest.paths, 'roots/1/code')));
unicodeName = ['data-', char(233), '.txt'];
write_text(fullfile(tree, 'data', unicodeName), 'UNICODE');
unicodePlan = jsondecode(__nelsonc_plan__('link', treeEntry, {fullfile(tree, 'data', 'data-?.txt')}));
asserts.istrue(any(strcmp({unicodePlan.files.relative}, unicodeName)));
clear cleanup;

function write_text(filename, text)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  fprintf(fid, '%s', text);
  fclose(fid);
end

function assert_rejected(callback, expected)
  message = '';
  try
    callback();
  catch ex
    message = ex.message;
  end
  asserts.istrue(contains(message, expected));
end
