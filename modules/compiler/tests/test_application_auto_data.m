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
source = fullfile(directory, 'source');
mkdir(source);
entry = fullfile(source, 'data_entry.m');
lines = {'%#function pragma.txt', '%#exclude excluded.txt', ...
  'function value = data_entry()', 'value = data_helper();', ...
  'fileread(''automatic.txt'');', 'fileread automatic.txt', 'load(''excluded.txt'');', 'end'};
filewrite(entry, strjoin(lines, char(10)));
filewrite(fullfile(source, 'data_helper.m'), 'function value = data_helper(); value = fileread(''transitive.txt''); end');
for name = {'automatic.txt', 'pragma.txt', 'excluded.txt', 'transitive.txt'}
  filewrite(fullfile(source, name{1}), name{1});
end
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete);
asserts.istrue(plan.autoDetectDataFiles);
asserts.istrue(has_file(plan, '/automatic.txt'));
asserts.istrue(has_file(plan, '/transitive.txt'));
asserts.istrue(has_file(plan, '/pragma.txt'));
asserts.isfalse(has_file(plan, '/excluded.txt'));
encoded = __nelsonc_plan__('link', entry, {}, false);
asserts.isequal(encoded, __nelsonc_plan__('link', entry, {}, false));
disabled = jsondecode(encoded);
asserts.istrue(disabled.complete);
asserts.isfalse(disabled.autoDetectDataFiles);
asserts.isfalse(disabled.dynamicFallback);
asserts.isfalse(has_file(disabled, '/automatic.txt'));
asserts.isfalse(has_file(disabled, '/transitive.txt'));
asserts.istrue(has_file(disabled, '/pragma.txt'));
asserts.istrue(has_file(disabled, '/data_helper.m'));
asserts.istrue(has_edge(disabled, 'ignored-data'));
explicit = fullfile(source, 'excluded.txt');
forced = jsondecode(__nelsonc_plan__('link', entry, {explicit}, false));
asserts.istrue(forced.complete);
asserts.istrue(forced.exclusions.explicitOverride);
asserts.istrue(has_file(forced, '/excluded.txt'));
asserts.istrue(has_file(forced, '/pragma.txt'));
asserts.isfalse(has_file(forced, '/automatic.txt'));
filewrite(entry, strjoin([lines(1:end - 1), {'fileread(''absent.txt'');', 'end'}], char(10)));
missing = jsondecode(__nelsonc_plan__('link', entry));
asserts.isfalse(missing.complete);
asserts.istrue(any(strcmp({missing.diagnostics.name}, 'absent.txt')));
disabled = jsondecode(__nelsonc_plan__('link', entry, {}, false));
asserts.istrue(disabled.complete);
filewrite(entry, 'function data_entry(); absent_function(); end');
missing = jsondecode(__nelsonc_plan__('link', entry, {}, false));
asserts.isfalse(missing.complete);
asserts.istrue(any(strcmp({missing.diagnostics.name}, 'absent_function')));
for value = {0, 1, [], [true false], 'off', struct()}
  rejected = false;
  try
    __nelsonc_plan__('link', entry, {}, value{1});
  catch ex
    rejected = contains(ex.message, 'logical scalar');
  end
  asserts.istrue(rejected);
end

scripts = fullfile(directory, 'scripts');
mkdir(scripts);
scriptEntry = fullfile(scripts, 'script_entry.m');
filewrite(scriptEntry, 'function script_entry(); run(''helper_script.m''); end');
filewrite(fullfile(scripts, 'helper_script.m'), 'fileread(''not_embedded.txt'');');
plan = jsondecode(__nelsonc_plan__('link', scriptEntry, {}, false));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.istrue(plan.dynamicFallback);
asserts.istrue(has_file(plan, '/helper_script.m'));
asserts.isfalse(has_file(plan, '/not_embedded.txt'));
clear cleanup;

function result = has_file(plan, suffix)
  result = any(endsWith(string({plan.files.source}), suffix));
end

function result = has_edge(plan, kind)
  result = false;
  for k = 1:numel(plan.edges)
    if iscell(plan.edges)
      edge = plan.edges{k};
    else
      edge = plan.edges(k);
    end
    result = result || strcmp(edge.kind, kind);
  end
end
