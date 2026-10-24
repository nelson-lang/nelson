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
normal = fullfile(directory, 'normal');
mkdir(normal);
mkdir(fullfile(normal, '+pragma_pkg'));
entry = fullfile(normal, 'directives_entry.m');
write_lines(entry, {
  '%#exclude omitted missing_pragma_target excluded.txt'
  '%#function pragma_pkg.extra "include data.txt" local_helper'
  'function value = directives_entry()'
  'single = ''%#function not_a_function'';'
  'double = "%#exclude pragma_pkg.extra";'
  '% ordinary comment %#function not_a_function'
  '%{'
  '%#function'
  '%#exclude pragma_pkg.extra'
  '%}'
  'if ~isdeployed()'
  '  omitted();'
  '  missing_pragma_target();'
  '  fileread(''excluded.txt'');'
  'end'
  'value = 1 + ... %#function not_a_function'
  '  local_helper(); %#function inline_helper'
  'end'
  'function value = local_helper(); value = 1; end'
  });
write_lines(fullfile(normal, 'omitted.m'), {'function omitted(); missing_hidden_target(); sin(1); end'});
write_lines(fullfile(normal, '+pragma_pkg', 'extra.m'), {'function value = extra(); value = 42; end'});
write_lines(fullfile(normal, 'inline_helper.m'), {'function value = inline_helper(); value = 2; end'});
filewrite(fullfile(normal, 'include data.txt'), 'explicit resource');
filewrite(fullfile(normal, 'excluded.txt'), 'host-only resource');
encoded = __nelsonc_plan__('link', entry);
asserts.isequal(__nelsonc_plan__('link', entry), encoded);
plan = jsondecode(encoded);
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.isfalse(plan.dynamicFallback);
asserts.istrue(has_file(plan, '/+pragma_pkg/extra.m'));
asserts.istrue(has_file(plan, '/inline_helper.m'));
asserts.istrue(has_file(plan, '/include data.txt'));
asserts.isfalse(has_file(plan, '/omitted.m'));
asserts.isfalse(has_file(plan, '/excluded.txt'));
asserts.isfalse(has_module(plan, 'trigonometric_functions'));
asserts.istrue(has_edge(plan, 'pragma-local'));
asserts.istrue(has_edge(plan, 'excluded'));
asserts.isequal(numel(plan.exclusions), 3);
included = jsondecode(__nelsonc_plan__('link', entry, {fullfile(normal, 'excluded.txt')}));
asserts.istrue(included.complete);
asserts.istrue(has_file(included, '/excluded.txt'));
record = included.exclusions(strcmp({included.exclusions.name}, 'excluded.txt'));
asserts.istrue(record.explicitOverride);

late = fullfile(directory, 'late');
mkdir(late);
lateEntry = fullfile(late, 'late_entry.m');
write_lines(lateEntry, {'function late_entry(); a_early(); z_configuration(); end'});
write_lines(fullfile(late, 'a_early.m'), {'function a_early(); b_transitive(); end'});
write_lines(fullfile(late, 'b_transitive.m'), {'function b_transitive(); sin(1); end'});
write_lines(fullfile(late, 'z_configuration.m'), {'%#exclude a_early', 'function z_configuration(); end'});
plan = jsondecode(__nelsonc_plan__('link', lateEntry));
asserts.istrue(plan.complete);
asserts.isfalse(has_file(plan, '/a_early.m'));
asserts.isfalse(has_file(plan, '/b_transitive.m'));
asserts.isfalse(has_module(plan, 'trigonometric_functions'));
asserts.istrue(has_file(plan, '/z_configuration.m'));
forced = jsondecode(__nelsonc_plan__('link', lateEntry, {fullfile(late, 'a_early.m')}));
asserts.istrue(forced.complete);
asserts.istrue(has_file(forced, '/a_early.m'));
asserts.istrue(has_file(forced, '/b_transitive.m'));
asserts.istrue(has_module(forced, 'trigonometric_functions'));
write_lines(fullfile(late, 'a_early.m'), {'function a_early(); value = (; end'});
plan = jsondecode(__nelsonc_plan__('link', lateEntry));
asserts.istrue(plan.complete);
asserts.isfalse(has_file(plan, '/a_early.m'));

dynamic = fullfile(directory, 'dynamic');
mkdir(dynamic);
dynamicEntry = fullfile(dynamic, 'dynamic_entry.m');
write_lines(dynamicEntry, {'%#exclude omitted', '%#function callback_helper', 'function value = dynamic_entry(name); value = feval(name); end'});
write_lines(fullfile(dynamic, 'omitted.m'), {'function omitted(); nonexistent_from_excluded_file(); end'});
write_lines(fullfile(dynamic, 'callback_helper.m'), {'function value = callback_helper(); value = 42; end'});
plan = jsondecode(__nelsonc_plan__('link', dynamicEntry));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.istrue(plan.dynamicFallback);
asserts.istrue(has_file(plan, '/callback_helper.m'));
asserts.isfalse(has_file(plan, '/omitted.m'));

classes = fullfile(directory, 'classes');
mkdir(classes);
classEntry = fullfile(classes, 'class_entry.m');
write_lines(classEntry, {'%#function PragmaHolder', 'function class_entry(); end'});
write_lines(fullfile(classes, 'PragmaHolder.m'), {
  'classdef PragmaHolder'
  '%#function class_extra'
  'methods (Static)'
  'function value = getValue(); value = 42; end'
  'end'
  'end'
  });
write_lines(fullfile(classes, 'class_extra.m'), {'function value = class_extra(); value = 1; end'});
plan = jsondecode(__nelsonc_plan__('link', classEntry));
asserts.istrue(plan.complete, jsonencode(plan.diagnostics));
asserts.istrue(has_file(plan, '/PragmaHolder.m'));
asserts.istrue(has_file(plan, '/class_extra.m'));

invalid = fullfile(directory, 'invalid');
mkdir(invalid);
invalidEntry = fullfile(invalid, 'invalid_entry.m');
write_lines(invalidEntry, {'%#function missing_directive_target', 'function invalid_entry(); end'});
plan = jsondecode(__nelsonc_plan__('link', invalidEntry));
asserts.isfalse(plan.complete);
asserts.isequal(plan.diagnostics.name, 'missing_directive_target');
asserts.isequal(plan.diagnostics.kind, 'pragma-function');
asserts.isequal(plan.diagnostics.line, 1);
for text = {'%#function', '%#exclude', '%#function "unterminated'}
  write_lines(invalidEntry, {text{1}, 'function invalid_entry(); end'});
  rejected = false;
  try
    __nelsonc_plan__('link', invalidEntry);
  catch ex
    rejected = contains(ex.message, 'directive');
  end
  asserts.istrue(rejected);
end
clear cleanup;

function result = has_file(plan, suffix)
  result = any(endsWith(string({plan.files.source}), suffix));
end

function result = has_module(plan, name)
  result = ~isempty(plan.modules) && any(strcmp({plan.modules.name}, name));
end

function result = has_edge(plan, kind)
  result = false;
  for k = 1:numel(plan.edges)
    if iscell(plan.edges)
      item = plan.edges{k};
    else
      item = plan.edges(k);
    end
    result = result || strcmp(item.kind, kind);
  end
end

function write_lines(filename, lines)
  filewrite(filename, strjoin(lines, char(10)));
end
