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
fixtures = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'fixtures', 'nelsonc_class');
addpath(fixtures);
pathCleanup = onCleanup(@() rmpath(fixtures));
asserts.isequal(class_entry(), 20);
asserts.isequal(legacy_entry(), 21);
clear pathCleanup;
entry = fullfile(fixtures, 'class_entry.m');
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete);
asserts.isfalse(plan.dynamicFallback);
asserts.isequal(numel(plan.files), 8);
sources = string({plan.files.relative});
asserts.istrue(any(sources == "+ncclasses/NcFactory.m"));
asserts.istrue(any(sources == "+ncclasses/NcBase.m"));
asserts.istrue(any(sources == "@NcDerived/NcDerived.m"));
asserts.istrue(any(sources == "@NcDerived/externalValue.m"));
asserts.istrue(any(sources == "@NcDerived/private/nc_class_private.m"));
representations = string({plan.files.representation});
asserts.isequal(sum(representations == "source"), 3);
asserts.isequal(sum(representations == "bytecode"), 5);

legacy = jsondecode(__nelsonc_plan__('link', fullfile(fixtures, 'legacy_entry.m')));
asserts.istrue(legacy.complete);
asserts.isfalse(legacy.dynamicFallback);
asserts.isequal(numel(legacy.files), 4);
asserts.istrue(all(string({legacy.files.representation}) == "bytecode"));
asserts.istrue(any(string({legacy.files.relative}) == "@NcLegacy/valueOf.m"));

directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
write_lines(fullfile(directory, 'class_analysis_entry.m'), {
  'function result = class_analysis_entry()'
  'object = NcAnalysisHandle();'
  'result = object.answer();'
  'end'
  });
write_lines(fullfile(directory, 'NcAnalysisHandle.m'), {
  'classdef NcAnalysisHandle < handle'
  'properties'
  'Value (1,1) double {nc_validate_positive, nc_validate_limit(Value, nc_analysis_limit())} = local_default();'
  'NeverEvaluate = error(''Dependency analysis must not execute defaults.'');'
  'end'
  'methods'
  'function result = answer(object)'
  'result = nc_analysis_leaf(object.Value);'
  'end'
  'end'
  'end'
  'function value = local_default()'
  'value = nc_analysis_default();'
  'end'
  });
for name = {'nc_analysis_leaf', 'nc_analysis_default', 'nc_analysis_limit', 'nc_validate_positive', 'nc_validate_limit'}
  write_lines(fullfile(directory, [name{1}, '.m']), {['function value = ', name{1}, '(varargin)'], 'value = 3;', 'end'});
end
write_lines(fullfile(directory, 'unreachable.m'), {'This is deliberately invalid source.'});
analysis = jsondecode(__nelsonc_plan__('link', fullfile(directory, 'class_analysis_entry.m')));
asserts.istrue(analysis.complete);
asserts.isfalse(analysis.dynamicFallback);
asserts.isequal(numel(analysis.files), 7);
asserts.isfalse(any(endsWith(string({analysis.files.source}), '/unreachable.m')));
asserts.istrue(any(string({analysis.modules.name}) == "handle"));

write_lines(fullfile(directory, 'nc_analysis_leaf.m'), {
  'function value = nc_analysis_leaf(input)'
  'value = nc_class_missing_dependency(input);'
  'end'
  });
missing = jsondecode(__nelsonc_plan__('link', fullfile(directory, 'class_analysis_entry.m')));
asserts.isfalse(missing.complete);
asserts.istrue(contains(jsonencode(missing.diagnostics), 'nc_class_missing_dependency'));

mkdir(fullfile(directory, '@NcMissingMethod'));
write_lines(fullfile(directory, '@NcMissingMethod', 'NcMissingMethod.m'), {
  'classdef NcMissingMethod'
  'methods'
  'result = absentMethod(object)'
  'end'
  'end'
  });
missingMethod = jsondecode(__nelsonc_plan__('link', fullfile(directory, '@NcMissingMethod', 'NcMissingMethod.m')));
asserts.isfalse(missingMethod.complete);
asserts.istrue(contains(jsonencode(missingMethod.diagnostics), 'absentMethod'));
write_lines(fullfile(directory, 'NcAbstractMethod.m'), {
  'classdef (Abstract) NcAbstractMethod'
  'methods (Abstract)'
  'result = absentMethod(object)'
  'end'
  'end'
  });
abstractMethod = jsondecode(__nelsonc_plan__('link', fullfile(directory, 'NcAbstractMethod.m')));
asserts.istrue(abstractMethod.complete);
asserts.isequal(numel(abstractMethod.files), 1);

mkdir(fullfile(directory, '@NcOperatorMethods'));
write_lines(fullfile(directory, '@NcOperatorMethods', 'NcOperatorMethods.m'), {
  'classdef NcOperatorMethods'
  'end'
  });
write_lines(fullfile(directory, '@NcOperatorMethods', 'plus.m'), {
  'function result = plus(object, value)'
  'result = nc_missing_operator_dependency(value);'
  'end'
  });
operatorMethods = jsondecode(__nelsonc_plan__('link', fullfile(directory, '@NcOperatorMethods', 'NcOperatorMethods.m')));
asserts.isfalse(operatorMethods.complete);
asserts.istrue(contains(jsonencode(operatorMethods.diagnostics), 'nc_missing_operator_dependency'));
asserts.istrue(any(endsWith(string({operatorMethods.files.source}), '/@NcOperatorMethods/plus.m')));
clear cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  for k = 1:numel(lines)
    fprintf(fid, '%s\n', lines{k});
  end
  fclose(fid);
end
