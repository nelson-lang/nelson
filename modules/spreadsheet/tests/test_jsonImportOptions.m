%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
opts = jsonImportOptions();
asserts.isequal(class(opts), 'nelson.io.json.JSONImportOptions');
expectedProperties = {'VariableNames'; 'VariableNamingRule'; 'VariableTypes'; ...
  'SelectedVariableNames'; 'VariableOptions'; 'VariableSelectors'; 'RowNamesSelector'; ...
  'TableSelector'; 'VariableDescriptionsSelector'; 'VariableUnitsSelector'; ...
  'ImportErrorRule'; 'MissingRule'; 'RepeatedNodeRule'; 'AllowComments'; ...
  'AllowInfAndNaN'; 'AllowTrailingCommas'};
asserts.isequal(properties(opts), expectedProperties);
asserts.isequal(opts.VariableNames, {'Var1'});
asserts.isequal(opts.VariableTypes, {'char'});
asserts.isequal(opts.SelectedVariableNames, {'Var1'});
asserts.isequal(opts.VariableNamingRule, 'preserve');
asserts.isequal(opts.ImportErrorRule, 'fill');
asserts.isequal(opts.MissingRule, 'fill');
asserts.isequal(opts.RepeatedNodeRule, 'addcol');
asserts.isequal(opts.AllowComments, true);
asserts.isequal(opts.AllowInfAndNaN, true);
asserts.isequal(opts.AllowTrailingCommas, true);
asserts.isequal(opts.TableSelector, '');
%=============================================================================
opts = jsonImportOptions('NumVariables', 3);
asserts.isequal(opts.VariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(opts.VariableTypes, {'char', 'char', 'char'});
%=============================================================================
opts = jsonImportOptions('ParsingMode', 'strict');
asserts.isequal([opts.AllowComments, opts.AllowInfAndNaN, opts.AllowTrailingCommas], [false, false, false]);
opts = jsonImportOptions('ParsingMode', 'strict', 'AllowComments', true);
asserts.isequal([opts.AllowComments, opts.AllowInfAndNaN, opts.AllowTrailingCommas], [true, false, false]);
%=============================================================================
opts = jsonImportOptions('VariableSelectors', ["/a", "/b/c"], 'VariableTypes', {'double', 'string'});
asserts.isequal(opts.VariableSelectors, {'/a', '/b/c'});
asserts.isequal(opts.VariableNames, {'Var1', 'Var2'});
asserts.isequal(opts.VariableTypes, {'double', 'string'});
%=============================================================================
filename = [tempname(), '.json'];
fid = fopen(filename, 'w', 'n', 'UTF-8');
fprintf(fid, '%s', '[{"a": 1, "b": {"c": "x"}}, {"a": "2", "b": {"c": "y"}}]');
fclose(fid);
opts = jsonImportOptions('VariableSelectors', ["/a", "/b/c"], 'VariableTypes', {'double', 'string'}, ...
  'VariableNames', {'A', 'C'});
T = readtable(filename, opts);
asserts.isequal(T.Properties.VariableNames, {'A', 'C'});
asserts.isequal(T.A, [1; 2]);
asserts.isequal(T.C, ["x"; "y"]);
opts.SelectedVariableNames = {'C'};
T = readtable(filename, opts);
asserts.isequal(T.Properties.VariableNames, {'C'});
%=============================================================================
opts = detectImportOptions(filename);
asserts.isequal(opts.VariableSelectors, {'/a', '/b/c'});
asserts.isequal(opts.VariableNames, {'a', 'c'});
asserts.isequal(opts.VariableTypes, {'cell', 'string'});
opts.VariableTypes{1} = 'double';
T = readtable(filename, opts);
asserts.isequal(T.a, [1; 2]);
opts = detectImportOptions(filename, 'TextType', 'char');
asserts.isequal(opts.VariableTypes, {'cell', 'char'});
%=============================================================================
asserts.checkerror('jsonImportOptions(''RepeatedNodeRule'', ''skip'')', _('Invalid value for RepeatedNodeRule.'));
asserts.checkerror('jsonImportOptions(''RepeatedNodeRule'', ''skip'')', _('Invalid value for RepeatedNodeRule.'), 'Nelson:spreadsheet:invalidOptionValue');
asserts.checkerror('jsonImportOptions(''Foo'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('jsonImportOptions(''Foo'', 1)', _('Invalid name-value argument name.'), 'Nelson:spreadsheet:unknownNameValue');
asserts.checkerror('readtable(filename, opts, ''TextType'', ''char'')', _('Import options object expected.'));
%=============================================================================
