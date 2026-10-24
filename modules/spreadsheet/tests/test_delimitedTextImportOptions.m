%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = delimitedTextImportOptions();
asserts.isequal(class(opts), 'nelson.io.text.DelimitedTextImportOptions');
asserts.isequal(opts.SelectedVariableNames, {'Var1'});
asserts.isequal(opts.VariableNames, {'Var1'});
asserts.isequal(opts.VariableTypes, {'char'});
asserts.isequal(opts.ImportErrorRule, 'fill');
asserts.isequal(opts.MissingRule, 'fill');
asserts.isequal(opts.PreserveVariableNames, false);
asserts.isequal(opts.VariableNamingRule, 'modify');
asserts.isequal(opts.DataLines, [1 Inf]);
asserts.isequal(opts.VariableNamesLine, 0);
asserts.isequal(opts.RowNamesColumn, 0);
asserts.isequal(opts.VariableUnitsLine, 0);
asserts.isequal(opts.VariableDescriptionsLine, 0);
asserts.isequal(opts.ExtraColumnsRule, 'addvars');
asserts.isequal(opts.EmptyLineRule, 'skip');
asserts.isequal(opts.CommentStyle, {});
asserts.isequal(opts.Encoding, '');
asserts.isequal(opts.ConsecutiveDelimitersRule, 'split');
asserts.isequal(opts.LeadingDelimitersRule, 'keep');
asserts.isequal(opts.TrailingDelimitersRule, 'keep');
asserts.isequal(opts.Delimiter, {','});
%=============================================================================
expectedProperties = {
  'SelectedVariableNames';
  'VariableNames';
  'VariableOptions';
  'VariableTypes';
  'ImportErrorRule';
  'MissingRule';
  'PreserveVariableNames';
  'VariableNamingRule';
  'DataLines';
  'VariableNamesLine';
  'RowNamesColumn';
  'VariableUnitsLine';
  'VariableDescriptionsLine';
  'ExtraColumnsRule';
  'EmptyLineRule';
  'Whitespace';
  'LineEnding';
  'CommentStyle';
  'Encoding';
  'ConsecutiveDelimitersRule';
  'LeadingDelimitersRule';
  'TrailingDelimitersRule';
  'Delimiter'};
asserts.isequal(properties(opts), expectedProperties);
%=============================================================================
opts = delimitedTextImportOptions('NumVariables', 3);
asserts.isequal(opts.VariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(opts.SelectedVariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(opts.VariableTypes, {'char', 'char', 'char'});
%=============================================================================
opts = delimitedTextImportOptions('VariableNames', {'A', 'B'}, ...
  'VariableTypes', {'double', 'string'}, 'Delimiter', ';', ...
  'DataLines', [2 Inf], 'LineEnding', {'\n'}, 'CommentStyle', {'#'});
asserts.isequal(opts.VariableNames, {'A', 'B'});
asserts.isequal(opts.SelectedVariableNames, {'A', 'B'});
asserts.isequal(opts.VariableTypes, {'double', 'string'});
asserts.isequal(opts.Delimiter, {';'});
asserts.isequal(opts.DataLines, [2 Inf]);
asserts.isequal(opts.LineEnding, {'\n'});
asserts.isequal(opts.CommentStyle, {'#'});
%=============================================================================
