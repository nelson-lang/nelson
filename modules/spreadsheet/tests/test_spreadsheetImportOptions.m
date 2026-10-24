%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = spreadsheetImportOptions();
asserts.isequal(class(opts), 'nelson.io.spreadsheet.SpreadsheetImportOptions');
asserts.isequal(opts.VariableNames, {'Var1'});
asserts.isequal(opts.VariableTypes, {'char'});
asserts.isequal(opts.Sheet, '');
asserts.isequal(opts.DataRange, '');
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
  'Sheet';
  'DataRange';
  'VariableNamesRange';
  'RowNamesRange';
  'VariableUnitsRange';
  'VariableDescriptionsRange';
  'ExtraColumnsRule'};
asserts.isequal(properties(opts), expectedProperties);
%=============================================================================
opts = spreadsheetImportOptions('NumVariables', 3);
asserts.isequal(opts.VariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(opts.VariableTypes, {'char', 'char', 'char'});
%=============================================================================
% setvaropts works on a spreadsheet import-options object too.
opts.VariableNames = ["A", "B", "C"];
opts.VariableTypes = ["string", "double", "datetime"];
opts = setvaropts(opts, "C", 'InputFormat', '');
asserts.isequal(class(opts.VariableOptions), 'struct');
cOpt = getvaropts(opts, "C");
asserts.isequal(cOpt.Type, 'datetime');
%=============================================================================
% Round-trip: write a workbook, then read it back honouring the declared types.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Spreadsheet table import is not stable in the portable runtime yet.');
A = ["x"; "y"; "z"];
B = [1; 2; 3];
C = [45292; 45293; 45294];
T0 = table(A, B, C);
xlsx_filename = [tempname(), '.xlsx'];
writetable(T0, xlsx_filename, 'FileType', 'spreadsheet');
opts = spreadsheetImportOptions('NumVariables', 3);
opts.VariableNames = ["A", "B", "C"];
opts.VariableTypes = ["string", "double", "datetime"];
T = readtable(xlsx_filename, opts, 'UseExcel', false);
asserts.isequal(size(T), [3 3]);
asserts.isequal(class(T.A), 'string');
asserts.isequal(class(T.B), 'double');
asserts.isequal(class(T.C), 'datetime');
asserts.isequal(T.A, ["x"; "y"; "z"]);
asserts.isequal(T.B, [1; 2; 3]);
expectedC = [datetime(2024, 1, 1); datetime(2024, 1, 2); datetime(2024, 1, 3)];
asserts.isequal(datenum(T.C), datenum(expectedC));
rmfile(xlsx_filename);
%=============================================================================
