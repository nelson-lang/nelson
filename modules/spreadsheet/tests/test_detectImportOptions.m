%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Delimited text table import options are not stable in the portable runtime yet.');
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_1.csv'];
options = detectImportOptions(csv_filename);
asserts.isequal(class(options), 'nelson.io.text.DelimitedTextImportOptions');
asserts.isequal(options.Delimiter, {','});
asserts.isequal(options.VariableNamesLine, 1);
asserts.isequal(options.VariableNames, {'Row', 'Age', 'Height', 'Weight', 'BloodPressure_1', 'BloodPressure_2'});
asserts.isequal(options.SelectedVariableNames, options.VariableNames);
asserts.isequal(options.VariableTypes, {'char', 'char', 'char', 'char', 'char', 'char'});
asserts.isequal(options.DataLines, [2 Inf]);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_2.csv'];
options = detectImportOptions(csv_filename);
asserts.isequal(class(options), 'nelson.io.text.DelimitedTextImportOptions');
asserts.isequal(options.DataLines, [6 Inf]);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_3.csv'];
options = detectImportOptions(csv_filename);
asserts.isequal(class(options), 'nelson.io.text.DelimitedTextImportOptions');
asserts.isequal(options.DataLines, [1 Inf]);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_1.csv'];
invalidOptions = struct();
asserts.checkerror('readcell(csv_filename, invalidOptions);', _('Import options object expected.'));
asserts.checkerror('readcell(csv_filename, invalidOptions);', _('Import options object expected.'), 'Nelson:spreadsheet:importOptionsExpected');
asserts.checkerror('readmatrix(csv_filename, invalidOptions);', _('Import options object expected.'));
asserts.checkerror('readmatrix(csv_filename, invalidOptions);', _('Import options object expected.'), 'Nelson:spreadsheet:importOptionsExpected');
asserts.checkerror('readtable(csv_filename, invalidOptions);', _('Import options object expected.'));
asserts.checkerror('readtable(csv_filename, invalidOptions);', _('Import options object expected.'), 'Nelson:spreadsheet:importOptionsExpected');
%=============================================================================
