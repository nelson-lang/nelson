%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% setvaropts stores per-variable import settings and returns the updated object.
opts = delimitedTextImportOptions('NumVariables', 6);
opts.VariableNames = ["label", "name", "when", "amount", "flag", "note"];
opts.VariableTypes = ["categorical", "string", "datetime", "double", "string", "string"];
opts = setvaropts(opts, ["name", "note"], 'WhitespaceRule', 'preserve');
opts = setvaropts(opts, ["label", "name", "note"], 'EmptyFieldRule', 'auto');
opts = setvaropts(opts, "when", 'InputFormat', 'MM/dd/yy');
asserts.isequal(class(opts.VariableOptions), 'struct');
asserts.isequal(size(opts.VariableOptions), [1 6]);
%=============================================================================
% getvaropts reads back the stored per-variable settings.
nameOpt = getvaropts(opts, "name");
asserts.isequal(nameOpt.WhitespaceRule, 'preserve');
asserts.isequal(nameOpt.EmptyFieldRule, 'auto');
whenOpt = getvaropts(opts, "when");
asserts.isequal(whenOpt.InputFormat, 'MM/dd/yy');
asserts.isequal(whenOpt.Type, 'datetime');
%=============================================================================
% getvaropts accepts a numeric selection and returns several entries.
twoOpts = getvaropts(opts, [1 2]);
asserts.isequal(numel(twoOpts), 2);
%=============================================================================
% setvaropts validates the selection.
cmd = 'setvaropts(opts, "unknownVar", ''WhitespaceRule'', ''trim'')';
asserts.checkerror(cmd, _('Unrecognized variable name ''unknownVar''.'));
%=============================================================================
% setvartype updates both the declared type and the per-variable Type entry.
opts2 = setvartype(opts, "flag", 'double');
asserts.isequal(opts2.VariableTypes{5}, 'double');
flagOpt = getvaropts(opts2, "flag");
asserts.isequal(flagOpt.Type, 'double');
%=============================================================================
% readtable honours the declared variable types and the datetime InputFormat.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Delimited text table import is not stable in the portable runtime yet.');
csv_filename = [modulepath('spreadsheet'), '/tests/test_setvaropts.csv'];
opts = delimitedTextImportOptions('NumVariables', 6);
opts.DataLines = [2 Inf];
opts.Delimiter = ",";
opts.VariableNames = ["label", "name", "when", "amount", "flag", "note"];
opts.VariableTypes = ["categorical", "string", "datetime", "double", "string", "string"];
opts = setvaropts(opts, "when", 'InputFormat', 'MM/dd/yy');
T = readtable(csv_filename, opts);
asserts.isequal(size(T), [3 6]);
asserts.isequal(class(T.label), 'categorical');
asserts.isequal(class(T.name), 'string');
asserts.isequal(class(T.when), 'datetime');
asserts.isequal(class(T.amount), 'double');
asserts.isequal(class(T.flag), 'string');
asserts.isequal(class(T.note), 'string');
%=============================================================================
expectedWhen = [datetime(2023, 12, 31); datetime(2024, 1, 15); datetime(2024, 2, 29)];
asserts.isequal(datenum(T.when), datenum(expectedWhen));
asserts.isequal(T.amount, [10; 20; 30]);
asserts.isequal(T.name, ["file one"; "file two"; "file three"]);
asserts.isequal(T.flag, ["1"; "0"; "1"]);
asserts.isequal(cellstr(T.label), {'alpha'; 'beta'; 'gamma'});
%=============================================================================
