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
T = table([1; NaN; Inf], ["a"; missing; "b""c"], [true; false; true], [1 2; 3 4; 5 6], ...
  'VariableNames', {'A', 'B', 'C', 'D'});
%=============================================================================
% default: one object per row, indentation of four spaces
filename = [tempname(), '.json'];
writetable(T, filename);
expected = ['[', newline, ...
  '    {', newline, '        "A": 1,', newline, '        "B": "a",', newline, ...
  '        "C": true,', newline, '        "D": [1,2]', newline, '    },', newline, ...
  '    {', newline, '        "A": NaN,', newline, '        "B": null,', newline, ...
  '        "C": false,', newline, '        "D": [3,4]', newline, '    },', newline, ...
  '    {', newline, '        "A": Inf,', newline, '        "B": "b\"c",', newline, ...
  '        "C": true,', newline, '        "D": [5,6]', newline, '    }', newline, ']'];
asserts.isequal(fileread(filename), expected);
%=============================================================================
% PrettyPrint false and PreserveInfAndNaN false
writetable(T, filename, 'PrettyPrint', false, 'PreserveInfAndNaN', false);
expected = ['[{"A":1,"B":"a","C":true,"D":[1,2]},{"A":null,"B":null,"C":false,"D":[3,4]},', ...
  '{"A":null,"B":"b\"c","C":true,"D":[5,6]}]'];
asserts.isequal(fileread(filename), expected);
%=============================================================================
% round trip
R = readtable(filename);
asserts.isequal(R.Properties.VariableNames, {'A', 'B', 'C', 'D'});
asserts.isequal(R.A, [1; NaN; NaN]);
asserts.isequal(R.B, T.B);
asserts.isequal(R.C, T.C);
asserts.isequal(R.D, T.D);
%=============================================================================
% full precision numbers, text types, FileType json on another extension
x = [pi; -1e-300; 123456789012; 0.1];
T = table(x, {'u'; 'v'; 'w'; 'x'}, categorical({'r'; 'g'; 'r'; 'b'}), int8([1; -2; 3; 4]), ...
  'VariableNames', {'x', 'c', 'k', 'i'});
txtname = [tempname(), '.txt'];
writetable(T, txtname, 'FileType', 'json');
R = readtable(txtname, 'FileType', 'json');
asserts.isequal(R.x, x);
asserts.isequal(R.c, ["u"; "v"; "w"; "x"]);
asserts.isequal(R.k, ["r"; "g"; "r"; "b"]);
asserts.isequal(R.i, [1; -2; 3; 4]);
%=============================================================================
% WriteRowNames uses the first dimension name as key
T = table([10; 20], 'VariableNames', {'V'}, 'RowNames', {'r1', 'r2'});
writetable(T, filename, 'WriteRowNames', true, 'PrettyPrint', false);
asserts.isequal(fileread(filename), '[{"Row":"r1","V":10},{"Row":"r2","V":20}]');
R = readtable(filename, 'RowNamesSelector', '/Row');
asserts.isequal(R.Properties.RowNames, {'r1'; 'r2'});
asserts.isequal(R.V, [10; 20]);
writetable(T, filename, 'PrettyPrint', false);
asserts.isequal(fileread(filename), '[{"V":10},{"V":20}]');
%=============================================================================
% datetime, duration and missing values
d = datetime(2024, 1, 2, 3, 4, 5) + days([0; 1]);
d(2) = NaT;
T = table(d, hours([1; NaN]), 'VariableNames', {'d', 'h'});
writetable(T, filename, 'PrettyPrint', false);
asserts.isequal(fileread(filename), '[{"d":"02-Jan-2024 03:04:05","h":"1 hr"},{"d":null,"h":null}]');
%=============================================================================
% empty table
T = table(zeros(0, 1), 'VariableNames', {'a'});
writetable(T, filename);
asserts.isequal(fileread(filename), '[]');
%=============================================================================
asserts.checkerror('writetable(table(1), filename, ''Delimiter'', '','')', _('Property name not supported.'));
asserts.checkerror('writetable(table(1), filename, ''Delimiter'', '','')', _('Property name not supported.'), 'Nelson:spreadsheet:propertyNotSupported');
%=============================================================================
