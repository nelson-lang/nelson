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
  'Delimited text table import is not stable in the portable runtime yet.');
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = [1;0;1;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_1.csv'];
R = readtable(csv_filename);
asserts.isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = {'1';'''0''';'''1''';'''0''';'1'};
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_2.csv'];
R = readtable(csv_filename);
asserts.isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = [1;NaN;NaN;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2);
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_3.csv'];
R = readtable(csv_filename);
asserts.isequal(R, REF)
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = [1;NaN;NaN;0;1];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure_1 = [124; 109; 125; 117; 122];
BloodPressure_2 = [93;77;83;75;80];
REF = table(LastName, Age, Smoker, Height, Weight, BloodPressure_1, BloodPressure_2, 'VariableNames', {'Last Name','Age','Smoker','Height','Weight','BloodPressure 1','BloodPressure 2'});
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_4.csv'];
R = readtable(csv_filename);
asserts.isequal(R, REF)
%=============================================================================
% TextType 'string' returns text columns as a string array.
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_1.csv'];
R = readtable(csv_filename, 'TextType', 'string');
asserts.isequal(class(R{:, 1}), 'string');
asserts.isequal(R{:, 1}, ["Sanchez"; "Johnson"; "Li"; "Diaz"; "Brown"]);
asserts.isequal(class(R{:, 2}), 'double');
%=============================================================================
% TextType 'char' keeps text columns as a cell array of character vectors.
R = readtable(csv_filename, 'TextType', 'char');
asserts.isequal(class(R{:, 1}), 'cell');
asserts.isequal(R{:, 1}, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
%=============================================================================
% 'Range' with a leading header row keeps the variable names from that row.
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_1.csv'];
R = readtable(csv_filename, 'Range', 'A1:B3');
asserts.isequal(size(R), [2 2]);
asserts.isequal(R.Properties.VariableNames, {'LastName', 'Age'});
asserts.isequal(R{:, 1}, {'Sanchez'; 'Johnson'});
asserts.isequal(R{:, 2}, [38; 43]);
%=============================================================================
% 'Range' starting below the header reads every selected row as data.
R = readtable(csv_filename, 'Range', 'A2:C4');
asserts.isequal(size(R), [3 3]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
asserts.isequal(R{:, 1}, {'Sanchez'; 'Johnson'; 'Li'});
asserts.isequal(R{:, 2}, [38; 43; 38]);
asserts.isequal(R{:, 3}, [1; 0; 1]);
%=============================================================================
% Column-only 'Range' keeps all rows and the header names.
R = readtable(csv_filename, 'Range', 'A:C');
asserts.isequal(size(R), [5 3]);
asserts.isequal(R.Properties.VariableNames, {'LastName', 'Age', 'Smoker'});
%=============================================================================
% Row-only 'Range' keeps all columns.
R = readtable(csv_filename, 'Range', '2:4');
asserts.isequal(size(R), [3 7]);
asserts.isequal(R{:, 1}, {'Sanchez'; 'Johnson'; 'Li'});
%=============================================================================
% Numeric 'Range' [firstRow firstCol lastRow lastCol].
R = readtable(csv_filename, 'Range', [1 1 3 2]);
asserts.isequal(size(R), [2 2]);
asserts.isequal(R.Properties.VariableNames, {'LastName', 'Age'});
%=============================================================================
% 'ReadVariableNames' false discards the header row but still consumes it.
R = readtable(csv_filename, 'Range', 'A1:B3', 'ReadVariableNames', false);
asserts.isequal(size(R), [2 2]);
asserts.isequal(R.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(R{:, 1}, {'Sanchez'; 'Johnson'});
%=============================================================================
% A 'Range' beyond the last row returns an empty table with the requested columns.
R = readtable(csv_filename, 'Range', 'A10:B12');
asserts.isequal(size(R), [0 2]);
%=============================================================================
% A column upper bound past the file width is clamped to the available columns.
R = readtable(csv_filename, 'Range', 'A1:Z100');
asserts.isequal(size(R), [5 7]);
asserts.isequal(R.Properties.VariableNames, ...
{'LastName', 'Age', 'Smoker', 'Height', 'Weight', 'BloodPressure_1', 'BloodPressure_2'});
%=============================================================================
% 'Sheet' is not supported for a delimited text file.
cmd = sprintf('readtable(''%s'', ''Sheet'', 1)', csv_filename);
asserts.checkerror(cmd, _('Unsupported parameter ''Sheet'' for FileType ''text''.'));
%=============================================================================
