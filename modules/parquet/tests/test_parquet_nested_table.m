%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
filename = [tempname(), '.parquet'];
Nested = table(int16([10; 20; 30]), [1.5; 2.5; 3.5], 'VariableNames', {'Code', 'Value'});
T = table((1:3)', Nested, 'VariableNames', {'Id', 'Nested'});
parquetwrite(filename, T);
R = parquetread(filename);
asserts.isequal(R.Id, T.Id);
asserts.istrue(istable(R.Nested));
asserts.isequal(R.Nested.Code, T.Nested.Code);
asserts.isequal(R.Nested.Value, T.Nested.Value);
rmfile(filename);
%=============================================================================
% A nested timetable is written as a table whose first variable is its row times.
filename = [tempname(), '.parquet'];
Inner = timetable(seconds([1; 2]), [10; 20]);
T = table([1; 2], Inner, 'VariableNames', {'Id', 'Inner'});
parquetwrite(filename, T);
R = parquetread(filename);
asserts.isequal(R.Id, T.Id);
asserts.istrue(istable(R.Inner));
asserts.isequal(R.Inner.Properties.VariableNames, {'Time', 'Var1'});
asserts.isequal(R.Inner.Time, seconds([1; 2]));
asserts.isequal(R.Inner.Var1, [10; 20]);
rmfile(filename);
%=============================================================================
TT = timetable(seconds([1; 2]), table([10; 20]), 'VariableNames', {'Inner'});
parquetwrite(filename, TT);
R = parquetread(filename);
asserts.isequal(R.Properties.VariableNames, {'Time', 'Inner'});
asserts.isequal(R.Time, seconds([1; 2]));
asserts.isequal(R.Inner.Var1, [10; 20]);
rmfile(filename);
%=============================================================================
