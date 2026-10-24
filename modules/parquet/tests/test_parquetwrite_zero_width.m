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
% A Parquet file cannot keep the height of a table without variables.
filename = [tempname(), '.parquet'];
T = table('Size', [2 0], 'VariableTypes', {});
asserts.checkerror('parquetwrite(filename, T)', _('Second argument must have at least one variable.'));
asserts.isfalse(isfile(filename));
%=============================================================================
asserts.checkerror('parquetwrite(filename, table())', _('Second argument must have at least one variable.'));
%=============================================================================
TT = timetable(seconds([1; 2]));
asserts.checkerror('parquetwrite(filename, TT)', _('Second argument must have at least one variable.'));
%=============================================================================
% A nested table without variables is rejected with its path.
inner = table('Size', [2 0], 'VariableTypes', {});
msg = _('T.inner is a table with zero variables. Zero variable tables are not supported.');
asserts.checkerror('parquetwrite(filename, table([1; 2], inner))', msg);
asserts.isfalse(isfile(filename));
asserts.checkerror('parquetwrite(filename, table(inner))', msg);
T = table([1; 2], table([1; 2], inner, 'VariableNames', {'a', 'b'}), 'VariableNames', {'p', 'q'});
asserts.checkerror('parquetwrite(filename, T)', _('T.q.b is a table with zero variables. Zero variable tables are not supported.'));
T = table([1; 2], timetable(seconds([1; 2]), inner), 'VariableNames', {'p', 'q'});
asserts.checkerror('parquetwrite(filename, T)', _('T.q.inner is a table with zero variables. Zero variable tables are not supported.'));
TT = timetable(seconds([1; 2]), [1; 2], inner);
asserts.checkerror('parquetwrite(filename, TT)', _('TT.inner is a table with zero variables. Zero variable tables are not supported.'));
asserts.isfalse(isfile(filename));
%=============================================================================
% A nested timetable without variables keeps its row times.
T = table([1; 2], timetable(seconds([1; 2])), 'VariableNames', {'p', 'q'});
parquetwrite(filename, T);
R = parquetread(filename);
asserts.isequal(R.Properties.VariableNames, {'p', 'q'});
asserts.istrue(istable(R.q));
asserts.isequal(R.q.Properties.VariableNames, {'Time'});
asserts.isequal(R.q.Time, seconds([1; 2]));
rmfile(filename);
%=============================================================================
% A nested table with no rows but some variables is written.
T = table(zeros(0, 1), table('Size', [0 2], 'VariableTypes', {'double', 'double'}));
parquetwrite(filename, T);
R = parquetread(filename);
asserts.isequal(size(R), [0 2]);
asserts.istrue(istable(R.Var2));
asserts.isequal(R.Var2.Properties.VariableNames, {'Var1', 'Var2'});
rmfile(filename);
%=============================================================================
