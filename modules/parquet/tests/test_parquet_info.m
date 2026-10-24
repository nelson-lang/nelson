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
T = table([1; 2; 3], ["x"; "y"; "z"], 'VariableNames', {'A', 'B'});
parquetwrite(filename, T);
info = parquetinfo(filename);
asserts.isequal(class(info), 'nelson.io.parquet.ParquetInfo');
asserts.isequal(info.NumRows, 3);
asserts.isequal(info.NumVariables, 2);
asserts.isequal(info.VariableNames, {'A'; 'B'});
asserts.istrue(info.FileSize > 0);
rmfile(filename);

filename = [tempname(), '.parquet'];
Time = datetime(2026, 1, 1) + days(0:2)';
Value = [10; 20; 30];
TT = timetable(Time, Value);
parquetwrite(filename, TT, 'RowGroupHeights', 2);
info = parquetinfo(filename);
asserts.isequal(info.NumRows, 3);
asserts.isequal(info.NumVariables, 2);
asserts.isequal(info.NumRowGroups, 2);
asserts.isequal(info.RowGroupHeights, [2; 1]);
asserts.isequal(info.VariableNames, {'Time'; 'Value'});
asserts.isequal(info.VariableTypes, {'datetime'; 'double'});
asserts.isequal(info.VariableCompression, {'snappy'; 'snappy'});
asserts.isequal(info.VariableEncoding, {'dictionary'; 'dictionary'});
asserts.isequal(info.Version, '2.0');
rmfile(filename);
%=============================================================================
