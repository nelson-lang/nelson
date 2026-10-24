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
T = table((1:6)', [10; 20; 30; 40; 50; 60], 'VariableNames', {'Id', 'Value'});
parquetwrite(filename, T, 'RowGroupHeights', 2);
rf = rowfilter(T);
R = _parquetread(filename, {}, [], (rf.Value >= 50).Expression);
asserts.isequal(size(R, 1), 2);
asserts.isequal(R.Id, [5; 6]);
R = _parquetread(filename, {}, [], (rf.Value > 100).Expression);
asserts.isequal(size(R, 1), 0);
asserts.isequal(R.Properties.VariableNames, {'Id', 'Value'});
rmfile(filename);
%=============================================================================
