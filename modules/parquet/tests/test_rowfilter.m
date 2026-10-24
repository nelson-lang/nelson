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
T = table((1:5)', [3; 4; 5; 6; 7], 'VariableNames', {'A', 'B'});
parquetwrite(filename, T);
rf = rowfilter({'A', 'B'});
R = parquetread(filename, 'RowFilter', rf.A >= 3 & rf.B < 7);
asserts.isequal(R.A, [3; 4]);
asserts.isequal(R.B, [5; 6]);
rmfile(filename);
%=============================================================================
