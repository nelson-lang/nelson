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
T = table((1:6)', (11:16)', 'VariableNames', {'A', 'B'});
parquetwrite(filename, T, 'RowGroupHeights', 2);
info = parquetinfo(filename);
asserts.istrue(info.NumRowGroups >= 3);
R = parquetread(filename, 'RowGroups', 2);
asserts.isequal(R.A, [3; 4]);
rmfile(filename);
%=============================================================================
