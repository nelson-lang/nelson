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
T = table([1; 2], [10; 20], 'VariableNames', {'A', 'B'});
parquetwrite(filename, T);
asserts.istrue(isfile(filename));
R = parquetread(filename);
asserts.isequal(R.A, T.A);
asserts.isequal(R.B, T.B);
rmfile(filename);
%=============================================================================
