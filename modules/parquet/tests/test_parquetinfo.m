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
asserts.isequal(info.VariableNames, {'A'; 'B'});
rmfile(filename);
%=============================================================================
