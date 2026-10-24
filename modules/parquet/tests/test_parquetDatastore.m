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
filename1 = [tempname(), '.parquet'];
filename2 = [tempname(), '.parquet'];
parquetwrite(filename1, table([1; 2], 'VariableNames', {'A'}));
parquetwrite(filename2, table([3; 4], 'VariableNames', {'A'}));
pds = parquetDatastore({filename1, filename2});
asserts.istrue(hasdata(pds));
Tall = readall(pds);
asserts.isequal(Tall.A, [1; 2; 3; 4]);
rmfile(filename1);
rmfile(filename2);
%=============================================================================
