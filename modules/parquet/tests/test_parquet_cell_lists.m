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
C = {int32([1 2]), 1; int32([3 4 5]), 2; int32(6), 3};
T = cell2table(C, 'VariableNames', {'Items', 'Id'});
parquetwrite(filename, T);
R = parquetread(filename);
asserts.isequal(R.Id, T.Id);
asserts.istrue(iscell(R.Items));
asserts.isequal(R.Items{1}, T.Items{1}(:));
asserts.isequal(R.Items{2}, T.Items{2}(:));
asserts.isequal(R.Items{3}, T.Items{3});
rmfile(filename);
%=============================================================================
