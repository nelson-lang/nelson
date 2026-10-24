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
T = table(int32([1; 2; 3]), ["aa"; "bb"; "cc"], 'VariableNames', {'Id', 'Name'});
parquetwrite(filename, T);
R = parquetread(filename);
asserts.istrue(istable(R));
asserts.isequal(R.Id, T.Id);
asserts.isequal(R.Name, T.Name);
rmfile(filename);
%=============================================================================
