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
T = table(complex([1; 2], [3; 4]), 'VariableNames', {'A'});
asserts.checkerror('parquetwrite(filename, T)', _('Complex and sparse variables are not supported by parquetwrite.'));
T = table(sparse([1; 2]), 'VariableNames', {'A'});
asserts.checkerror('parquetwrite(filename, T)', _('Complex and sparse variables are not supported by parquetwrite.'));
%=============================================================================
