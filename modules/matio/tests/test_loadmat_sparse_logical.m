%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
clear sp_log_5_4
M = [   1   1   1   0;
0   0   1   0;
0   0   1   0;
0   0   0   0;
0   0   0   0];
sp_log_5_4_ref = sparse(logical(M));
loadmat([mat_dir, 'test_logical_sparse.mat']);
assert_isequal(sp_log_5_4, sp_log_5_4_ref);
%=============================================================================
