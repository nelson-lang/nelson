%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng(5489);
filename = [tempdir(), 'bench_write_read_matrix.csv'];
A = rand(3000, 3000);
tic();writematrix(A, filename);toc()
options = detectImportOptions(filename);
tic();B = readmatrix(filename,options);toc()
isapprox(A, B, 1e-3)
