%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 12000;
value = cell(n, 1);
rhs = {'x'};
tic();
value(:) = rhs;
elapsed = toc();
ops = n;
asserts.isequal(value{n}, 'x');
fprintf('BENCH_REFERENCE,cell,cell full slice broadcast,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
