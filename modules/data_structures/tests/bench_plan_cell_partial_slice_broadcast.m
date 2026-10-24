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
value = cell(1, n);
value(:) = {0};
rhs = {'x'};
idx = 2:2:n;
tic();
value(idx) = rhs;
elapsed = toc();
ops = length(idx);
asserts.isequal(value{idx(end)}, 'x');
fprintf('BENCH_REFERENCE,cell,cell partial slice broadcast,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
