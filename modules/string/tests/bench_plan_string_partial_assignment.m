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
value = strings(1, n);
value(:) = string('base');
token = string('token');
idx = 2:2:n;
tic();
value(idx) = string('token');
elapsed = toc();
ops = length(idx);
asserts.isequal(value(idx(end)), token);
fprintf('BENCH_REFERENCE,string,string partial assignment,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
