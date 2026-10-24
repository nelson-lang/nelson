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
value = strings(n, 1);
token = string('token');
tic();
value(:) = 'token';
elapsed = toc();
ops = n;
asserts.isequal(sum(value == token), n);
fprintf('BENCH_REFERENCE,string,string setNDimSubset full scalar broadcast,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
