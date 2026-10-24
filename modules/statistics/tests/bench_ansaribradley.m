%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
x = randn(60000, 1);
y = randn(60000, 1) * 1.05;
tic();
[h, p, stats] = ansaribradley(x, y, 'Method', 'approximate');
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfield(stats, 'W'));
asserts.istrue(isfield(stats, 'Wstar'));
asserts.istrue(t < 10);
%=============================================================================
X = randn(12000, 8);
Y = randn(12000, 8) .* repmat(1:8, 12000, 1) / 8;
tic();
[h2, p2, stats2] = ansaribradley(X, Y, 'Method', 'approximate');
t2 = toc();
asserts.isequal(size(h2), [1 8]);
asserts.isequal(size(p2), [1 8]);
asserts.isequal(size(stats2.W), [1 8]);
asserts.istrue(all(isfinite(p2)));
asserts.istrue(t2 < 10);
%=============================================================================
