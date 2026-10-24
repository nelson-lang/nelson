%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = linspace(0, 4 * pi, 10);
y = sin(x);
p = polyfit(x, y, 7);
REF = [-0.0001 0.0028 -0.0464 0.3702 -1.3808 1.9084 -0.1141 0.0002];
asserts.isapprox(p, REF, 1e-4);
%=============================================================================
x = [1, 2, 3; 4, 5, 6];
y = [0, 0, 1; 1, 0, 0];
p = polyfit(x, y, 5);
REF = [0.0000 0.0833 -1.1667 5.4167 -9.3333 5.0000];
asserts.isapprox(p, REF, 1e-4);
%=============================================================================
asserts.isapprox(polyfit(1:3, [1 4 9], 2), [1 0 0], 1e-12);
%=============================================================================
% Second output S (structure for error estimation) and third output mu.
[p, S] = polyfit([1 2 3], [2 4 6], 1);
asserts.isapprox(p, [2 0], 1e-10);
asserts.isequal(S.df, 1);
asserts.istrue(isfield(S, 'R'));
asserts.istrue(isfield(S, 'normr'));
asserts.istrue(S.normr < 1e-10);
[p3, S3, mu] = polyfit([10 20 30], [1 4 9], 2);
asserts.isapprox(mu, [20; 10], 1e-10);
%=============================================================================
