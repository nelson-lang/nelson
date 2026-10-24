%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('lognrnd'), -1);
asserts.isequal(nargout('lognrnd'), 1);
%=============================================================================
rng(0);
Z = randn(2, 4);
E = exp(1 + 2 .* Z);
rng(0);
R = lognrnd(1, 2, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
Z = randn(1, 3);
E = exp([0 1 2] + [1 0 2] .* Z);
rng(0);
R = lognrnd([0 1 2], [1 0 2]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
Z = randn(2, 3, 'single');
E = exp(single(2) .* Z);
rng(0);
R = lognrnd(single(0), single(2), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = lognrnd(1, 0, [1 3]);
asserts.isapprox(R, exp(ones(1, 3)), 1e-15);
R = lognrnd(0, -1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = lognrnd(0, 1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = lognrnd(single(0), single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = lognrnd(0, 1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('lognrnd(0)', _('Not enough input arguments.'));
asserts.checkerror('lognrnd([0 1], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('lognrnd(0, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('lognrnd(0, 1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('lognrnd({0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('lognrnd(0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
