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
asserts.isequal(nargin('raylrnd'), -1);
asserts.isequal(nargout('raylrnd'), 1);
%=============================================================================
rng(0);
U = rand(2, 4);
E = 2 .* sqrt(-2 .* log1p(-U));
rng(0);
R = raylrnd(2, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(1, 3);
E = [1 2 3] .* sqrt(-2 .* log1p(-U));
rng(0);
R = raylrnd([1 2 3]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = single(2) .* sqrt(single(-2) .* log1p(-U));
rng(0);
R = raylrnd(single(2), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = raylrnd(0, [1 3]);
asserts.istrue(all(isnan(R)));
R = raylrnd(-1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = raylrnd(1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = raylrnd(single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = raylrnd(1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('raylrnd()', _('Not enough input arguments.'));
asserts.checkerror('raylrnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('raylrnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('raylrnd(1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('raylrnd({1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
