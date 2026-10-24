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
asserts.isequal(nargin('wblrnd'), -1);
asserts.isequal(nargout('wblrnd'), 1);
%=============================================================================
rng(0);
R = wblrnd(2, 3, 2, 3);
E = [2.380352079886876 1.028019877901595 2.000432593216861;
     2.663617858664475 2.694796270738094 0.9363898340233198];
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 4);
E = 2 .* (-log1p(-U)) .^ (1 / 3);
rng(0);
R = wblrnd(2, 3, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(1, 3);
E = [1 2 3] .* (-log1p(-U)) .^ (1 ./ [1 2 3]);
rng(0);
R = wblrnd([1 2 3], [1 2 3]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = single(2) .* (-log1p(-U)) .^ single(1 / 3);
rng(0);
R = wblrnd(single(2), single(3), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = wblrnd(0, 1, [1 3]);
asserts.istrue(all(isnan(R)));
R = wblrnd(1, -1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = wblrnd(1, 1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = wblrnd(single(1), single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = wblrnd(1, 1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('wblrnd(1)', _('Not enough input arguments.'));
asserts.checkerror('wblrnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('wblrnd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('wblrnd(1, 1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('wblrnd({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('wblrnd(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
