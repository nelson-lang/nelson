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
asserts.isequal(nargin('logninv'), 5);
asserts.isequal(nargout('logninv'), 3);
%=============================================================================
p = [0 0.15865525393145707 0.5 0.8413447460685429 1];
R = logninv(p);
E = [0 exp(-1) 1 exp(1) Inf];
asserts.isapprox(R(1:4), E(1:4), 1e-14);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = logninv([0.15865525393145707 0.5 0.8413447460685429], 1, 2);
E = [exp(-1) exp(1) exp(3)];
asserts.isapprox(R, E, 1e-12);
%=============================================================================
[x, xLo, xUp] = logninv(0.8413447460685429, 0, 1, [0.01 0; 0 0.04]);
asserts.isapprox(x, exp(1), 1e-14);
asserts.isapprox(xLo, 1.7537190944221677, 1e-14);
asserts.isapprox(xUp, 4.213363544043105, 1e-14);
%=============================================================================
R = logninv(single([0.5 0.8413447460685429]), single(0), single(1));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [1 exp(1)], 1e-6);
%=============================================================================
R = logninv([-0.1 1.1 NaN 0.5 0.5], 0, 1);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 1);
asserts.isequal(R(5), 1);
R = logninv([0.5 0.5], [NaN 0], [1 0]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = logninv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('logninv(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('logninv({0.5}, 0, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('logninv(0.5, {0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('logninv(0.5, 0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
