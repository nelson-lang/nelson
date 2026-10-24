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
asserts.isequal(nargin('betainv'), 3);
asserts.isequal(nargout('betainv'), 1);
%=============================================================================
p = [0 0.025 0.5 0.975 1];
a = [0.5 1 2 5 2];
b = [2 3 2 1 0.5];
R = betainv(p, a, b);
REF = [0 0.00840375865961264 0.5 0.994949236620532 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = betainv(single([0.025 0.5 0.975]), single([1 2 5]), single([3 2 1]));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([0.00840375865961264 0.5 0.994949236620532]), 1e-6);
%=============================================================================
R = betainv([-0.1 1.1 NaN 0 1], 2, 2);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 0);
asserts.isequal(R(5), 1);
%=============================================================================
R = betainv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('betainv(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('betainv({0.5}, 2, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('betainv(0.5, {2}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('betainv(0.5, 2, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isapprox(betainv(0.5, 2, 2), 0.5, 1e-12);
%=============================================================================
