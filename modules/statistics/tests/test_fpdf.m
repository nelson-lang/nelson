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
asserts.isequal(nargin('fpdf'), 3);
asserts.isequal(nargout('fpdf'), 1);
%=============================================================================
x = [0 0.5 1 2 Inf];
v1 = [1 2 5 10 4];
v2 = [5 10 20 30 6];
R = fpdf(x, v1, v2);
REF = [Inf 0.564473930053778 0.544878125182182 ...
       0.139963161616634 0];
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:5), REF(2:5), 1e-12);
%=============================================================================
R = fpdf([0.5 1 2], [Inf 5 Inf], [5 Inf Inf]);
asserts.isapprox(R(1:2), [0.566691106834690 0.610207606746937], 1e-12);
asserts.istrue(isnan(R(3)));
%=============================================================================
R = fpdf(single(x), single(v1), single(v2));
asserts.isequal(class(R), 'single');
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:5), single([0.564473930053778 0.544878125182182 ...
                                0.139963161616634 0]), 1e-6);
%=============================================================================
R = fpdf([-1 0 NaN 1], [2 2 2 -1], [5 5 5 5]);
asserts.isequal(R(1), 0);
asserts.isequal(R(2), 1);
asserts.istrue(isnan(R(3)));
asserts.istrue(isnan(R(4)));
%=============================================================================
R = fpdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('fpdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('fpdf({1}, 2, 3)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('fpdf(1, {2}, 3)', sprintf(_('%s must be real numeric.'), 'V1'));
asserts.checkerror('fpdf(1, 2, {3})', sprintf(_('%s must be real numeric.'), 'V2'));
%=============================================================================
