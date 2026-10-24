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
asserts.isequal(nargin('wblinv'), 3);
asserts.isequal(nargout('wblinv'), 1);
%=============================================================================
p = [0 0.5 0.9];
R = wblinv(p, 2, 3);
asserts.isapprox(R, [0 1.769994089001035 2.641000956907371], 1e-14);
%=============================================================================
p = [0 0.6321205588285577 0.9996645373720975 1];
R = wblinv(p, 2, 3);
asserts.isapprox(R(1:3), [0 2 4], 1e-13);
asserts.istrue(isinf(R(4)) && R(4) > 0);
%=============================================================================
asserts.isapprox(wblinv(0.5), 0.6931471805599453, 1e-15);
R = wblinv([0.6321205588285577 0.6321205588285577], [1 2], [1 3]);
asserts.isapprox(R, [1 2], 1e-13);
%=============================================================================
R = wblinv(single([0 0.6321205588285577]), single(2), single(3));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 2], 1e-6);
%=============================================================================
R = wblinv([-0.1 1.1 NaN 0.5 0.5], 1, 1);
asserts.istrue(all(isnan(R(1:3))));
asserts.isapprox(R(4), 0.6931471805599453, 1e-15);
asserts.isapprox(R(5), 0.6931471805599453, 1e-15);
R = wblinv([0.5 0.5], [NaN 0], [1 1]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = wblinv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('wblinv(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('wblinv({0.5}, 1, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('wblinv(0.5, {1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('wblinv(0.5, 1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
