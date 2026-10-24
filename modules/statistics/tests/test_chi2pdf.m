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
asserts.isequal(nargin('chi2pdf'), 2);
asserts.isequal(nargout('chi2pdf'), 1);
%=============================================================================
x = [0.5 1 2 5];
R = chi2pdf(x, 4);
REF = [0.0973500978839256 0.151632664928158 ...
       0.183939720585721 0.102606248279874];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
x = [0 0.5 1 2 5 10];
v = [1 2 3 4 5 6];
R = chi2pdf(x, v);
REF = [Inf 0.389400391535702 0.241970724519143 ...
       0.183939720585721 0.122041521349387 0.0421121687442842];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = chi2pdf(single(x), single(v));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single(REF), 1e-6);
%=============================================================================
R = chi2pdf([-1 0 0 Inf NaN 1], [2 1 2 2 2 NaN]);
asserts.isequal(R(1), 0);
asserts.istrue(isinf(R(2)) && R(2) > 0);
asserts.isequal(R(3), 0.5);
asserts.isequal(R(4), 0);
asserts.istrue(isnan(R(5)));
asserts.istrue(isnan(R(6)));
%=============================================================================
R = chi2pdf([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('chi2pdf(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('chi2pdf({1}, 2)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('chi2pdf(1, {2})', sprintf(_('%s must be real numeric.'), 'V'));
%=============================================================================
asserts.isapprox(chi2pdf(2, 4), exp(-1) / 2, 1e-12);
asserts.istrue(isnan(chi2pdf(1, -1)));
%=============================================================================
