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
asserts.isequal(nargin('geoinv'), 2);
asserts.isequal(nargout('geoinv'), 1);
%=============================================================================
y = [0 0.25 0.4375 0.578125 0.9 1];
R = geoinv(y, 0.25);
asserts.isapprox(R(1:5), [0 0 1 2 8], 1e-15);
asserts.istrue(isinf(R(6)) && R(6) > 0);
%=============================================================================
R = geoinv([0.25 0.5 0.75], [0.25 0.5 1]);
asserts.isapprox(R, [0 0 0], 1e-15);
R = geoinv([0 0.5], [0 0]);
asserts.isequal(R(1), 0);
asserts.istrue(isinf(R(2)) && R(2) > 0);
%=============================================================================
R = geoinv(single([0.25 0.4375]), single(0.25));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 1], 1e-7);
%=============================================================================
R = geoinv([-0.1 1.1 NaN 0.5], 0.25);
asserts.istrue(all(isnan(R(1:3))));
asserts.isapprox(R(4), 2, 1e-15);
R = geoinv([0.5 0.5], [-0.1 1.1]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = geoinv([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('geoinv(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('geoinv({0.5}, 0.5)', sprintf(_('%s must be real numeric.'), 'Y'));
asserts.checkerror('geoinv(0.5, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
asserts.isequal(geoinv(geocdf(2, 0.25), 0.25), 2);
%=============================================================================
