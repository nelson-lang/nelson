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
asserts.isequal(nargin('poissinv'), 2);
asserts.isequal(nargout('poissinv'), 1);
%=============================================================================
y = [0 0.025 0.5 0.975 1];
lambda = [0.5 1 2 4 8];
R = poissinv(y, lambda);
asserts.isequal(R(1:4), [0 0 2 8]);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = poissinv([0.1 0.9 1], 0);
asserts.isequal(R, [0 0 0]);
%=============================================================================
R = poissinv(single(y), single(lambda));
asserts.isequal(class(R), 'single');
asserts.isequal(double(R(1:4)), [0 0 2 8]);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = poissinv([-0.1 1.1 NaN 0 1], 2);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 0);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = poissinv([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('poissinv(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('poissinv({0.5}, 2)', sprintf(_('%s must be real numeric.'), 'Y'));
asserts.checkerror('poissinv(0.5, {2})', _('LAMBDA must be a nonnegative numeric array.'));
%=============================================================================
asserts.isequal(poissinv(poisscdf(2, 3), 3), 2);
%=============================================================================
