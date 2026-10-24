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
asserts.isequal(nargin('raylinv'), 2);
asserts.isequal(nargout('raylinv'), 1);
%=============================================================================
p = [0 0.3934693402873666 0.8646647167633873 1];
R = raylinv(p, 2);
asserts.isapprox(R(1:3), [0 2 4], 1e-14);
asserts.istrue(isinf(R(4)) && R(4) > 0);
%=============================================================================
R = raylinv([0.3934693402873666 0.3934693402873666], [1 2]);
asserts.isapprox(R, [1 2], 1e-14);
%=============================================================================
R = raylinv(single([0 0.3934693402873666]), single(2));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0 2], 1e-6);
%=============================================================================
R = raylinv([-0.1 1.1 NaN 0.5 0.5], 1);
asserts.istrue(all(isnan(R(1:3))));
asserts.isapprox(R(4), 1.1774100225154747, 1e-15);
asserts.isapprox(R(5), 1.1774100225154747, 1e-15);
R = raylinv([0.5 0.5], [NaN 0]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = raylinv([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('raylinv(0.5)', _('Not enough input arguments.'));
asserts.checkerror('raylinv(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('raylinv({0.5}, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('raylinv(0.5, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
