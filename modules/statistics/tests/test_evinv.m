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
asserts.isequal(nargin('evinv'), 3);
asserts.isequal(nargout('evinv'), 1);
%=============================================================================
p = [-0.1 0 0.025 0.5 0.975 1 1.1 NaN];
R = evinv(p);
asserts.istrue(isnan(R(1)));
asserts.istrue(isinf(R(2)) && R(2) < 0);
asserts.isapprox(R(3:5), [-3.676247257954177 -0.3665129205816643 ...
                         1.305322740963236], 1e-15);
asserts.istrue(isinf(R(6)) && R(6) > 0);
asserts.istrue(isnan(R(7)));
asserts.istrue(isnan(R(8)));
%=============================================================================
R = evinv([0.1 0.25 0.5 0.75 0.9], [0 0 1 1 2], [1 2 1 3 2]);
E = [-2.250367327312445 -2.491798647414476 0.6334870794183356 ...
     1.979902779934843 3.668064890495912];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = evinv(single([0.1 0.5 0.9]), single(0), single(1));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [-2.250367327312445 -0.3665129205816643 ...
                            0.8340324452479560], 1e-6);
%=============================================================================
R = evinv([0.5 NaN 0.5 0.5], [0 0 NaN 0], [1 1 1 0]);
asserts.isapprox(R(1), -0.3665129205816643, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = evinv([]);
asserts.isequal(size(R), [0 0]);
R = evinv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('evinv(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('evinv({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('evinv(0.5, {0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('evinv(0.5, 0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
