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
asserts.isequal(nargin('gevinv'), 4);
asserts.isequal(nargout('gevinv'), 1);
%=============================================================================
p = [0 0.1 0.5 0.9 1];
R = gevinv(p, 0.2, 1, 0);
REF = [-5 -0.7681831196358124 0.3802804256950254 2.842137032512669 Inf];
asserts.isapprox(R(1:4), REF(1:4), 1e-15);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = gevinv(p, -0.2, 1, 0);
asserts.istrue(isinf(R(1)) && R(1) < 0);
asserts.istrue(R(5) == 5);
%=============================================================================
R = gevinv(single([0.1 0.5 0.9]), single(0), single(1), single(0));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [-0.83403245 0.36651292 2.2503674], 1e-6);
%=============================================================================
R = gevinv([-0.1 NaN 0.5 0.5 0.5], [0 0 0 NaN 0], [1 1 1 1 0], [0 0 0 0 NaN]);
asserts.istrue(isnan(R(1)));
asserts.istrue(isnan(R(2)));
asserts.isapprox(R(3), 0.3665129205816643, 1e-15);
asserts.istrue(isnan(R(4)));
asserts.istrue(isnan(R(5)));
R = gevinv([], [], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('gevinv(0.5, 0, 1)', _('Not enough input arguments.'));
asserts.checkerror('gevinv(ones(2), ones(3), 1, 0)', _('Size mismatch on input arguments'));
asserts.checkerror('gevinv({0.5}, 0, 1, 0)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('gevinv(0.5, {0}, 1, 0)', sprintf(_('%s must be real numeric.'), 'K'));
asserts.checkerror('gevinv(0.5, 0, {1}, 0)', sprintf(_('%s must be real numeric.'), 'SIGMA'));
asserts.checkerror('gevinv(0.5, 0, 1, {0})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
