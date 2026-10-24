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
asserts.isequal(nargin('unifinv'), 3);
asserts.isequal(nargout('unifinv'), 1);
%=============================================================================
p = [0 0.25 0.5 0.75 1];
R = unifinv(p);
asserts.isequal(R, p);
%=============================================================================
asserts.isequal(unifinv(0.5), 0.5);
R = unifinv([0.75 0.99], -1, 1);
asserts.isapprox(R, [0.5 0.98], 1e-15);
%=============================================================================
R = unifinv(p, -1, 3);
asserts.isequal(R, [-1 0 1 2 3]);
%=============================================================================
R = unifinv([0.25 0.5 0.75], [-1 0 1], [1 2 5]);
asserts.isequal(R, [-0.5 1 4]);
%=============================================================================
R = unifinv(single([0 0.5 1]), single(0), single(2));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([0 1 2]));
%=============================================================================
R = unifinv([-0.1 1.1 NaN 0.5 0.5], 0, 1);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 0.5);
asserts.isequal(R(5), 0.5);
R = unifinv([0.5 0.5], [1 NaN], [1 2]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = unifinv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unifinv(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('unifinv(0.5, 0)', _('Not enough input arguments.'));
asserts.checkerror('unifinv({0.5}, 0, 1)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('unifinv(0.5, {0}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('unifinv(0.5, 0, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isequal(unifinv(0.25, 0, 2), 0.5);
%=============================================================================
