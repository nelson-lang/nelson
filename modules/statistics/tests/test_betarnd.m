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
asserts.isequal(nargin('betarnd'), -1);
asserts.isequal(nargout('betarnd'), 1);
%=============================================================================
rng(0, 'twister');
R = betarnd(2, 5, 2, 3);
REF = [0.4331491952906957 0.1063543321687892 0.3250939404965425;
       0.5170351491539715 0.5262909727485290 0.09128371257013587];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(0, 'twister');
U = rand(2, 3);
E = betainv(U, 2, 5);
rng(0, 'twister');
R = betarnd(2, 5, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0, 'twister');
U = rand(1, 3);
E = betainv(U, [1 2 3], [4 5 6]);
rng(0, 'twister');
R = betarnd([1 2 3], [4 5 6]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0, 'twister');
R = betarnd(single(2), single(5), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = betarnd(2, 5, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = betarnd([0 -1 NaN], [5 5 5], [1 3]);
asserts.istrue(all(isnan(R)));
R = betarnd(2, [0 -1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
R = betarnd(2, 5, 0, 3);
asserts.isequal(size(R), [0 3]);
R = betarnd(single(2), single(5), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
%=============================================================================
asserts.checkerror('betarnd(1)', _('Not enough input arguments.'));
asserts.checkerror('betarnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('betarnd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('betarnd({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('betarnd(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
