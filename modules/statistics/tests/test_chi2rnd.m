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
asserts.isequal(nargin('chi2rnd'), -1);
asserts.isequal(nargout('chi2rnd'), 1);
%=============================================================================
rng(0);
R = chi2rnd(4, 2, 3);
REF = [6.191782680520468 1.230732934682247 4.294289187503583;
       7.929155514899250 8.138797937569517 1.047824422781352];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(0);
U = rand(2, 3);
E = chi2inv(U, 4);
rng(0);
R = chi2rnd(4, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
U = rand(1, 3);
E = chi2inv(U, [1 2 3]);
rng(0);
R = chi2rnd([1 2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
R = chi2rnd(single(4), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = chi2rnd(4, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = chi2rnd([0 -1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
R = chi2rnd(4, 0, 3);
asserts.isequal(size(R), [0 3]);
R = chi2rnd(single(4), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
%=============================================================================
asserts.checkerror('chi2rnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('chi2rnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('chi2rnd({1})', sprintf(_('%s must be real numeric.'), 'V'));
%=============================================================================
