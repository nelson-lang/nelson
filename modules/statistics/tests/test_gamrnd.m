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
asserts.isequal(nargin('gamrnd'), -1);
asserts.isequal(nargout('gamrnd'), 1);
%=============================================================================
rng(0);
R = gamrnd(2, 3, 2, 3);
REF = [9.287674020780702 1.846099402023371 6.441433781255375;
       11.89373327234888 12.20819690635428 1.571736634172027];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
rng(0);
U = rand(2, 3);
E = gaminv(U, 2, 3);
rng(0);
R = gamrnd(2, 3, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
U = rand(1, 3);
E = gaminv(U, [1 2 3], [1 2 4]);
rng(0);
R = gamrnd([1 2 3], [1 2 4]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
R = gamrnd(single(2), single(3), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = gamrnd(2, 3, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = gamrnd([0 -1 NaN], [3 3 3], [1 3]);
asserts.istrue(all(isnan(R)));
R = gamrnd(2, [0 -1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
R = gamrnd(2, 3, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('gamrnd(1)', _('Not enough input arguments.'));
asserts.checkerror('gamrnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('gamrnd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('gamrnd({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('gamrnd(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
