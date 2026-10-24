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
asserts.isequal(nargin('frnd'), -1);
asserts.isequal(nargout('frnd'), 1);
%=============================================================================
rng(0);
R = frnd(5, 7, 2, 3);
REF = [2.066582351908887 0.3407914473165244 1.284107396864324;
       2.969349652963443 3.092620571073041 0.2927921622872169];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(0);
U = rand(2, 3);
E = finv(U, 5, 7);
rng(0);
R = frnd(5, 7, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
U = rand(1, 3);
E = finv(U, [3 5 7], [4 6 8]);
rng(0);
R = frnd([3 5 7], [4 6 8]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
R = frnd(single(5), single(7), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = frnd(5, 7, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = frnd([0 -1 NaN], [5 5 5], [1 3]);
asserts.istrue(all(isnan(R)));
R = frnd(5, [0 -1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
R = frnd(5, 7, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('frnd(1)', _('Not enough input arguments.'));
asserts.checkerror('frnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('frnd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('frnd({1}, 1)', sprintf(_('%s must be real numeric.'), 'V1'));
asserts.checkerror('frnd(1, {1})', sprintf(_('%s must be real numeric.'), 'V2'));
%=============================================================================
