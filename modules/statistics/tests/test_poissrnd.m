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
asserts.isequal(nargin('poissrnd'), -1);
asserts.isequal(nargout('poissrnd'), 1);
%=============================================================================
rng(0);
R = poissrnd(4, 2, 3);
REF = [6 2 5; 7 7 2];
asserts.isequal(R, REF);
%=============================================================================
rng(0);
U = rand(2, 3);
E = poissinv(U, 4);
rng(0);
R = poissrnd(4, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
U = rand(1, 3);
E = poissinv(U, [1 4 10]);
rng(0);
R = poissrnd([1 4 10]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
R = poissrnd(single(4), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = poissrnd(4, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = poissrnd([-1 Inf NaN], [1 3]);
asserts.istrue(all(isnan(R)));
asserts.isequal(poissrnd(0, 1, 3), zeros(1, 3));
R = poissrnd(4, 0, 3);
asserts.isequal(size(R), [0 3]);
R = poissrnd(single(4), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
%=============================================================================
asserts.checkerror('poissrnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('poissrnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('poissrnd({1})', _('LAMBDA must be a nonnegative numeric array.'));
%=============================================================================
