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
asserts.isequal(nargin('nbinrnd'), -1);
asserts.isequal(nargout('nbinrnd'), 1);
%=============================================================================
rng(0);
R = nbinrnd(3, 0.4, 2, 3);
REF = [7 1 5; 9 9 1];
asserts.isequal(R, REF);
%=============================================================================
rng(0);
U = rand(2, 3);
E = nbininv(U, 3, 0.4);
rng(0);
R = nbinrnd(3, 0.4, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
U = rand(1, 3);
E = nbininv(U, [1 3 5], [0.5 0.4 0.8]);
rng(0);
R = nbinrnd([1 3 5], [0.5 0.4 0.8]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
R = nbinrnd(single(3), single(0.4), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = nbinrnd(3, 0.4, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = nbinrnd([0 -1 NaN], [0.4 0.4 0.4], [1 3]);
asserts.istrue(all(isnan(R)));
R = nbinrnd(3, [-0.1 1.1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
asserts.isequal(nbinrnd(3, 1, 1, 3), zeros(1, 3));
R = nbinrnd(3, 0.4, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('nbinrnd(1)', _('Not enough input arguments.'));
asserts.checkerror('nbinrnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('nbinrnd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('nbinrnd({1}, 0.4)', _('R must be a positive numeric array.'));
asserts.checkerror('nbinrnd(1, {0.4})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
