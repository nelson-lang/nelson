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
asserts.isequal(nargin('unidrnd'), -1);
asserts.isequal(nargout('unidrnd'), 1);
%=============================================================================
rng(0);
R = unidrnd(5, 2, 3);
REF = [5 1 4; 5 5 1];
asserts.isequal(R, REF);
%=============================================================================
rng(0);
U = rand(2, 3);
E = unidinv(U, 5);
rng(0);
R = unidrnd(5, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
U = rand(1, 3);
E = unidinv(U, [3 5 10]);
rng(0);
R = unidrnd([3 5 10]);
asserts.isequal(R, E);
%=============================================================================
rng(0);
R = unidrnd(single(5), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = unidrnd(5, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = unidrnd([0 -1 1.5 NaN], [1 4]);
asserts.istrue(all(isnan(R)));
R = unidrnd(5, 0, 3);
asserts.isequal(size(R), [0 3]);
R = unidrnd(single(5), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
%=============================================================================
asserts.checkerror('unidrnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('unidrnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('unidrnd({1})', _('N must be a positive integer numeric array.'));
%=============================================================================
