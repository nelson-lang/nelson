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
asserts.isequal(nargin('binornd'), -1);
asserts.isequal(nargout('binornd'), 1);
%=============================================================================
rng(0, 'twister');
R = binornd(10, 0.3, 2, 3);
REF = [4 1 3; 5 5 1];
asserts.isequal(R, REF);
%=============================================================================
rng(0, 'twister');
U = rand(2, 3);
E = binoinv(U, 10, 0.3);
rng(0, 'twister');
R = binornd(10, 0.3, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isequal(R, E);
%=============================================================================
rng(0, 'twister');
U = rand(1, 3);
E = binoinv(U, [5 10 20], [0.2 0.3 0.7]);
rng(0, 'twister');
R = binornd([5 10 20], [0.2 0.3 0.7]);
asserts.isequal(R, E);
%=============================================================================
rng(0, 'twister');
R = binornd(single(10), single(0.3), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = binornd(10, 0.3, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = binornd([-1 1.5 NaN], [0.5 0.5 0.5], [1 3]);
asserts.istrue(all(isnan(R)));
R = binornd(10, [-0.1 1.1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
asserts.isequal(binornd(0, 0.5, 1, 3), zeros(1, 3));
asserts.isequal(binornd(10, 0, 1, 3), zeros(1, 3));
asserts.isequal(binornd(10, 1, 1, 3), 10 * ones(1, 3));
R = binornd(10, 0.3, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('binornd(1)', _('Not enough input arguments.'));
asserts.checkerror('binornd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('binornd(1, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('binornd({1}, 1)', _('N must be a nonnegative integer numeric array.'));
asserts.checkerror('binornd(1, {1})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
