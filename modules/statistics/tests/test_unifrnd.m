%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('unifrnd'), -1);
asserts.isequal(nargout('unifrnd'), 1);
%=============================================================================
rng(0);
U = rand(1, 1);
E = U;
rng(0);
R = unifrnd(0, 1);
asserts.isequal(R, E);
asserts.istrue(R >= 0 && R < 1);
%=============================================================================
rng(0);
U = rand(1, 5);
E = (1:5) .* U;
rng(0);
R = unifrnd(0, 1:5);
asserts.isequal(R, E);
asserts.isequal(size(R), [1 5]);
%=============================================================================
rng(0);
U = rand(2, 3);
E = -1 + 2 .* U;
rng(0);
R = unifrnd(-1, 1, [2 3]);
asserts.isequal(R, E);
asserts.isequal(size(R), [2 3]);
asserts.istrue(all(R(:) >= -1 & R(:) < 1));
%=============================================================================
rng(0);
U = rand(1, 6);
E = (0:5) + U;
rng(0);
R = unifrnd(0:5, 1:6, 1, 6);
asserts.isequal(R, E);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = single(2) .* U;
rng(0);
R = unifrnd(single(0), single(2), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(R, E);
%=============================================================================
R = unifrnd([0 1 NaN], [1 1 2]);
asserts.istrue(R(1) >= 0 && R(1) < 1);
asserts.istrue(all(isnan(R(2:3))));
%=============================================================================
R = unifrnd(0, 1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = unifrnd(single(0), single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = unifrnd(0, 1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('unifrnd(0)', _('Not enough input arguments.'));
asserts.checkerror('unifrnd([0 1], 2, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('unifrnd(0, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('unifrnd(0, 1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('unifrnd({0}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('unifrnd(0, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
