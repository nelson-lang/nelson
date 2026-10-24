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
asserts.isequal(nargin('gevrnd'), -1);
asserts.isequal(nargout('gevrnd'), 1);
%=============================================================================
rng(0);
R = gevrnd(0.2, 1, 0, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.istrue(all(R(:) >= -5));
rng(0);
R1 = gevrnd(0, 1, 0, 2, 3);
rng(0);
R2 = gevrnd(0, 1, 0, [2 3]);
asserts.isequal(R1, R2);
%=============================================================================
R = gevrnd(single(0.2), single(1), single(0), 2, 3);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = gevrnd(0.2, 1, 0, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
R = gevrnd([0.2 0.3], [1 1], [0 0]);
asserts.isequal(size(R), [1 2]);
asserts.checkerror('gevrnd(0.2, 1)', _('Not enough input arguments.'));
asserts.checkerror('gevrnd([0.2 0.3], 1, 0, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('gevrnd(0.2, 1, 0, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('gevrnd({0.2}, 1, 0)', sprintf(_('%s must be real numeric.'), 'K'));
asserts.checkerror('gevrnd(0.2, {1}, 0)', sprintf(_('%s must be real numeric.'), 'SIGMA'));
asserts.checkerror('gevrnd(0.2, 1, {0})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
