%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.squareMatrix'), 1);
asserts.isequal(nargout('asserts.squareMatrix'), 2);
%=============================================================================
asserts.squareMatrix(ones(2, 2));
%=============================================================================
[r, msg] = asserts.squareMatrix(ones(2, 3));
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.squareMatrix'));
%=============================================================================
