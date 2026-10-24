%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('range'), 2);
asserts.isequal(nargout('range'), 1);
%=============================================================================
asserts.isequal(range([3 1 8 4]), 7);
asserts.isequal(range([1 2; 5 4]), [4 2]);
asserts.isequal(range([1 2; 5 4], 2), [1; 1]);
asserts.isequal(range([1 2; 5 4], 1), [4 2]);
asserts.isequal(range(-5), 0);
asserts.isapprox(range([1.5 2.5 -3.0]), 5.5, 1e-12);
% ignores NaN like max/min
asserts.isequal(range([1 NaN 5]), 4);
% column vector
asserts.isequal(range([2; 9; 4]), 7);
%=============================================================================
asserts.checkerror('range(''abc'')', sprintf(_('%s must be numeric or logical.'), 'Input'), 'Nelson:validators:mustBeNumericOrLogical');
%=============================================================================
