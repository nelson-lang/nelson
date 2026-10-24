%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% scalar p: constant polynomial evaluated at each element of x
asserts.isequal(polyval(5, [1 2 3]), [5 5 5]);
asserts.isequal(polyval(5, [1 2; 3 4]), [5 5; 5 5]);
asserts.isequal(polyval(5, 7), 5);
% empty p: zeros
asserts.isequal(polyval([], 3), 0);
asserts.isequal(polyval([], [1 2; 3 4]), zeros(2, 2));
% unchanged
asserts.isequal(polyval([2 1], [1 2]), [3 5]);
asserts.isequal(polyval([1 2 3], 2), 11);
% class
R = polyval(single(5), [1 2]);
asserts.isequal(class(R), 'single');
%=============================================================================
