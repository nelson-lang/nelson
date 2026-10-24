%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% trailing zero coefficients: zero roots must be kept
asserts.isequal(roots([1 0]), 0);
asserts.isequal(roots([1 0 0]), [0; 0]);
asserts.isequal(roots([0 1 0]), 0);
asserts.isequal(roots([0 0 3 0 0]), [0; 0]);
R = roots(int32([1 0]));
asserts.isequal(class(R), 'int32');
asserts.isequal(R, int32(0));
% unchanged
R = sort(roots([1 -3 2]));
asserts.isapprox(R, [1; 2], 1e-12);
asserts.isequal(roots([]), []);
asserts.isequal(roots(5), zeros(0, 1));
% mixed: leading and trailing zeros with nontrivial roots
R = sort(roots([0 1 -3 2 0]));
asserts.isapprox(R, [0; 1; 2], 1e-12);
%=============================================================================
