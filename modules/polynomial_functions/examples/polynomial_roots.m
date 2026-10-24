%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Construct a polynomial from known roots and evaluate it.
expectedRoots = [-2, 1, 3];
coefficients = poly(expectedRoots);
computedRoots = sort(roots(coefficients));
disp('Polynomial coefficients:');
disp(coefficients);
disp('Recovered roots:');
disp(computedRoots);
