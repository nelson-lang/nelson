%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% circle: equal axes -> zero eccentricity
asserts.isapprox(axes2ecc(5, 5), 0, 1e-12);
%=============================================================================
% e = sqrt(1 - (b/a)^2)
asserts.isapprox(axes2ecc(10, 6), sqrt(1 - (6 / 10) ^ 2), 1e-12);
%=============================================================================
% two-element vector form
asserts.isapprox(axes2ecc([10 6]), sqrt(1 - 0.6 ^ 2), 1e-12);
%=============================================================================
% two-column matrix form (row per ellipse)
asserts.isapprox(axes2ecc([10 6; 5 3]), [sqrt(1 - 0.6 ^ 2); sqrt(1 - 0.6 ^ 2)], 1e-12);
%=============================================================================
% vectorized over a vector of semiminor axes
r = axes2ecc(10, [10 8 6]);
asserts.isapprox(r, sqrt(1 - ([10 8 6] / 10) .^ 2), 1e-12);
%=============================================================================
% invalid single-input shape errors
cmd = 'axes2ecc([1 2 3])';
asserts.checkerror(cmd, _('A single input must be a two-column matrix or a two-element vector.'));
%=============================================================================
