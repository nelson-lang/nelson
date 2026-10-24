%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% exact values on multiples of 45 degrees
asserts.isequal(tand(45), 1);
asserts.isequal(tand(135), -1);
asserts.isequal(tand(225), 1);
asserts.isequal(tand(-45), -1);
asserts.isequal(tand(0), 0);
asserts.isequal(tand(180), 0);
% infinities with the sign of the quadrant
asserts.isequal(tand(90), Inf);
asserts.isequal(tand(-90), -Inf);
asserts.isequal(tand(270), -Inf);
asserts.isequal(tand(450), Inf);
% general accuracy
asserts.isapprox(tand(30), tan(pi / 6), 1e-14);
asserts.isapprox(tand(60), tan(pi / 3), 1e-14);
asserts.isapprox(tand(-30), -tan(pi / 6), 1e-14);
% vector
R = tand([45 90 135 180 270]);
asserts.isequal(R, [1 Inf -1 0 -Inf]);
% non finite
assert(isnan(tand(NaN)));
assert(isnan(tand(Inf)));
%=============================================================================
