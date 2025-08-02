%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('tand'), 1);
assert_isequal(nargout('tand'), 1);
%=============================================================================
assert_isequal(tand(NaN), NaN);
assert_isequal(tand(-NaN), NaN);
assert_isequal(tand(Inf), NaN);
assert_isequal(tand(-Inf), NaN);
%=============================================================================
assert_isequal(tand([90, 270]), [Inf Inf]);
assert_isequal(tand([0, 180, 360]), [0 0  0]);
assert_isapprox(tand(10:10:80), tan(pi*[10:10:80] *inv(180)), -10 * eps);
%=============================================================================
