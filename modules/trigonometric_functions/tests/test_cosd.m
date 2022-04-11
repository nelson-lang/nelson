%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cosd'), 1);
assert_isequal(nargout('cosd'), 1);
%=============================================================================
assert_isequal(cosd(NaN), NaN);
assert_isequal(cosd(-NaN), NaN);
assert_isequal(cosd(Inf), NaN);
assert_isequal(cosd(-Inf), NaN);
%=============================================================================
assert_isequal(cosd ([90, 270]), [0 0]);
assert_isequal(cosd([0, 180, 360]), [1 -1  1]);
assert_isapprox(cosd(0:10:90), cos (pi*[0:10:90]* inv(180)), -10 * eps);
%=============================================================================
