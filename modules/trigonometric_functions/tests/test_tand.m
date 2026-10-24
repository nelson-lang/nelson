%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tand'), 1);
asserts.isequal(nargout('tand'), 1);
%=============================================================================
asserts.isequal(tand(NaN), NaN);
asserts.isequal(tand(-NaN), NaN);
asserts.isequal(tand(Inf), NaN);
asserts.isequal(tand(-Inf), NaN);
%=============================================================================
asserts.isequal(tand([90, 270]), [Inf -Inf]);
asserts.isequal(tand([0, 180, 360]), [0 0 0]);
asserts.isapprox(tand(10:10:80), tan(pi * [10:10:80] * inv(180)), 10 * eps);
%=============================================================================
