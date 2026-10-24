%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('cosd'), 1);
asserts.isequal(nargout('cosd'), 1);
%=============================================================================
asserts.isequal(cosd(NaN), NaN);
asserts.isequal(cosd(-NaN), NaN);
asserts.isequal(cosd(Inf), NaN);
asserts.isequal(cosd(-Inf), NaN);
%=============================================================================
arch = computer('arch');
if strcmp(arch, 'win32')
  asserts.isapprox(cosd([90, 270]), [0 0], 1e-15);
  asserts.isapprox(cosd([0, 180, 360]), [1 -1 1], 1e-15);
else
  asserts.isequal(cosd([90, 270]), [0 0]);
  asserts.isequal(cosd([0, 180, 360]), [1 -1 1]);
end
asserts.isapprox(cosd(0:10:90), cos(pi * [0:10:90] * inv(180)), 10 * eps);
%=============================================================================
