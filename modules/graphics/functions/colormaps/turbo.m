%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = turbo(varargin)
  nargoutchk(0, 1);
  narginchk(0, 1);
  usesDefaultLength = nargin == 0;
  colorCount = requestedColorCount(varargin{:});
  if colorCount == 0
    varargout{1} = zeros(0, 3);
    return
  end
  x = linspace(0, 1, colorCount).';
  powers = [ones(colorCount, 1), x, x .^ 2, x .^ 3, x .^ 4, x .^ 5];
  coefficients = [
    0.13572138, 4.61539260, -42.66032258, 132.13108234, -152.94239396, 59.28637943;
    0.09140261, 2.19418839, 4.84296658, -14.18503333, 4.27729857, 2.82956604;
    0.10667330, 12.64194608, -60.58204836, 110.36276771, -89.90310912, 27.34824973];
  map = powers * coefficients.';
  map(map < 0) = 0;
  map(map > 1) = 1;
  if usesDefaultLength && colorCount >= 2
    map(1:2, :) = [0.18995, 0.07176, 0.23217; 0.19483, 0.08339, 0.26149];
  end
  varargout{1} = map;
end
%=============================================================================
