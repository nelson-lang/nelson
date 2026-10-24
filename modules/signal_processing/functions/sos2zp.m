%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sos2zp(sos)
  narginchk(1, 1);
  nargoutchk(0, 3);
  if isempty(sos)
    z = [];
    p = [];
    k = 1;
  else
    if ~ismatrix(sos) || size(sos, 2) ~= 6
      error('Nelson:signal:invalidSOSMatrix', 'SOS matrix must have six columns.');
    end
    z = [];
    p = [];
    k = 1;
    for row = 1:size(sos, 1)
      numerator = sos(row, 1:3);
      denominator = signalTrimTrailingZeros(sos(row, 4:6));
      sectionOrder = length(denominator);
      numerator = numerator(1:sectionOrder);
      z = [z; rootsWithTrailingZeros(numerator)];
      p = [p; roots(denominator)];
      k = k * numerator(1) / denominator(1);
    end
  end
  varargout{1} = z;
  if nargout > 1
    varargout{2} = p;
  end
  if nargout > 2
    varargout{3} = k;
  end
end
%=============================================================================
function r = rootsWithTrailingZeros(coefficients)
  trimmed = signalTrimTrailingZeros(coefficients);
  numberOfZeros = length(coefficients) - length(trimmed);
  r = roots(trimmed);
  if numberOfZeros > 0
    r = [r; zeros(numberOfZeros, 1)];
  end
end
%=============================================================================
