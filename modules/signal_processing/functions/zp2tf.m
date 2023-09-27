%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zp2tf(varargin)
  narginchk(3, 3);
  nargoutchk(0, 2);
  zerosArray = varargin{1};
  polesArray = varargin{2};
  gain = varargin{3};
  if ~isvector(gain)
    error(_('Third argument must be a vector.'));
  end
  
  denominator = findDenominator(polesArray);
  varargout{1} = findNumerator(zerosArray, gain, denominator);
  
  if (nargout > 1)
    varargout{2} = denominator;
  end
end
%=============================================================================
function denominator = findDenominator(polesArray)
  denominator = real(poly(polesArray(:)));
end
%=============================================================================
function numerator = findNumerator(zerosArray, gain, denominator)
  numerator = [];
  denominator_cols = size(denominator, 2);
  gain = gain(:);
  gain_rows = size(gain, 1);
  if isempty(zerosArray)
    if (denominator_cols - 1 > 0)
      numerator = [zeros(gain_rows, denominator_cols - 1), gain];
    else
      numerator = gain;
    end
  else
    [zeros_rows, zeros_cols] = size(zerosArray);
    if (gain_rows ~= zeros_cols)
      if ((zeros_rows - 1) ~= 0)
        error(_('Input argument #1 and #3 should have the same column size.'));
      else
        error(_('First argument must be a column vector.'));
      end
    else
      numerator = findNumeratorStandardImpl(zerosArray, zeros_cols, gain, denominator_cols);
    end
  end
end
%=============================================================================
function numerator = findNumeratorStandardImpl(zerosArray, zeros_cols, gain, denominator_cols)
  numerator = [];
  for nidx = 1:zeros_cols
    zeros_index = zerosArray(:, nidx);
    poles_index = real(poly(zeros_index) * gain(nidx));
    zeros_rows = zeros(1, denominator_cols - length(poles_index));
    if isempty(numerator)
      numerator = [zeros_rows, poles_index];
    else
      numerator(nidx, :) = [zeros_rows, poles_index];
    end
  end
end
%=============================================================================
