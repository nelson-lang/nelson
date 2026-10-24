%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tf2zp(varargin)
  narginchk(2, 2);
  nargoutchk(0, 3);
  b = varargin{1};
  a = varargin{2};
  if ~isvector(a)
    error('Nelson:validators:mustBeVector', '%s must be a vector.', 'Second argument');
  end
  if isvector(b)
    b = b(:).';
  end
  zerosArray = [];
  gain = zeros(size(b, 1), 1);
  for row = 1:size(b, 1)
    numerator = signalTrimLeadingZeros(b(row, :));
    if isempty(numerator)
      gain(row) = 0;
      z = [];
    else
      gain(row) = numerator(1) / a(1);
      z = roots(numerator);
    end
    if row == 1
      zerosArray = zeros(length(z), size(b, 1));
    end
    if length(z) < size(zerosArray, 1)
      z = [z; zeros(size(zerosArray, 1) - length(z), 1)];
    end
    zerosArray(:, row) = z;
  end
  varargout{1} = zerosArray;
  if nargout > 1
    varargout{2} = roots(a);
  end
  if nargout > 2
    varargout{3} = gain;
  end
end
%=============================================================================
