%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function b = padarray(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  a = varargin{1};
  ip_assert_full_image(a, true);
  if ~isnumeric(varargin{2}) || ~isreal(varargin{2})
    error('Nelson:image_processing:padSizeContainNonnegativeIntegers', '%s must contain nonnegative integers.', 'Pad size');
  end
  padSize = double(varargin{2});
  if ~(numel(padSize) == 1 || numel(padSize) == 2) ...
      || any(~isfinite(padSize(:))) || any(padSize(:) < 0) ...
      || any(padSize(:) ~= floor(padSize(:)))
    error('Nelson:image_processing:padSizeContainNonnegativeIntegers', '%s must contain nonnegative integers.', 'Pad size');
  end
  if numel(padSize) == 1
    padSize = [padSize padSize];
  end
  method = 0;
  direction = 'both';
  for k = 3:nargin
    v = varargin{k};
    if ischar(v) || isstring(v)
      txt = lower(convertStringsToChars(v));
      if strcmp(txt, 'pre') || strcmp(txt, 'post') || strcmp(txt, 'both')
        direction = txt;
      else
        method = txt;
      end
    else
      method = v;
    end
  end
  if ~(isnumeric(method) || islogical(method) || ischar(method))
    error('Nelson:image_processing:unsupportedPaddingMethod', 'Unsupported "%s" option.', 'padding method');
  end
  top = padSize(1); bottom = padSize(1); left = padSize(2); right = padSize(2);
  if strcmp(direction, 'pre')
    bottom = 0; right = 0;
  elseif strcmp(direction, 'post')
    top = 0; left = 0;
  end
  s = size(a);
  if length(s) < 3
    s(3) = 1;
  end
  outRows = s(1) + top + bottom;
  outCols = s(2) + left + right;
  if ischar(method)
    rowIdx = ip_pad_index((1:outRows) - top, s(1), method);
    colIdx = ip_pad_index((1:outCols) - left, s(2), method);
    b = a(rowIdx, colIdx, :);
  else
    b = zeros(outRows, outCols, s(3)) + method;
    b(top + 1:top + s(1), left + 1:left + s(2), :) = a;
  end
  if length(size(a)) < 3
    b = b(:, :, 1);
  end
  if ~strcmp(class(a), 'double')
    b = cast(b, class(a));
  end
end
%=============================================================================
