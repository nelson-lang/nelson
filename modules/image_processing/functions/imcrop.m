%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imcrop(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  img = varargin{1};
  ip_assert_full_image(img, true);
  if nargin == 1
    out = img;
    return
  end
  if ~isnumeric(varargin{2}) || ~isreal(varargin{2}) || numel(varargin{2}) ~= 4
    error('Nelson:image_processing:cropRectangleNumericElementVector', '%s must be a numeric 4-element vector.', 'Crop rectangle');
  end
  rect = double(varargin{2});
  if any(~isfinite(rect(:)))
    error('Nelson:image_processing:cropRectangleContainFiniteValues', '%s must contain finite values.', 'Crop rectangle');
  end
  if rect(3) < 0 || rect(4) < 0
    error('Nelson:image_processing:cropRectangleWidthHeightNonnegative', '%s width and height must be nonnegative.', 'Crop rectangle');
  end
  x1 = max(1, floor(rect(1)));
  y1 = max(1, floor(rect(2)));
  x2 = min(size(img, 2), floor(rect(1) + rect(3)));
  y2 = min(size(img, 1), floor(rect(2) + rect(4)));
  if ip_is_rgb_image(img)
    out = img(y1:y2, x1:x2, :);
  else
    out = img(y1:y2, x1:x2);
  end
end
%=============================================================================
