%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imadjust(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  img = varargin{1};
  isMap = ip_is_colormap(img) && isa(img, 'double') && all(img(:) >= 0) && all(img(:) <= 1);
  if nargin < 2 || isempty(varargin{2})
    if isMap
      in = stretchlim(reshape(img, size(img, 1), 1, 3));
    else
      in = stretchlim(img);
    end
  else
    in = double(varargin{2});
  end
  if nargin < 3 || isempty(varargin{3})
    outRange = [0; 1];
  else
    outRange = double(varargin{3});
  end
  if nargin < 4
    gamma = 1;
  else
    gamma = double(varargin{4});
  end
  if isMap
    d = reshape(img, size(img, 1), 1, 3);
  else
    ip_assert_full_image(img, true);
    d = ip_to_unit_double(img);
  end
  channels = size(d, 3);
  if size(in, 2) == 1
    in = repmat(in(:), 1, channels);
  end
  if size(outRange, 2) == 1
    outRange = repmat(outRange(:), 1, channels);
  end
  if numel(gamma) == 1
    gamma = repmat(gamma, 1, channels);
  end
  y = zeros(size(d));
  for c = 1:channels
    lo = in(1, c); hi = in(2, c);
    if hi == lo
      error('Nelson:image_processing:inputIntensityRangeDistinctBounds', 'Input intensity range must have distinct bounds.');
    end
    plane = ip_clip01((d(:, :, c) - lo) / (hi - lo));
    plane = plane .^ gamma(c);
    y(:, :, c) = outRange(1, c) + plane * (outRange(2, c) - outRange(1, c));
  end
  y = ip_clip01(y);
  if isMap
    out = reshape(y, size(img, 1), 3);
    return;
  end
  switch class(img)
    case 'uint8'
      out = uint8(round(y * 255));
    case 'uint16'
      out = uint16(round(y * 65535));
    case 'int16'
      out = int16(round(y * 65535 - 32768));
    case 'single'
      out = single(y);
    otherwise
      out = y;
  end
end
%=============================================================================
