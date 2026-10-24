%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bw, thresh] = edge(varargin)
  narginchk(1, 4);
  nargoutchk(0, 2);
  ip_assert_full_image(varargin{1}, true);
  img = im2gray(varargin{1});
  if nargin >= 2
    if ~(ischar(varargin{2}) || isStringScalar(varargin{2}))
      error('Nelson:image_processing:edgeDetectionMethodString', '%s must be a string.', 'Edge detection method');
    end
    method = lower(convertStringsToChars(varargin{2}));
  else
    method = 'sobel';
  end
  sigma = [];
  if nargin >= 3 && ~isempty(varargin{3})
    thresh = varargin{3};
  else
    thresh = [];
  end
  if nargin >= 4
    sigma = varargin{4};
  end
  d = ip_to_unit_double(img);
  direction = 'both';
  switch method
    case 'sobel'
      direction = ip_validate_edge_direction(sigma);
      hx = fspecial('sobel')';
      hy = fspecial('sobel');
    case 'prewitt'
      direction = ip_validate_edge_direction(sigma);
      hx = fspecial('prewitt')';
      hy = fspecial('prewitt');
    case 'roberts'
      direction = ip_validate_edge_direction(sigma);
      hx = zeros(2, 2);
      hx(1, 1) = 1;
      hx(2, 2) = -1;
      hy = zeros(2, 2);
      hy(1, 2) = 1;
      hy(2, 1) = -1;
    case 'log'
      [bw, thresh] = ip_edge_log(d, thresh, sigma);
      return
    case 'canny'
      [bw, thresh] = ip_edge_canny(d, thresh, sigma);
      return
    otherwise
      error('Nelson:image_processing:unsupportedEdgeDetectionMethod', 'Unsupported "%s" option.', 'edge detection method');
  end
  ip_validate_edge_threshold(thresh, false);
  gx = imfilter(d, hx, 'replicate');
  gy = imfilter(d, hy, 'replicate');
  if strcmp(direction, 'horizontal')
    if strcmp(method, 'roberts')
      mag = abs(gx);
    else
      mag = abs(gy);
    end
  elseif strcmp(direction, 'vertical')
    if strcmp(method, 'roberts')
      mag = abs(gy);
    else
      mag = abs(gx);
    end
  else
    mag = sqrt(gx .^ 2 + gy .^ 2);
  end
  if isempty(thresh)
    thresh = graythresh(mag);
  end
  bw = mag > thresh;
end
%=============================================================================
