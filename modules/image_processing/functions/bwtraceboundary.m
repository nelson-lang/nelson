%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function boundary = bwtraceboundary(varargin)
  narginchk(2, 5);
  nargoutchk(0, 1);
  bw = varargin{1};
  ip_assert_full_image(bw, false);
  bw = ip_to_logical_mask(bw);
  startPoint = double(varargin{2});
  if ~isnumeric(varargin{2}) || ~isreal(varargin{2}) || numel(startPoint) ~= 2 ...
      || any(~isfinite(startPoint(:))) || any(startPoint(:) ~= floor(startPoint(:)))
    error('Nelson:image_processing:startingPointContainRowColumnInteger', 'Starting point must contain row and column integer coordinates.');
  end
  startPoint = startPoint(:)';
  firstStep = 'N';
  conn = 8;
  maxPoints = Inf;
  for k = 3:nargin
    arg = varargin{k};
    if ischar(arg) || isstring(arg)
      firstStep = upper(convertStringsToChars(arg));
    elseif k < 5
      conn = double(arg);
    else
      maxPoints = double(arg);
    end
  end
  if ~(conn == 4 || conn == 8)
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  if ~(isscalar(maxPoints) && isfinite(maxPoints) && maxPoints >= 1 && maxPoints == floor(maxPoints)) ...
      && ~isinf(maxPoints)
    error('Nelson:image_processing:maximumNumberBoundaryPointsPositiveInteger', '%s must be a positive integer scalar.', 'Maximum number of boundary points');
  end
  if startPoint(1) < 1 || startPoint(2) < 1 || startPoint(1) > size(bw, 1) ...
      || startPoint(2) > size(bw, 2) || ~bw(startPoint(1), startPoint(2))
    error('Nelson:image_processing:startingPointForegroundPixel', 'Starting point must be a foreground pixel.');
  end
  perim = bwperim(bw, conn);
  if ~perim(startPoint(1), startPoint(2))
    error('Nelson:image_processing:startingPointBoundaryPixel', 'Starting point must be a boundary pixel.');
  end
  selected = bwselect(perim, startPoint(2), startPoint(1), conn);
  boundary = ip_trace_boundary_pixels(selected, startPoint, conn, firstStep, maxPoints);
end
%=============================================================================
