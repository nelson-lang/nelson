%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = ip_resolve_output_ref(outputView, srcRef, tform)
  if ischar(outputView) || isstring(outputView)
    mode = lower(convertStringsToChars(outputView));
    if strcmp(mode, 'same')
      ref = srcRef;
    elseif strcmp(mode, 'full')
      ref = ip_full_output_ref(srcRef, tform);
    else
      error('Nelson:image_processing:unsupportedOutputView', 'Unsupported "%s" option.', 'output view');
    end
  elseif isstruct(outputView)
    ref = ip_validate_imref2d(outputView);
  else
    ref = ip_make_imref2d(outputView, [], []);
  end
end
%=============================================================================
function ref = ip_full_output_ref(srcRef, tform)
  xlim = srcRef.XWorldLimits;
  ylim = srcRef.YWorldLimits;
  dx = srcRef.PixelExtentInWorldX;
  dy = srcRef.PixelExtentInWorldY;
  xCenters = [xlim(1) + dx / 2, xlim(2) - dx / 2];
  yCenters = [ylim(1) + dy / 2, ylim(2) - dy / 2];
  corners = [xCenters(1) yCenters(1) 1; xCenters(2) yCenters(1) 1; ...
    xCenters(1) yCenters(2) 1; xCenters(2) yCenters(2) 1] * tform;
  x = corners(:, 1) ./ corners(:, 3);
  y = corners(:, 2) ./ corners(:, 3);
  xLimits = [min(x) - dx / 2, max(x) + dx / 2];
  yLimits = [min(y) - dy / 2, max(y) + dy / 2];
  imageSize = [max(1, ceil(diff(yLimits) / dy)) max(1, ceil(diff(xLimits) / dx))];
  xLimits(2) = xLimits(1) + imageSize(2) * dx;
  yLimits(2) = yLimits(1) + imageSize(1) * dy;
  ref = ip_make_imref2d(imageSize, xLimits, yLimits);
end
%=============================================================================
