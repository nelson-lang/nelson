%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = ip_resolve_output_ref3d(outputView, srcRef, tform)
  if ischar(outputView) || isStringScalar(outputView)
    mode = lower(convertStringsToChars(outputView));
    if strcmp(mode, 'same')
      ref = srcRef;
    elseif strcmp(mode, 'full')
      ref = ip_full_output_ref3d(srcRef, tform);
    else
      error('Nelson:image_processing:unsupportedOutputView', 'Unsupported "%s" option.', 'output view');
    end
  elseif isstruct(outputView)
    ref = ip_validate_imref3d(outputView);
  else
    ref = ip_make_imref3d(outputView, [], [], []);
  end
end
%=============================================================================
function ref = ip_full_output_ref3d(srcRef, tform)
  dx = srcRef.PixelExtentInWorldX;
  dy = srcRef.PixelExtentInWorldY;
  dz = srcRef.PixelExtentInWorldZ;
  xCenters = [srcRef.XWorldLimits(1) + dx / 2, srcRef.XWorldLimits(2) - dx / 2];
  yCenters = [srcRef.YWorldLimits(1) + dy / 2, srcRef.YWorldLimits(2) - dy / 2];
  zCenters = [srcRef.ZWorldLimits(1) + dz / 2, srcRef.ZWorldLimits(2) - dz / 2];
  [X, Y, Z] = ndgrid(xCenters, yCenters, zCenters);
  corners = [X(:), Y(:), Z(:), ones(numel(X), 1)] * tform;
  xLimits = [min(corners(:, 1)) - dx / 2, max(corners(:, 1)) + dx / 2];
  yLimits = [min(corners(:, 2)) - dy / 2, max(corners(:, 2)) + dy / 2];
  zLimits = [min(corners(:, 3)) - dz / 2, max(corners(:, 3)) + dz / 2];
  imageSize = [max(1, ceil(diff(yLimits) / dy)), max(1, ceil(diff(xLimits) / dx)), ...
    max(1, ceil(diff(zLimits) / dz))];
  xLimits(2) = xLimits(1) + imageSize(2) * dx;
  yLimits(2) = yLimits(1) + imageSize(1) * dy;
  zLimits(2) = zLimits(1) + imageSize(3) * dz;
  ref = ip_make_imref3d(imageSize, xLimits, yLimits, zLimits);
end
%=============================================================================
