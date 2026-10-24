%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_imwarp3d(volume, srcRef, tform, refOut, method, fillValue)
  fillValue = ip_imwarp3d_fill_value(fillValue);
  outSize = refOut.ImageSize;
  out = builtin('__imwarp3d_kernel__', volume, inv(tform), ...
    [srcRef.XWorldLimits(1), srcRef.YWorldLimits(1), srcRef.ZWorldLimits(1)], ...
    [srcRef.PixelExtentInWorldX, srcRef.PixelExtentInWorldY, srcRef.PixelExtentInWorldZ], ...
    [refOut.XWorldLimits(1), refOut.YWorldLimits(1), refOut.ZWorldLimits(1)], ...
    [refOut.PixelExtentInWorldX, refOut.PixelExtentInWorldY, refOut.PixelExtentInWorldZ], ...
    outSize, method, fillValue);
end
%=============================================================================
function fillValue = ip_imwarp3d_fill_value(fillValue)
  if ~(isnumeric(fillValue) || islogical(fillValue)) || ~isreal(fillValue) || numel(fillValue) ~= 1
    error('Nelson:image_processing:fillValuesScalarDVolumes', 'Fill values must be scalar for 3-D volumes.');
  end
  fillValue = double(fillValue);
  if ~isfinite(fillValue)
    error('Nelson:image_processing:fillValuesFinite', '%s values must be finite.', 'Fill');
  end
end
%=============================================================================
