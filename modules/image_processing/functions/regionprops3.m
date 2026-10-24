%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stats = regionprops3(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  [components, imageSize, intensity, propertyArgs] = ip_regionprops3_components(varargin);
  properties = ip_regionprops3_properties(propertyArgs);
  if isempty(intensity) && ip_regionprops3_needs_intensity(properties)
    error('Nelson:image_processing:intensityVolumeRequired', '%s is required.', 'Intensity volume');
  end
  stats = ip_regionprops3_measure(components, imageSize, intensity, properties);
end
%=============================================================================
