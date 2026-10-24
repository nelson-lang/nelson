%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ref = ip_validate_imref2d(ref)
  if ~isstruct(ref) || ~isfield(ref, 'ImageSize') || ~isfield(ref, 'XWorldLimits') ...
      || ~isfield(ref, 'YWorldLimits')
    error('Nelson:image_processing:spatialReferenceContainImagesizeXworldlimitsYworldlimits', 'Spatial reference must contain ImageSize, XWorldLimits and YWorldLimits fields.');
  end
  ref = ip_make_imref2d(ref.ImageSize, ref.XWorldLimits, ref.YWorldLimits);
end
%=============================================================================
