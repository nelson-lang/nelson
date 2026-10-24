%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function direction = ip_validate_edge_direction(direction)
  if isempty(direction)
    direction = 'both';
    return
  end
  if ~(ischar(direction) || isStringScalar(direction))
    error('Nelson:image_processing:edgeDirectionString', '%s must be a string.', 'Edge direction');
  end
  direction = lower(convertStringsToChars(direction));
  if ~(strcmp(direction, 'horizontal') || strcmp(direction, 'vertical') || strcmp(direction, 'both'))
    error('Nelson:image_processing:unsupportedEdgeDirection', 'Unsupported "%s" option.', 'edge direction');
  end
end
%=============================================================================
