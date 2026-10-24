%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = hot(varargin)
  narginchk(0, 1);
  colorCount = requestedColorCount(varargin{:});
  hotSegmentLength = fix(3 * colorCount / 8);
  colorIndex = (1:colorCount).';
  red = saturatingRamp(colorIndex, 0, hotSegmentLength);
  green = saturatingRamp(colorIndex, hotSegmentLength, hotSegmentLength);
  blue = saturatingRamp(colorIndex, 2 * hotSegmentLength, colorCount - 2 * hotSegmentLength);
  map = [red, green, blue];
end
%=============================================================================
function channel = saturatingRamp(colorIndex, offset, span)
  channel = min(max((colorIndex - offset) ./ max(span, 1), 0), 1);
end
%=============================================================================
