%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [markerValue, floorValue] = ip_imimposemin_levels(values, model)
  switch class(model)
    case {'uint8', 'uint16', 'uint32'}
      markerValue = 0;
      floorValue = 1;
    case 'int8'
      markerValue = -128;
      floorValue = -127;
    case 'int16'
      markerValue = -32768;
      floorValue = -32767;
    case 'int32'
      markerValue = -2147483648;
      floorValue = -2147483647;
    case 'logical'
      markerValue = 0;
      floorValue = 1;
    otherwise
      base = min(values(:));
      span = max(values(:)) - base;
      delta = max(eps(max(abs(values(:)))), max(span, 1) * 1e-9);
      markerValue = base - delta;
      floorValue = base;
  end
end
%=============================================================================
