%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = set(obj, varargin)
  if rem(nargin - 1, 2) ~= 0
    error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(varargin)
    propertyName = timeseriesText(varargin{k}, 'Property name');
    obj.(propertyName) = varargin{k + 1};
  end
end
%=============================================================================
