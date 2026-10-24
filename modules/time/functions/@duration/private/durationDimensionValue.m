%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = durationDimensionValue(secondsValue, query, varargin)
  switch query
    case 'numel'
      value = numel(secondsValue, varargin{:});
    case 'length'
      value = length(secondsValue);
    case 'isempty'
      value = isempty(secondsValue);
    case 'ndims'
      value = ndims(secondsValue);
    case 'isrow'
      value = isrow(secondsValue);
    case 'iscolumn'
      value = iscolumn(secondsValue);
    case 'isscalar'
      value = isscalar(secondsValue);
    case 'isvector'
      value = isvector(secondsValue);
    case 'end'
      shape = size(secondsValue);
      index = varargin{1};
      count = varargin{2};
      if count == 1
        value = numel(secondsValue);
      else
        value = shape(index);
      end
    otherwise
      error('Nelson:time:unknownDurationDimensionQuery', 'Unknown %s.', 'duration dimension query');
  end
end
%=============================================================================
