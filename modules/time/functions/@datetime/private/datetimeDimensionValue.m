%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = datetimeDimensionValue(serialValue, query, varargin)
  switch query
    case 'numel'
      value = numel(serialValue, varargin{:});
    case 'length'
      value = length(serialValue);
    case 'isempty'
      value = isempty(serialValue);
    case 'ndims'
      value = ndims(serialValue);
    case 'isrow'
      value = isrow(serialValue);
    case 'iscolumn'
      value = iscolumn(serialValue);
    case 'isscalar'
      value = isscalar(serialValue);
    case 'isvector'
      value = isvector(serialValue);
    case 'end'
      shape = size(serialValue);
      index = varargin{1};
      count = varargin{2};
      if count == 1
        value = numel(serialValue);
      else
        value = shape(index);
      end
    otherwise
      error('Nelson:time:unknownDatetimeDimensionQuery', 'Unknown %s.', 'datetime dimension query');
  end
end
%=============================================================================
