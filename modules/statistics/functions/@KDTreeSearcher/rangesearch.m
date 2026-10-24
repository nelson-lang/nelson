%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, dist] = rangesearch(obj, Y, radius, varargin)
  narginchk(3, inf);
  nargoutchk(0, 2);
  args = mergeSearcherOptionsLocal(obj, varargin);
  [idx, dist] = rangesearch(obj.X, Y, radius, args{:});
end
%=============================================================================
function args = mergeSearcherOptionsLocal(obj, userArgs)
  hasDistance = false;
  hasP = false;
  for k = 1:2:numel(userArgs)
    switch lower(userArgs{k})
      case 'distance'
        hasDistance = true;
      case 'p'
        hasP = true;
    end
  end
  args = userArgs;
  if ~hasDistance
    args = [args, {'Distance', obj.Distance}];
  end
  if ~hasP && strcmpi(obj.Distance, 'minkowski') && ~isempty(obj.DistParameter)
    args = [args, {'P', obj.DistParameter}];
  end
end
%=============================================================================
