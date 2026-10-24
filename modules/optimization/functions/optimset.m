%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = optimset(varargin)
  if nargin == 0
    opts = optimDefaultOptions('default');
    return
  end
  if nargin == 1 && ischar(varargin{1})
    opts = optimMergeOptions(varargin{1});
    return
  end
  if mod(nargin, 2) == 1
    base = varargin{1};
    if ~(isempty(base) || isstruct(base) || isobject(base))
      error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
    end
    first = 2;
  else
    base = [];
    first = 1;
  end
  if mod(nargin - first + 1, 2) ~= 0
    error('Nelson:optimization:nameValueExpected', 'Name-value arguments expected.');
  end
  updates = struct();
  for k = first:2:nargin
    name = optimCanonicalName(varargin{k});
    updates.(name) = varargin{k + 1};
  end
  opts = optimMergeOptions('default', base, updates);
end
%=============================================================================
