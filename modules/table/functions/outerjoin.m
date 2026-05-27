%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = outerjoin(left, right, varargin)
  narginchk(2, Inf);
  joinType = 'full';
  args = varargin;
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    if ischar(name) && strcmpi(name, 'Type')
      joinType = lower(char(args{k + 1}));
      args(k:k + 1) = [];
    else
      k = k + 2;
    end
  end
  switch joinType
    case {'full', 'left', 'right'}
      T = tableJoin(left, right, joinType, args{:});
    otherwise
      error(_('Type must be ''full'', ''left'', or ''right''.'));
  end
end
%=============================================================================
