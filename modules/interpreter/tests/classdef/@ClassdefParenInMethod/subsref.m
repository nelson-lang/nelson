%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(f, index)
  switch index(1).type
    case '()'
      g = f(1);
      varargout{1} = g.v;
    case '.'
      varargout{1} = builtin('subsref', f, index);
    otherwise
      error('unsupported');
  end
end
