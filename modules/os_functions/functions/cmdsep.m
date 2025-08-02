%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cmdsep(varargin)
  nargoutchk(0, 1);
  narginchk(0, 0);
  if ispc()
    varargout{1} = "&&";
  else
    varargout{1} = ";";
  end
end
%=============================================================================
