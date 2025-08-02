%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = loglog(varargin)
  go = plot(varargin{:});
  ax = go.Parent;
  ax.XScale = 'log';
  ax.YScale = 'log';
  if (nargout > 0)
    varargout{1} = go;
  end
end
%=============================================================================
