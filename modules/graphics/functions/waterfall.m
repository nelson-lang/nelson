%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = waterfall(varargin)
  % waterfall(X, Y, Z)
  % waterfall(X, Y, Z, C)
  % waterfall(Z)
  % waterfall(Z, C)
  % waterfall(ax, ...)
  % p = waterfall(...)
  h = meshz (varargin{:});
  h.MeshStyle = 'row';
  varargout = {};
  if nargout > 0
    varargout{1} = h;
  end
end    
%=============================================================================
