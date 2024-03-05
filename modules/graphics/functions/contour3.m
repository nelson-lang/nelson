%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contour3(varargin)
  [M, h] = contour(varargin{:});
  h.Floating = 'on';
  
  if ~ishold
    ax = ancestor(h, 'axes');
    view(ax, 3 );
    grid(ax, 'on');
  end
  if nargout > 0
    varargout{1} = h.ContourMatrix;
  else
    varargout = {};
  end
  if nargout > 1
    varargout{2} = h;
  end    
end
%=============================================================================
