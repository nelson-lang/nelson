%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = copper(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);    
  if nargin < 1
    f = get(groot(), 'CurrentFigure');
    if isempty(f)
      m = 256;
    else
      m = size(f.Colormap,1);
    end
  else
    m = varargin{1};
  end
  varargout{1} = min(1,gray(m) * diag([1.2500 0.7812 0.4975]));
end
%=============================================================================
