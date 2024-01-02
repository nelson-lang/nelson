%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mesh(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if ((length(inputArguments{1}) == 1) && isgraphics(inputArguments{1},'axes'))
      go = inputArguments{1}(1);
    else   
      go = newplot();
    end
  else
    go = newplot();
  end
  nextplot = go.NextPlot;
  
  args = {'FaceColor', 'w', 'EdgeColor', 'flat', 'FaceLighting', 'none', 'EdgeLighting', 'flat'};
  args = [inputArguments{:}, args];
  h = surface(go, args{:});
  
  if (~ishold(go))
    view(go, 3);
    grid(go, 'on');
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
