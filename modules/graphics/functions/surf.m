%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surf(varargin)
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
  
  h = surface(inputArguments{:});
  
  if strcmp(nextplot, 'replaceall') || strcmp(nextplot, 'replace') 
    view(go, 3);
    grid(go, 'on');
  elseif strcmp(nextplot, 'replacechildren')
    view(go, 3);
  end
  
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
