%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = newplot(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  
  gr = groot();
  fig = gr.CurrentFigure;
  if (isempty (fig))
    fig = figure();
  end
  if (nargin() == 1)
    ax = varargin{1};
    if isempty(ax)
      ax = gca();
    end
  else
    ax = gca();
  end
  if (strcmp(ax.NextPlot, 'replace'))
    cla();
  end
  if (nargin() == 1)
    h = ax;
  else
    h = gca();
  end
  if (nargout == 1)
    varargout{1} = h;
  end
end
%=============================================================================
