%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cla(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if (nargin == 0)
    ax = gca();
  else 
    ax = varargin{1};
  end
  ax.Children = [];
  ax.XLim = [0 1];
  ax.YLim = [0 1];
  ax.XLimMode = 'auto';
  ax.YLimMode = 'auto';
  ax.ZLimMode = 'auto';
  ax.XScale = 'linear';
  ax.YScale = 'linear';
  ax.ZScale = 'linear';
  ax.XTickMode = 'auto';
  ax.YTickMode = 'auto';
  ax.ZTickMode = 'auto';
  ax.XTickLabelMode = 'auto';
  ax.YTickLabelMode = 'auto';
  ax.ZTickLabelMode = 'auto';
  ax.ColorOrderIndex = 1;
  ax.LineStyleOrderIndex = 1;
  if nargout == 1
    varargout{1} = ax;
  else
    varargout = {};
  end
end
%=============================================================================
