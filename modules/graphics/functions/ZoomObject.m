%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ZoomObject < handle
  % Zoom mode object returned by zoom(fig) or zoom(ax) when an output is
  % requested. It designates the figure the zoom mode applies to and exposes
  % the usual interactive zoom properties.
  properties
    Figure = []
    Enable = 'off'
    Motion = 'both'
    Direction = 'in'
  end
  methods
    function obj = ZoomObject(fig)
      if nargin > 0
        obj.Figure = fig;
      end
    end
    %=========================================================================
    function setAxes3DPanAndZoomStyle(obj, ax, style)
      % setAxes3DPanAndZoomStyle(ax, style) stores the 3-D pan and zoom style
      % ('limits' or 'camera') on the given axes.
      narginchk(3, 3);
      if ~isgraphics(ax, 'axes')
        error('Nelson:zoom:invalidAxes', _('An axes handle is expected.'));
      end
      if ~(ischar(style) && isrow(style)) && ~isStringScalar(style)
        error('Nelson:zoom:invalidStyle', _('Style must be ''limits'' or ''camera''.'));
      end
      style = lower(convertStringsToChars(style));
      if ~any(strcmp(style, {'limits', 'camera'}))
        error('Nelson:zoom:invalidStyle', _('Style must be ''limits'' or ''camera''.'));
      end
      setappdata(ax, 'Axes3DPanAndZoomStyle', style);
    end
  end
end
%=============================================================================
