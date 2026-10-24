%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Minimal elliptical region of interest. It draws a filled ellipse on the axes;
% interactive editing is not supported (InteractionsAllowed defaults to 'none').
%=============================================================================
classdef Ellipse < handle
  properties
    Center = [0, 0];
    SemiAxes = [1, 1];
    RotationAngle = 0;
    Color = [0, 0, 1];
    FaceAlpha = 0.2;
    InteractionsAllowed = 'none';
    Parent = [];
  end
  properties (Access = private)
    PatchHandle = [];
  end
  methods
    function obj = Ellipse(varargin)
      args = varargin;
      ax = [];
      if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1})
        ax = args{1};
        args = args(2:end);
      end
      if mod(numel(args), 2) ~= 0
        error('Nelson:images:roi:Ellipse:nameValue', 'Name-value pair arguments expected.');
      end
      for k = 1:2:numel(args)
        name = lower(char(args{k}));
        value = args{k + 1};
        switch name
          case 'center'
            obj.Center = value(:)';
          case 'semiaxes'
            obj.SemiAxes = value(:)';
          case 'rotationangle'
            obj.RotationAngle = value;
          case 'color'
            obj.Color = value;
          case 'facealpha'
            obj.FaceAlpha = value;
          case 'interactionsallowed'
            obj.InteractionsAllowed = value;
          case 'parent'
            ax = value;
          otherwise
            % Other ROI properties are accepted and ignored by this minimal ROI.
        end
      end
      if isempty(ax)
        ax = gca();
      end
      obj.Parent = ax;
      obj.PatchHandle = drawEllipse(obj);
    end
  end
  methods (Access = private)
    function h = drawEllipse(obj)
      t = linspace(0, 2 * pi, 200);
      a = obj.SemiAxes(1);
      b = obj.SemiAxes(2);
      x0 = a * cos(t);
      y0 = b * sin(t);
      th = obj.RotationAngle * pi / 180;
      x = obj.Center(1) + x0 * cos(th) - y0 * sin(th);
      y = obj.Center(2) + x0 * sin(th) + y0 * cos(th);
      h = patch('Parent', obj.Parent, 'XData', x(:), 'YData', y(:), ...
        'FaceColor', obj.Color, 'EdgeColor', obj.Color, 'FaceAlpha', obj.FaceAlpha);
    end
  end
end
%=============================================================================
