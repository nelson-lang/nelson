%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef polyshape
  properties
    Vertices = zeros(0, 2)
  end

  methods
    function obj = polyshape(varargin)
      if nargin == 0
        return
      end
      if nargin == 1
        P = varargin{1};
        if ~isnumeric(P) || ndims(P) ~= 2 || size(P, 2) ~= 2
          error('Nelson:polyshape:invalidInput', ...
            'A single input must be an N-by-2 matrix of vertices.');
        end
        obj.Vertices = double(P);
      else
        x = varargin{1};
        y = varargin{2};
        if ~isnumeric(x) || ~isnumeric(y) || numel(x) ~= numel(y)
          error('Nelson:polyshape:invalidInput', ...
            'X and Y must be numeric vectors of the same length.');
        end
        obj.Vertices = [x(:), y(:)];
      end
    end

    function a = area(obj)
      if size(obj.Vertices, 1) < 3
        a = 0;
      else
        a = polyarea(obj.Vertices(:, 1), obj.Vertices(:, 2));
      end
    end

    function p = perimeter(obj)
      v = obj.Vertices;
      if size(v, 1) < 2
        p = 0;
        return
      end
      d = diff([v; v(1, :)]);
      p = sum(sqrt(sum(d .^ 2, 2)));
    end

    function n = numsides(obj)
      n = size(obj.Vertices, 1);
    end

    function h = plot(obj, varargin)
      v = obj.Vertices;
      hp = patch('XData', v(:, 1), 'YData', v(:, 2), ...
        'FaceColor', [0 0.447 0.741], 'FaceAlpha', 0.35, ...
        'EdgeColor', [0 0.447 0.741], varargin{:});
      if nargout > 0
        h = hp;
      end
    end

    function obj = rotate(obj, theta, refpoint)
      % Rotate the polygon by theta degrees counterclockwise about refpoint
      % (default: the origin).
      if nargin < 3 || isempty(refpoint)
        refpoint = [0, 0];
      end
      if ~isnumeric(theta) || ~isscalar(theta)
        error('Nelson:polyshape:invalidAngle', 'Rotation angle must be a scalar.');
      end
      if ~isnumeric(refpoint) || numel(refpoint) ~= 2
        error('Nelson:polyshape:invalidRefPoint', 'Reference point must be a two-element vector.');
      end
      refpoint = refpoint(:)';
      R = [cosd(theta), sind(theta); -sind(theta), cosd(theta)];
      obj.Vertices = (obj.Vertices - refpoint) * R + refpoint;
    end

    function obj = translate(obj, varargin)
      % Translate the polygon by [dx dy] (or dx, dy given separately).
      if numel(varargin) == 1
        delta = varargin{1};
      else
        delta = [varargin{1}, varargin{2}];
      end
      if ~isnumeric(delta) || numel(delta) ~= 2
        error('Nelson:polyshape:invalidTranslation', 'Translation must be a two-element vector.');
      end
      obj.Vertices = obj.Vertices + delta(:)';
    end
  end
end
%=============================================================================
