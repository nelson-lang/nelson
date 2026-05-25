classdef ClassdefPoint
  properties
    X = 0
    Y = 0
    Label = 'point'
  end

  properties (Constant)
    Dimension = 2
  end

  events
    Moved
  end

  methods
    function obj = ClassdefPoint(x, y)
      if nargin > 0
        obj.X = x;
        obj.Y = y;
      end
    end

    function r = magnitude(obj)
      r = sqrt(obj.X * obj.X + obj.Y * obj.Y);
    end

    function obj = shift(obj, dx, dy)
      obj.X = obj.X + dx;
      obj.Y = obj.Y + dy;
    end
  end

  methods (Static)
    function obj = origin()
      obj = ClassdefPoint(0, 0);
    end
  end
end
