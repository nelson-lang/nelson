%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Axis-aligned bounds [xmin xmax ymin ymax zmin zmax] of a graphics object.
% For an axes the current limits are used, otherwise the object XData/YData/
% ZData (falling back to a zero Z range when the object is planar).
function b = __camObjectBounds__(h)
  if (isgraphics(h, 'axes'))
    xl = get(h, 'XLim');
    yl = get(h, 'YLim');
    zl = get(h, 'ZLim');
    b = [xl(1), xl(2), yl(1), yl(2), zl(1), zl(2)];
    return
  end
  xs = dataValues(h, 'XData');
  ys = dataValues(h, 'YData');
  zs = dataValues(h, 'ZData');
  if (isempty(xs) || isempty(ys))
    error('Nelson:graphics:camlookatNoData', 'Object has no data to look at.');
  end
  if (isempty(zs))
    zs = 0;
  end
  b = [min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)];
end
%=============================================================================
function v = dataValues(h, name)
  v = [];
  if (isprop(h, name))
    d = get(h, name);
    v = double(d(:));
    v = v(isfinite(v));
  end
end
%=============================================================================
