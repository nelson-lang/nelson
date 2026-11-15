%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function setpixelposition(h, position, recursive)
  narginchk(2, 3);
  nargoutchk(0, 0);
  recursive = nargin > 2 && isRecursive(recursive);
  if isnumeric(h) && isempty(h)
    return
  end
  h = scalarObject(h);
  if ~(isnumeric(position) && isreal(position) && numel(position) == 4)
    error('Nelson:graphics:setpixelposition:invalidPosition', ...
      'A real four-element numeric vector is expected as second argument.');
  end
  if ~(isprop(h, 'Position') && numel(h.Position) == 4)
    error('Nelson:graphics:setpixelposition:noPosition', 'The object has no four-element Position.');
  end
  position = reshape(double(position), 1, 4);
  if recursive
    position = toParentFrame(h, position);
  end
  applyPixelPosition(h, position);
end
%=============================================================================
function tf = isRecursive(recursive)
  if ~(isscalar(recursive) && (islogical(recursive) || isnumeric(recursive)))
    error('Nelson:graphics:setpixelposition:invalidRecursive', 'A logical scalar is expected as third argument.');
  end
  tf = logical(recursive);
end
%=============================================================================
function position = toParentFrame(h, position)
  % a position given in the figure becomes relative to the parent container
  parent = h.Parent;
  if isscalar(parent) && ~isgraphics(parent, 'figure') && ~isgraphics(parent, 'root')
    parentPosition = getpixelposition(parent, true);
    position(1:2) = position(1:2) - parentPosition(1:2);
  end
end
%=============================================================================
function applyPixelPosition(h, position)
  if ~isprop(h, 'Units') || strcmp(h.Units, 'pixels')
    h.Position = position;
    return
  end
  % the object converts the pixels to its own units when Units is restored
  units = h.Units;
  h.Units = 'pixels';
  try
    h.Position = position;
  catch
    e = lasterror();
    h.Units = units;
    error(e);
  end
  h.Units = units;
end
%=============================================================================
function h = scalarObject(h)
  % a scalar graphics object, or the number of an open figure
  if isnumeric(h) && isscalar(h) && isreal(h) && h >= 1 && h == fix(h)
    h = findall(groot, 'Type', 'figure', 'Number', double(h));
  end
  if ~(isscalar(h) && isgraphics(h))
    error('Nelson:graphics:setpixelposition:invalidHandle', ...
      'A valid scalar graphics object is expected as first argument.');
  end
end
%=============================================================================
