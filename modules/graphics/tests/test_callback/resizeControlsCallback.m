%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function resizeControlsCallback(src, event)
  % Children run top first, so they are walked the other way up to lay the
  % controls out in the order they were added.
  children = flipud(src.Children(:));
  position = src.Position;
  controlWidth = max(20, floor((position(3) - 30) / 4));
  for k = 1:length(children)
    x = 10 + mod(k - 1, 4) * (controlWidth + 5);
    y = 10 + floor((k - 1) / 4) * 12;
    children(k).Position = [x, y, controlWidth, 10];
  end
  src.Tag = 'sizechanged';
end
%=============================================================================
