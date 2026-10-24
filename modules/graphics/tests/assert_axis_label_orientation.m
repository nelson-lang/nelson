%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function assert_axis_label_orientation(ax, name, rot, h, v)
  lbl = ax.(name);
  asserts.isequal(lbl.Rotation, rot);
  asserts.isequal(lbl.HorizontalAlignment, h);
  asserts.isequal(lbl.VerticalAlignment, v);
end
