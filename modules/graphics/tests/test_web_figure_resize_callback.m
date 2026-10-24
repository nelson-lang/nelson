%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
f = figure('Visible', 'off', 'Position', [10 20 920 580]);
f.UserData = struct('count', 0, 'width', 0, 'height', 0);
f.SizeChangedFcn = @recordViewport;
position = f.Position;
handle = __web_graphics_handle__(f, 'number');
asserts.istrue(__web_figure_size__(handle, 595, 580));
% the SizeChangedFcn is queued by the resize and runs at the next drawnow()
asserts.isequal(f.UserData.count, 0);
drawnow();
asserts.isequal(f.UserData, struct('count', 1, 'width', 595, 'height', 580));
asserts.isequal(f.Position, position);
asserts.istrue(__web_figure_size__(handle, 595, 580));
drawnow();
asserts.isequal(f.UserData.count, 1);
asserts.isfalse(__web_figure_size__(handle, 0, 580));
drawnow();
asserts.isequal(f.UserData.count, 1);
close(f);

function recordViewport(source, event)
  state = source.UserData;
  state.count = state.count + 1;
  state.width = event.Width;
  state.height = event.Height;
  source.UserData = state;
end
