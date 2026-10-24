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
%=============================================================================
% uicontrol 'Enable' is a three-state property: 'on', 'off' or 'inactive'.
% 'inactive' shows the control looking normal but non-interactive, while its
% ButtonDownFcn still fires (kept for compatibility).
%=============================================================================
h = uicontrol();
asserts.isequal(h.Enable, 'on');
%=============================================================================
% 'inactive' is accepted through the dot-assignment set path.
h.Enable = 'inactive';
asserts.isequal(h.Enable, 'inactive');
%=============================================================================
% 'on' and 'off' still work, through set().
set(h, 'Enable', 'off');
asserts.isequal(h.Enable, 'off');
set(h, 'Enable', 'on');
asserts.isequal(h.Enable, 'on');
%=============================================================================
% 'inactive' is accepted at construction time, for every uicontrol style.
styles = {'pushbutton', 'togglebutton', 'checkbox', 'radiobutton', ...
  'edit', 'text', 'slider', 'popupmenu', 'listbox'};
for k = 1:numel(styles)
  hk = uicontrol('Style', styles{k}, 'Enable', 'inactive');
  asserts.isequal(hk.Enable, 'inactive');
end
%=============================================================================
% Any other value is still rejected.
asserts.checkerror('set(h, ''Enable'', ''foo'');', graphicsPropertyErrorMessage('Enable', 'Nelson:graphics:illegalSelection'));
%=============================================================================
% An inactive control renders without error in a real figure.
close all
fig = figure('Visible', 'off', 'Position', [100 100 220 120], 'Color', [1 1 1]);
u = uicontrol('Parent', fig, 'Style', 'pushbutton', 'String', 'BTN', ...
  'Enable', 'inactive', 'Position', [20 20 180 60]);
asserts.isequal(u.Enable, 'inactive');
if ~ismac()
  drawnow();
  frame = getframe(fig);
  asserts.istrue(~isempty(frame.cdata));
end
close(fig);
%=============================================================================
