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
skip_if_portable_graphics_backend('native tab order focus traversal is not available.');
% Tab moves the keyboard focus between the uicontrols of a figure in creation
% order (the reverse of Children), entering a panel where it stands, skipping
% static text, disabled and hidden controls, and wrapping around; Shift+Tab
% goes the other way.
%=============================================================================
f = figure('Position', [100 100 400 300]);
a = uicontrol(f, 'Style', 'pushbutton', 'String', 'a', 'Position', [10 250 60 20]);
uicontrol(f, 'Style', 'text', 'String', 'label', 'Position', [80 250 60 20]);
p = uipanel(f, 'Position', [0.3 0.1 0.4 0.5]);
b = uicontrol(p, 'Style', 'edit', 'Position', [10 10 60 20]);
c = uicontrol(p, 'Style', 'checkbox', 'String', 'c', 'Position', [10 40 60 20]);
d = uicontrol(f, 'Style', 'pushbutton', 'String', 'd', 'Position', [10 200 60 20]);
uicontrol(f, 'Style', 'pushbutton', 'String', 'off', 'Enable', 'off', 'Position', [10 170 60 20]);
uicontrol(f, 'Style', 'pushbutton', 'String', 'hidden', 'Visible', 'off', 'Position', [10 140 60 20]);
drawnow();
uicontrol(a);
drawnow();
[delivered, focused] = __uicontrol_key_event__(a, 'tab');
assert_istrue(delivered);
assert_isequal(focused, b);
[~, focused] = __uicontrol_key_event__(b, 'tab');
assert_isequal(focused, c);
[~, focused] = __uicontrol_key_event__(c, 'tab');
assert_isequal(focused, d);
% the last control wraps around to the first one
[~, focused] = __uicontrol_key_event__(d, 'tab');
assert_isequal(focused, a);
[~, focused] = __uicontrol_key_event__(a, 'shift+tab');
assert_isequal(focused, d);
[~, focused] = __uicontrol_key_event__(b, 'shift+tab');
assert_isequal(focused, a);
%=============================================================================
% the order follows Children (most recent first): listing the panel before d
% and d before a gives the Tab order a, d, then the panel content
children = f.Children;
keep = true(numel(children), 1);
for k = 1:numel(children)
  keep(k) = ~isequal(children(k), a) && ~isequal(children(k), d) && ~isequal(children(k), p);
end
others = children(keep);
f.Children = [others; p; d; a];
drawnow();
[~, focused] = __uicontrol_key_event__(a, 'tab');
assert_isequal(focused, d);
[~, focused] = __uicontrol_key_event__(d, 'tab');
assert_isequal(focused, b);
[~, focused] = __uicontrol_key_event__(c, 'tab');
assert_isequal(focused, a);
close(f);
%=============================================================================
