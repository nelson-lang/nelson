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
% '|' is a literal character in the text of UI components. Only the legacy
% uicontrol String reads it as an entry separator.
%=============================================================================
fig = uifigure('Visible', 'off');
%=============================================================================
lbl = uilabel(fig, 'Text', 'A | B | C');
assert_isequal(lbl.Text, 'A | B | C');
lbl.Text = '493.88 Hz   |   MIDI 71';
assert_isequal(lbl.Text, '493.88 Hz   |   MIDI 71');
lbl.Text = {'a|b'; 'c'};
assert_isequal(lbl.Text, {'a|b'; 'c'});
%=============================================================================
btn = uibutton(fig, 'Text', 'Play | Stop');
assert_isequal(btn.Text, 'Play | Stop');
%=============================================================================
cb = uicheckbox(fig, 'Text', 'x|y');
assert_isequal(cb.Text, 'x|y');
%=============================================================================
bg = uibuttongroup(fig);
rb = uiradiobutton(bg, 'Text', 'on|off');
assert_isequal(rb.Text, 'on|off');
%=============================================================================
ta = uitextarea(fig, 'Value', 'left | right');
assert_isequal(ta.Value, {'left | right'});
%=============================================================================
dd = uidropdown(fig, 'Items', {'a|b', 'c'});
assert_isequal(dd.Items, {'a|b', 'c'});
%=============================================================================
% uicontrol String keeps the '|' delimited reading.
f = figure('Visible', 'off');
pm = uicontrol(f, 'Style', 'popupmenu', 'String', 'one|two|three');
assert_isequal(pm.String, {'one'; 'two'; 'three'});
close(f);
%=============================================================================
delete(fig);
%=============================================================================
