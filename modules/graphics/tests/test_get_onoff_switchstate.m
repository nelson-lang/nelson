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
% A pure on/off property reads back as a nelson.lang.OnOffSwitchState
% enumeration (logical-valued), not a plain 'on'/'off' char.
%=============================================================================
f = figure('Visible', 'off');
c = onCleanup(@() close(f));
%=============================================================================
v = get(f, 'Visible');
assert_isequal(class(v), 'nelson.lang.OnOffSwitchState');
assert_istrue(islogical(v));
assert_isfalse(ischar(v));
assert_istrue(v == 'off');
assert_isequal(char(v), 'off');
assert_istrue(strcmp(v, 'off'));
assert_isequal(double(v), 0);
%=============================================================================
ax = axes('Parent', f);
assert_isequal(class(get(ax, 'Box')), 'nelson.lang.OnOffSwitchState');
assert_isequal(class(get(ax, 'XGrid')), 'nelson.lang.OnOffSwitchState');
assert_istrue(strcmp(get(ax, 'XGrid'), 'off'));
assert_istrue(get(ax, 'Visible') == 'on');
%=============================================================================
l = line('Parent', ax, 'XData', 1:3, 'YData', 1:3);
assert_isequal(class(get(l, 'Visible')), 'nelson.lang.OnOffSwitchState');
assert_istrue(get(l, 'Selected') == false);
assert_istrue(get(l, 'Clipping') == true);
%=============================================================================
% The struct form of get() carries the same enum-typed fields.
s = get(ax);
assert_isequal(class(s.Box), 'nelson.lang.OnOffSwitchState');
%=============================================================================
% HandleVisibility is on/off/callback, not a pure switch: it stays char.
assert_istrue(ischar(get(ax, 'HandleVisibility')));
%=============================================================================
% Setting back accepts the enum and the legacy char alike.
set(l, 'Visible', get(f, 'Visible'));
assert_istrue(get(l, 'Visible') == 'off');
set(l, 'Visible', 'on');
assert_istrue(get(l, 'Visible') == 'on');
%=============================================================================
