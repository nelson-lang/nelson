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
f = figure();
t = get(f, 'Theme');
assert_isequal(t.BaseColorStyle, 'light');
%=============================================================================
% apply dark theme to the current figure
theme('dark');
t = get(f, 'Theme');
assert_isequal(t.BaseColorStyle, 'dark');
c = get(f, 'Color');
assert_istrue(all(c < 0.5));
%=============================================================================
% apply light theme to an explicit figure handle
theme(f, 'light');
t = get(f, 'Theme');
assert_isequal(t.BaseColorStyle, 'light');
c = get(f, 'Color');
assert_istrue(all(c > 0.5));
%=============================================================================
% string scalar input
theme(f, "dark");
assert_isequal(get(f, 'Theme').BaseColorStyle, 'dark');
%=============================================================================
% output argument returns the applied theme
r = theme(f, 'light');
assert_isequal(r.BaseColorStyle, 'light');
%=============================================================================
% a theme object (struct with BaseColorStyle) can be reused
darkTheme = struct('Name', 'Dark Theme', 'BaseColorStyle', 'dark');
theme(f, darkTheme);
assert_isequal(get(f, 'Theme').BaseColorStyle, 'dark');
%=============================================================================
% applying a theme to an axes resolves to the parent figure
theme(f, 'light');
ax = axes('Parent', f);
theme(ax, 'dark');
assert_isequal(get(f, 'Theme').BaseColorStyle, 'dark');
%=============================================================================
assert_checkerror('theme()', getString(message('Nelson:narginchk:notEnoughInputs')));
assert_checkerror('theme(''blue'')', getString(message('Nelson:figure:invalidTheme')));
%=============================================================================
close(f);
%=============================================================================
