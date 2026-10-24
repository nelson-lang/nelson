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
% <--GUI MODE-->
% A slider's thickness is fixed: the height of a horizontal slider (and the width
% of a vertical one) stays at 3. Setting another value warns and resets it.
%=============================================================================
fig = uifigure('Visible', 'off');
%=============================================================================
% Horizontal slider: default height 3, and a changed height is reset to 3.
%=============================================================================
s = uislider(fig);
assert_isequal(s.Position(4), 3);
lastwarn('');
s.Position = [50 50 200 40];
assert_isequal(s.Position, [50 50 200 3]);
[msg, id] = lastwarn();
assert_isfalse(isempty(msg));
assert_isequal(id, 'Nelson:ui:Slider:fixedHeight');
%=============================================================================
% Vertical slider: the width is fixed instead.
%=============================================================================
sv = uislider(fig, 'Orientation', 'vertical');
lastwarn('');
sv.Position = [50 50 40 200];
assert_isequal(sv.Position, [50 50 3 200]);
[msgv, idv] = lastwarn();
assert_isfalse(isempty(msgv));
assert_isequal(idv, 'Nelson:ui:Slider:fixedWidth');
%=============================================================================
% The warning carries an identifier, so it can be switched off (the thickness is
% still enforced; a disabled warning is still recorded by lastwarn).
%=============================================================================
warning('off', 'Nelson:ui:Slider:fixedHeight');
lastwarn('');
s.Position = [50 50 200 25];
assert_isequal(s.Position, [50 50 200 3]);
[~, idoff] = lastwarn();
assert_isequal(idoff, 'Nelson:ui:Slider:fixedHeight');
warning('on', 'Nelson:ui:Slider:fixedHeight');
delete(fig);
%=============================================================================
