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
% UI container default property values aligned with the reference
% (portable defaults only; FontName and Units are intentionally left as the
% Nelson cross-platform values).
%=============================================================================
close all
%=============================================================================
p = uipanel();
assert_isequal(p.FontUnits, 'points');
assert_isequal(p.FontSize, 8);
assert_isequal(char(p.AutoResizeChildren), 'off');
close all
%=============================================================================
bg = uibuttongroup();
assert_isequal(bg.FontUnits, 'points');
assert_isequal(bg.FontSize, 8);
assert_isequal(char(bg.AutoResizeChildren), 'off');
assert_isapprox(bg.BorderColor, [1 1 1], 1e-12);
assert_isapprox(bg.HighlightColor, [1 1 1], 1e-12);
close all
%=============================================================================
t = uitable();
assert_isequal(t.FontUnits, 'points');
assert_isequal(t.FontSize, 8);
close all
%=============================================================================
tg = uitabgroup();
assert_isequal(char(tg.AutoResizeChildren), 'off');
tb = uitab(tg);
assert_isequal(char(tb.AutoResizeChildren), 'off');
close all
%=============================================================================
