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
ax = gca();
ax.Visible = 'off';
t = title('Text visible', 'Color', 'm');
asserts.isequal(char(t.Visible), 'on');
ax.Visible = 'on';
asserts.isequal(char(t.Visible), 'on');
t.Visible = 'off';
asserts.isequal(char(t.Visible), 'off');
close all;
%=============================================================================
f = figure();
ax = gca();
ax.Visible = 'off';
t = title('Text not visible', 'Color', 'm', 'Visible', 'on');
asserts.isequal(char(t.Visible), 'on');
ax.Visible = 'off';
asserts.isequal(char(t.Visible), 'on');
close all;
%=============================================================================
