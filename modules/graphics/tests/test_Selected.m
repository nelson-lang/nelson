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
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = plot(ax, 1:10, 1:10);
%=============================================================================
% default Selected is 'off': no selection handles are drawn
assert_isequal(char(h.Selected), 'off');
d = char(__web_display_list__(f));
base = numel(strfind(d, '"kind":"polyline"'));
%=============================================================================
% Selected 'on' (with SelectionHighlight 'on') draws the selection handles
h.Selected = 'on';
h.Color = [1 0 0];
d = char(__web_display_list__(f));
assert_istrue(numel(strfind(d, '"kind":"polyline"')) > base);
%=============================================================================
% SelectionHighlight 'off' suppresses the handles even when Selected is 'on'
h.SelectionHighlight = 'off';
h.Color = [0 1 0];
d = char(__web_display_list__(f));
assert_isequal(numel(strfind(d, '"kind":"polyline"')), base);
%=============================================================================
close(f);
%=============================================================================
