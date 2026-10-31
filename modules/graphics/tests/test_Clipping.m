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
% default Clipping is 'on': data entirely outside the limits is clipped away
assert_isequal(char(h.Clipping), 'on');
ylim(ax, [100 200]);
dl = char(__web_display_list__(f));
assert_isequal(numel(strfind(dl, '"kind":"polyline"')), 0);
%=============================================================================
% Clipping 'off' lets the line draw outside the limits
h.Clipping = 'off';
ylim(ax, [100 200]);
dl = char(__web_display_list__(f));
assert_istrue(numel(strfind(dl, '"kind":"polyline"')) >= 1);
%=============================================================================
close(f);
%=============================================================================
