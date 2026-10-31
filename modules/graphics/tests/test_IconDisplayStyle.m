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
h1 = plot(ax, 1:10, 'DisplayName', 'one');
hold(ax, 'on');
h2 = plot(ax, 2:11, 'DisplayName', 'two');
h3 = plot(ax, 3:12, 'DisplayName', 'three');
%=============================================================================
% default IconDisplayStyle is 'on'
assert_isequal(char(h1.Annotation.LegendInformation.IconDisplayStyle), 'on');
%=============================================================================
% excluding h2 keeps only one and three in the automatic legend
h2.Annotation.LegendInformation.IconDisplayStyle = 'off';
lg = legend(ax);
assert_isequal(numel(lg.String), 2);
assert_isequal(lg.String{1}, 'one');
assert_isequal(lg.String{2}, 'three');
%=============================================================================
close(f);
%=============================================================================
