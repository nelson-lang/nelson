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
t = tiledlayout(2, 2);
nexttile;
plot(1:10, sin(1:10));
t.TileSpacing = 'compact';
t.Padding = 'compact';
asserts.isequal(t.TileSpacing, 'compact');
asserts.isequal(t.Padding, 'compact');
axCurrent = gca();
posBefore = axCurrent.OuterPosition;
topBefore = posBefore(2) + posBefore(4);
t.Subtitle.String = 'An Insightful Subtitle';
t.Subtitle.FontAngle = 'italic';
posAfter = axCurrent.OuterPosition;
topAfter = posAfter(2) + posAfter(4);
asserts.isequal(t.Subtitle.String, 'An Insightful Subtitle');
asserts.isequal(t.Subtitle.FontAngle, 'italic');
asserts.istrue(topAfter < topBefore);
close all;
%=============================================================================
