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
% Default property values aligned with the numerical computing standard.
%=============================================================================
close all
%=============================================================================
% scatter / scatter3 / swarmchart: AlphaDataMapping default is 'scaled'
s = scatter(1:5, 1:5);
assert_isequal(s.AlphaDataMapping, 'scaled');
close all
s3 = scatter3(1:5, 1:5, 1:5);
assert_isequal(s3.AlphaDataMapping, 'scaled');
close all
sw = swarmchart(ones(1, 10), 1:10);
assert_isequal(sw.AlphaDataMapping, 'scaled');
close all
%=============================================================================
% text: AffectAutoLimits default is 'off'
t = text(0.5, 0.5, 'hi');
assert_isequal(char(t.AffectAutoLimits), 'off');
close all
%=============================================================================
% patch: LineJoin default is 'miter'
p = patch([0 1 1], [0 0 1], [1 0 0]);
assert_isequal(p.LineJoin, 'miter');
close all
%=============================================================================
% colorbar: Interruptible default is 'off'
plot(1:10);
cb = colorbar();
assert_isequal(char(cb.Interruptible), 'off');
close all
%=============================================================================
% errorbar: AlignVertexCenters default is 'on'
eb = errorbar(1:5, 1:5, ones(1, 5));
assert_isequal(char(eb.AlignVertexCenters), 'on');
close all
%=============================================================================
% annotation textarrow: HorizontalAlignment default is 'right'
ta = annotation('textarrow');
assert_isequal(ta.HorizontalAlignment, 'right');
close all
% annotation textbox keeps 'left'
tb = annotation('textbox', [.2 .2 .2 .2]);
assert_isequal(tb.HorizontalAlignment, 'left');
close all
%=============================================================================
% contour / contourf: LabelColor default is the dark-gray foreground
[~, hc] = contour(peaks(20));
assert_isapprox(hc.LabelColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
close all
%=============================================================================
