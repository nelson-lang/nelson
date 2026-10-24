%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% stairs() returns a dedicated Stair chart primitive, not a reused Line.
h = stairs(1:5);
asserts.isequal(class(h), 'nelson.graphics.chart.primitive.Stair');
asserts.isequal(h.Type, 'stair');
asserts.istrue(isgraphics(h, 'stair'));
%=============================================================================
% Stair is a 2-D primitive: the line-series-only extras must be absent.
asserts.isfalse(isprop(h, 'ZData'));
asserts.isfalse(isprop(h, 'ZDataMode'));
asserts.isfalse(isprop(h, 'ThetaData'));
asserts.isfalse(isprop(h, 'RData'));
asserts.isfalse(isprop(h, 'LineJoin'));
asserts.isfalse(isprop(h, 'MarkerIndices'));
asserts.isfalse(isprop(h, 'AffectAutoLimits'));
asserts.isfalse(isprop(h, 'AlignVertexCenters'));
%=============================================================================
% The reference Stair property surface is present.
asserts.istrue(isprop(h, 'XData'));
asserts.istrue(isprop(h, 'YData'));
asserts.istrue(isprop(h, 'XDataMode'));
asserts.istrue(isprop(h, 'YDataMode'));
asserts.istrue(isprop(h, 'ColorMode'));
asserts.istrue(isprop(h, 'LineStyleMode'));
asserts.istrue(isprop(h, 'MarkerMode'));
asserts.istrue(isprop(h, 'SeriesIndex'));
asserts.istrue(isprop(h, 'DataTipTemplate'));
%=============================================================================
% The full surface matches expected behavior 42-property count.
p = properties(h);
asserts.isequal(numel(p), 42);
%=============================================================================
close all;
%=============================================================================
