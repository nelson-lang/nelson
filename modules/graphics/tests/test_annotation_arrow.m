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
plot(1:10);
%=============================================================================
ar = annotation('arrow', 'Color', 'red', 'LineStyle', '--', 'HeadStyle', 'plain');
asserts.isequal(ar.Type, 'arrowshape');
asserts.isequal(ar.Color, [1 0 0]);
asserts.isequal(ar.LineStyle, '--');
asserts.isequal(ar.HeadStyle, 'plain');
asserts.isequal(ar.HeadLength, 10);
asserts.isequal(ar.HeadWidth, 10);
headStyles = {'plain', 'ellipse', 'vback1', 'vback2', 'vback3', 'cback1', 'cback2', 'cback3', ...
  'fourstar', 'rectangle', 'diamond', 'rose', 'hypocycloid', 'astroid', 'deltoid', 'none'};
for k = 1:numel(headStyles)
  ar.HeadStyle = headStyles{k};
  asserts.isequal(ar.HeadStyle, headStyles{k});
end
%=============================================================================
