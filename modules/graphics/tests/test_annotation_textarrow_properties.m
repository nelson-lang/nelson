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
headStyles = annotation_head_styles();
%=============================================================================
ta = annotation('textarrow', [0.2 0.5], [0.7 0.5], 'String', {'one', 'two'}, ...
  'TextRotation', 15, 'TextBackgroundColor', 'yellow', 'TextEdgeColor', 'black', ...
  'LineStyle', '-.');
asserts.isequal(ta.Type, 'textarrowshape');
asserts.isequal(ta.TextRotation, 15);
asserts.isequal(ta.TextBackgroundColor, [1 1 0]);
asserts.isequal(ta.TextEdgeColor, [0 0 0]);
asserts.isequal(ta.LineStyle, '-.');
asserts.isequal(ta.Interpreter, 'tex');
asserts.isequal(ta.TextMargin, 2);
for k = 1:numel(headStyles)
  ta.HeadStyle = headStyles{k};
  asserts.isequal(ta.HeadStyle, headStyles{k});
end
%=============================================================================
p = properties(ta);
REF = {'Color'; 'HeadLength'; 'HeadStyle'; 'HeadWidth'; 'HorizontalAlignment';
  'LineStyle'; 'LineWidth'; 'Position'; 'String'; 'TextBackgroundColor';
  'TextColor'; 'TextEdgeColor'; 'TextLineWidth'; 'TextMargin'; 'TextRotation';
  'Interpreter'; 'FontName'; 'FontSize'; 'FontWeight'; 'FontAngle'; 'FontUnits';
  'Units'; 'VerticalAlignment'; 'X'; 'Y'};
asserts.istrue(all(ismember(REF, p)));
%=============================================================================
