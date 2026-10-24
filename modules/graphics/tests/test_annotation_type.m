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
% The Type property of annotation objects reports the shape name, distinct
% from the constructor name passed to annotation(...).
%=============================================================================
f = figure('Visible', 'off');
%=============================================================================
hLine = annotation(f, 'line', [0.05 0.20], [0.95 0.95]);
asserts.isequal(hLine.Type, 'lineshape');
hArrow = annotation(f, 'arrow', [0.05 0.20], [0.85 0.85]);
asserts.isequal(hArrow.Type, 'arrowshape');
hDoubleArrow = annotation(f, 'doublearrow', [0.05 0.20], [0.75 0.75]);
asserts.isequal(hDoubleArrow.Type, 'doubleendarrowshape');
hTextArrow = annotation(f, 'textarrow', [0.05 0.20], [0.65 0.65], 'String', 'a');
asserts.isequal(hTextArrow.Type, 'textarrowshape');
hRectangle = annotation(f, 'rectangle', [0.30 0.80 0.10 0.10]);
asserts.isequal(hRectangle.Type, 'rectangleshape');
hEllipse = annotation(f, 'ellipse', [0.45 0.80 0.10 0.10]);
asserts.isequal(hEllipse.Type, 'ellipseshape');
hTextBox = annotation(f, 'textbox', [0.60 0.78 0.20 0.12], 'String', 'b');
asserts.isequal(hTextBox.Type, 'textboxshape');
%=============================================================================
% isgraphics resolves the shape Type, not the constructor name.
asserts.istrue(isgraphics(hArrow, 'arrowshape'));
asserts.isfalse(isgraphics(hArrow, 'arrow'));
asserts.istrue(isgraphics(hTextBox, 'textboxshape'));
%=============================================================================
% findobj matches on the shape Type.
asserts.istrue(any(findobj(f, 'Type', 'arrowshape') == hArrow));
asserts.istrue(any(findobj(f, 'Type', 'textboxshape') == hTextBox));
asserts.istrue(any(findobj(f, 'Type', 'lineshape') == hLine));
asserts.istrue(isempty(findobj(f, 'Type', 'arrow')));
%=============================================================================
% copyobj preserves the shape Type (constructor name is decoupled).
c = copyobj(hEllipse, f);
asserts.isequal(c.Type, 'ellipseshape');
delete(c);
%=============================================================================
close(f);
%=============================================================================
