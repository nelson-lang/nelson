%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% Non-regression tests for true super/subscript positioning of the TeX
% interpreter (raised / lowered runs at a smaller font), verified through the
% text Extent so no browser or reference image is required.
%=============================================================================
% The String property keeps the raw markup; only the drawing is reinterpreted.
f = figure();
t = text(0.5, 0.5, 'x^2', 'Interpreter', 'tex');
asserts.isequal(t.String, 'x^2');
close(f);
%=============================================================================
% A superscript adds width (the exponent) and height (it is raised) versus the
% bare base character.
f = figure();
tb = text(0.2, 0.5, 'x', 'Interpreter', 'tex', 'FontSize', 20);
ts = text(0.6, 0.5, 'x^2', 'Interpreter', 'tex', 'FontSize', 20);
drawnow();
eb = tb.Extent;
es = ts.Extent;
asserts.istrue(es(3) > eb(3));
asserts.istrue(es(4) > eb(4));
close(f);
%=============================================================================
