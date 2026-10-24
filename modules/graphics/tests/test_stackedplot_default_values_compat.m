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
% stackedplot default property values aligned with the reference.
%=============================================================================
close all
%=============================================================================
h = stackedplot([1 2; 3 4; 5 6]);
assert_isequal(h.LegendOrientation, 'horizontal');
assert_isequal(h.LineStyle, '-');
assert_isequal(h.Marker, 'none');
assert_isequal(h.FontName, 'Helvetica');
assert_isequal(h.FontSize, 8);
assert_isapprox(h.Color, [0.066 0.443 0.745], 1e-12);
assert_isapprox(h.MarkerEdgeColor, [0.066 0.443 0.745], 1e-12);
close all
%=============================================================================
