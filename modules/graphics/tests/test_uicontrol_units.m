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
f = figure();
h = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Units', 'pixels');
pixelPosition = [100, 100, 100, 50];
h.Position = pixelPosition;
asserts.isequal(h.Position, pixelPosition);
asserts.isequal(h.Units, 'pixels');
h.Units = 'normalized';
asserts.isequal(h.Units, 'normalized');
normalizedPosition = h.Position;
asserts.istrue(all(normalizedPosition > 0));
h.Units = 'pixels';
asserts.isequal(h.Units, 'pixels');
asserts.isapprox(h.Position, pixelPosition, 1e-3);
%=============================================================================
f = figure();
h = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Units', 'normalized');
h.Position = normalizedPosition;
asserts.isequal(h.Units, 'normalized');
asserts.isapprox(h.Position, normalizedPosition, 1e-3);
h.Units = 'pixels';
asserts.isequal(h.Units, 'pixels');
asserts.isapprox(h.Position, pixelPosition, 1e-3);
%=============================================================================
