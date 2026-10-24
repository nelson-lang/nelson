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
close all
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
%=============================================================================
an = animatedline(ax, 1:2, 3:4, 5:6, 'MaximumNumPoints', 3);
addpoints(an, 7:10, 17:20);
[x, y, z] = getpoints(an);
asserts.isequal(x, [8 9 10]);
asserts.isequal(y, [18 19 20]);
asserts.isequal(z, [0 0 0]);
clearpoints(an);
[x, y, z] = getpoints(an);
asserts.istrue(isempty(x));
asserts.istrue(isempty(y));
asserts.istrue(isempty(z));
an.MaximumNumPoints = 2.2;
asserts.isequal(an.MaximumNumPoints, 3);
addpoints(an, 1:5, 11:15);
[x, y] = getpoints(an);
asserts.isequal(x, [3 4 5]);
asserts.isequal(y, [13 14 15]);
an.MaximumNumPoints = Inf;
asserts.istrue(isinf(an.MaximumNumPoints));
gotError = false;
try
  an.MaximumNumPoints = 0;
catch
  gotError = true;
end
asserts.istrue(gotError);
gotError = false;
try
  addpoints(an, 1:2, 1:3);
catch
  gotError = true;
end
asserts.istrue(gotError);
%=============================================================================
