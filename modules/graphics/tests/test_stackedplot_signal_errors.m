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
[T] = graphics_stackedplot_fixture();
t = 0:0.01:10;
y1 = sin(2 * pi * 0.5 * t);
y2 = cos(2 * pi * 0.3 * t) .* exp(-0.1 * t);
y3 = sin(2 * pi * 0.8 * t) + 0.3 * sin(2 * pi * 3 * t);
%=============================================================================
didError = false;
try
  s = stackedplot(t, [y1' y2' y3']);
  s.LineProperties.Color = [0.47; 0.67; 0.19];
catch
  didError = true;
end
asserts.istrue(didError);
close all;
%=============================================================================
didError = false;
try
  stackedplot({1, 2, 3});
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
didError = false;
try
  stackedplot(T, {'missing'});
catch
  didError = true;
end
asserts.istrue(didError);
%=============================================================================
