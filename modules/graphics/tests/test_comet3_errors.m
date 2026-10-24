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
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
gotError = false;
try
  comet3(ax, 1:3, 1:3);
catch
  gotError = true;
end
asserts.istrue(gotError);
gotError = false;
try
  comet3(ax, 1:2, 1:3, 1:3);
catch
  gotError = true;
end
asserts.istrue(gotError);
gotError = false;
try
  comet3(ax, 1:3, 1:3, 1:3, 1);
catch
  gotError = true;
end
asserts.istrue(gotError);
gotError = false;
try
  comet3(ax, 1:3, 1:3, 1:3, -0.1);
catch
  gotError = true;
end
asserts.istrue(gotError);
close(f);
close all
%=============================================================================
