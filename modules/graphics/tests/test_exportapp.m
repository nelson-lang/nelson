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
% exportapp writes a uifigure (with UI components) to an image file
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 400 300]);
uibutton(fig, 'Text', 'Hello', 'Position', [100 130 120 30]);
drawnow();
pngName = [tempname(), '.png'];
exportapp(fig, pngName);
assert_istrue(isfile(pngName));
img = imread(pngName);
assert_isequal(size(img, 1), 300);
assert_isequal(size(img, 2), 400);
rmfile(pngName);
%=============================================================================
% exportapp matches the getframe capture of the same figure
%=============================================================================
frame = getframe(fig);
pngName2 = [tempname(), '.png'];
exportapp(fig, pngName2);
assert_isequal(imread(pngName2), frame.cdata);
rmfile(pngName2);
%=============================================================================
% filename accepts a string scalar
%=============================================================================
pngName3 = [tempname(), '.png'];
exportapp(fig, string(pngName3));
assert_istrue(isfile(pngName3));
rmfile(pngName3);
delete(fig);
%=============================================================================
% error paths
%=============================================================================
assert_checkerror('exportapp()', getString(message('Nelson:narginchk:notEnoughInputs')));
assert_checkerror('exportapp(42, ''out.png'')', 'First argument must be a single figure handle.');
fig2 = uifigure('Visible', 'off');
assert_checkerror('exportapp(fig2)', getString(message('Nelson:narginchk:notEnoughInputs')));
assert_checkerror('exportapp(fig2, 42)', 'Second argument must be a filename as a character vector or string scalar.');
delete(fig2);
close all
%=============================================================================
