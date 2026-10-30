%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
gioFigureVisible = 'off';
gioAnimationEnabled = true;
gioFramePause = 0;
gioAnimationContinuous = false;
gioGrid = 3;
gioThumbSize = 80;
run([modulepath('graphics_io', 'root'), '/examples/contact_sheet.m']);

assert_istrue(isgraphics(gioFigure));
assert_istrue(isgraphics(gioImage));
% The contact sheet holds a grid of square thumbnails.
assert_isequal(size(gioSheet), [gioGrid * gioThumbSize ...
  gioGrid * gioThumbSize 3]);
assert_isequal(class(gioSheet), 'uint8');
% imwrite produced the PNG files that imread then loaded into the sheet.
assert_istrue(isfile([gioDirectory, filesep(), 'frame_01.png']));
assert_istrue(sum(double(gioSheet(:))) > 0);
delete(gioFigure);
drawnow();
%=============================================================================
