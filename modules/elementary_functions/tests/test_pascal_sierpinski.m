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
sierpinskiFigureVisible = 'off';
sierpinskiAnimationEnabled = true;
sierpinskiAnimationFrames = 20;
sierpinskiFramePause = 0;
sierpinskiAnimationContinuous = false;
run([modulepath('elementary_functions', 'root'), ...
  '/examples/pascal_sierpinski.m']);

assert_istrue(isgraphics(sierpinskiFigure));
assert_istrue(isgraphics(sierpinskiImage));
% The triangle is the parity of the lower-triangular Pascal matrix.
triangle = mod(pascal(10, 1), 2);
assert_istrue(all(triangle(:) == 0 | triangle(:) == 1));
% Row 1 and the diagonal of Pascal's triangle are always odd.
assert_istrue(all(diag(triangle) == 1));
delete(sierpinskiFigure);
drawnow();
%=============================================================================
