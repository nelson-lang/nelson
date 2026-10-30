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
catDistFigureVisible = 'off';
catDistAnimationEnabled = true;
catDistAnimationFrames = 8;
catDistFramePause = 0;
catDistAnimationContinuous = false;
catDistSampleSize = 300;
run([modulepath('categorical', 'root'), ...
  '/examples/category_distribution.m']);

assert_istrue(isgraphics(catDistFigure));
assert_isequal(numel(catDistBars), catDistCount);
% The last sample is a categorical array over the declared categories.
assert_isequal(class(sample), 'categorical');
assert_isequal(numel(categories(sample)), catDistCount);
% The per-category counts cover the whole sample.
counts = zeros(1, catDistCount);
for category = 1:catDistCount
  counts(category) = sum(sample == catDistNames{category});
end
assert_isequal(sum(counts), catDistSampleSize);
delete(catDistFigure);
drawnow();
%=============================================================================
