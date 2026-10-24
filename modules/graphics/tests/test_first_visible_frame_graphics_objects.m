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
close all
addpath([modulepath('graphics', 'tests')]);

cases = {'plot', 'scatter', 'mesh', 'surf', 'imagesc'};
for k = 1:numel(cases)
  frame = first_visible_content_frame(cases{k});
  render_assert_not_blank(frame);
  assert_saturated_pixels(frame, cases{k});
end
close all
%=============================================================================
function frame = first_visible_content_frame(kind)
  f = figure('Visible', 'on');
  set(f, 'Position', [120 120 640 480]);
  axes;
  drawnow();
  switch kind
    case 'plot'
      plot([0 1], [0 1], 'r', 'LineWidth', 8);
      xlim([0 1]);
      ylim([0 1]);
    case 'scatter'
      scatter(0.5, 0.5, 1200, [1 0 0], 'filled');
      xlim([0 1]);
      ylim([0 1]);
    case 'mesh'
      h = mesh(peaks);
      set(h, 'EdgeColor', [1 0 0]);
    case 'surf'
      surf(peaks);
    case 'imagesc'
      imagesc(repmat([1 2; 3 4], 20, 20));
      colormap([1 0 0; 0 1 0; 0 0 1; 1 1 0]);
    otherwise
      error(_('Unexpected first visible frame case.'));
  end
  pause(0.2);
  frame = __getframe__(f, false);
  close(f);
end
%=============================================================================
function assert_saturated_pixels(frame, kind)
  maxChannel = max(frame, [], 3);
  minChannel = min(frame, [], 3);
  saturatedPixels = maxChannel > 120 & (maxChannel - minChannel) > 70;
  asserts.istrue(nnz(saturatedPixels) > 100, [kind, ' did not render visible colored content.']);
end
%=============================================================================
