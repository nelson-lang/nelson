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
% saveas and getframe of a hidden figure go through the software display list:
% the tab bar of a uitabgroup must be drawn there (bar background, one label
% per tab, the selected one on its page color), 23 pixels along the top or the
% bottom of the group.
%=============================================================================
close all
%=============================================================================
group = [51 41 200 150];
barHeight = 23;
locations = {'top', 'bottom'};
for useUIFigure = [false true]
  for l = 1:numel(locations)
    for useSaveAs = [false true]
      if useUIFigure
        f = uifigure('Visible', 'off', 'Position', [100 100 400 300], 'Color', [1 1 1]);
      else
        f = figure('Visible', 'off', 'Position', [100 100 400 300], 'Color', [1 1 1], ...
          'MenuBar', 'none', 'ToolBar', 'none');
      end
      tg = uitabgroup(f, 'Units', 'pixels', 'Position', group, 'TabLocation', locations{l});
      uitab(tg, 'Title', 'Tab', 'BackgroundColor', [1 0 0]);
      uitab(tg, 'Title', 'Second', 'BackgroundColor', [0 0 1]);
      drawnow();
      if useSaveAs
        filename = [tempname(), '.png'];
        saveas(f, filename);
        img = imread(filename);
        delete(filename);
      else
        frame = getframe(f);
        img = frame.cdata;
      end
      close(f);
      % saveas may scale the figure: pixel coordinates follow the image size
      H = size(img, 1);
      sx = size(img, 2) / 400;
      sy = H / 300;
      left = group(1) - 1;
      if strcmp(locations{l}, 'top')
        barBottom = group(2) - 1 + group(4) - barHeight;
      else
        barBottom = group(2) - 1;
      end
      rows = (H - floor((barBottom + barHeight - 3) * sy)):(H - ceil((barBottom + 3) * sy));
      cols = (ceil((left + 3) * sx)):(floor((left + group(3) - 3) * sx));
      bar = double(img(rows, cols, :));
      r = bar(:, :, 1); g = bar(:, :, 2); b = bar(:, :, 3);
      % bar background on the right of the labels
      free = bar(:, (end - round(20 * sx)):end, :);
      assert_istrue(all(abs(free(:) - 224) <= 3));
      % the selected tab stands out on its page color, at the start of the bar
      selected = r > 200 & g < 60 & b < 60;
      assert_istrue(all(all(selected(:, 1:round(4 * sx)))));
      % the other tab keeps the bar color: its page color is not in the bar
      assert_isfalse(any(any(b > 200 & r < 60 & g < 60)));
      % one label per tab: dark text on the selected label and on the next one
      dark = r < 110 & g < 110 & b < 110;
      textColumns = find(any(dark, 1));
      selectedColumns = find(any(selected, 1));
      assert_istrue(any(textColumns <= max(selectedColumns)));
      assert_istrue(any(textColumns > max(selectedColumns) + round(6 * sx)));
      % the bar is 23 pixels thick: the strip beyond it is not bar colored
      if strcmp(locations{l}, 'top')
        outside = H - round((barBottom + barHeight + 4) * sy);
      else
        outside = H - round((barBottom - 4) * sy);
      end
      assert_istrue(all(all(img(outside, cols, :) > 250)));
    end
  end
end
%=============================================================================
% the live web page shows the header as HTML controls: nothing of it is painted
% in its canvas, a title only appears in the items of the control
f = figure('Visible', 'off', 'Position', [100 100 400 300], 'Color', [1 1 1]);
tg = uitabgroup(f, 'Units', 'pixels', 'Position', group);
uitab(tg, 'Title', 'Tab');
uitab(tg, 'Title', 'Second');
drawnow();
live = __web_display_list__(f);
close(f);
assert_isequal(numel(strfind(live, 'Second')), 1);
assert_istrue(contains(live, '"items":["Tab","Second"]'));
%=============================================================================
