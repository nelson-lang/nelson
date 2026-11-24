%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
addpath([modulepath('graphics_io'), '/tests/helpers']);
%=============================================================================
asserts.isequal(nargin('warndlg'), 3);
asserts.isequal(nargout('warndlg'), -1);
%=============================================================================
h = warndlg();
asserts.isequal(class(h), 'nelson.ui.Figure');
asserts.istrue(isgraphics(h, 'figure'));
children = allchild(h);
asserts.isequal(class(children), 'nelson.graphics.Graphics');
asserts.isfalse(strcmp(class(children(1)), 'graphics_object'));
asserts.istrue(isa(children(1), 'nelson.graphics.Graphics'));
asserts.isequal(h.Tag, 'Msgbox_Warning Dialog');
button = findobj(h, 'Tag', 'OKButton');
asserts.isequal(class(button), 'nelson.ui.control.UIControl');
message = findobj(h, 'Tag', 'MessageBox');
asserts.isequal(class(message), 'nelson.graphics.primitive.Text');
asserts.isequal(message.String, 'This is the default warning.');
icon = findobj(h, 'Tag', 'IconAxes');
asserts.isequal(class(icon), 'nelson.graphics.axis.Axes');
asserts.istrue(isgraphics(icon, 'axes'));
image = findobj(icon, 'Type', 'image');
asserts.isequal(class(image), 'nelson.graphics.primitive.Image');
asserts.isequal(numel(image), 1);
close(h);
%=============================================================================
h = warndlg('warning string', 'warning title');
asserts.istrue(isgraphics(h, 'figure'));
asserts.isequal(h.Name, 'warning title');
close(h);
%=============================================================================
svgFile = [tempdir(), 'warndlg_export.svg'];
pngFile = [tempdir(), 'warndlg_export.png'];
h = warndlg('Check the input value.', 'Warning', 'non-modal');
drawnow();
qtImage = getframe(h).cdata;
saveas(h, svgFile);
saveas(h, pngFile);
close(h);
svgInfo = assert_valid_svg(svgFile, [], 'path|image|polygon');
asserts.istrue(contains(svgInfo.text, '<svg'));
exportedImage = imread(pngFile);
asserts.istrue(size(exportedImage, 2) >= size(qtImage, 2));
asserts.istrue(size(exportedImage, 1) >= size(qtImage, 1));
renderedImages = {exportedImage};
for k = 1:numel(renderedImages)
  renderedImage = renderedImages{k};
  warningPixels = renderedImage(:, :, 1) > 150 & renderedImage(:, :, 2) > 40 ...
    & renderedImage(:, :, 2) < 200 & renderedImage(:, :, 3) < 100;
  asserts.istrue(nnz(warningPixels) > 20);
  rowCounts = sum(warningPixels, 2);
  activeRows = find(rowCounts > 0);
  middleRow = floor((activeRows(1) + activeRows(end)) / 2);
  topPixels = sum(rowCounts(activeRows(1):middleRow));
  bottomStart = middleRow + 1;
  bottomPixels = sum(rowCounts(bottomStart:activeRows(end)));
  asserts.istrue(bottomPixels > topPixels);
end
%=============================================================================
asserts.checkerror('warndlg(''a'', ''b'', ''c'', ''d'')', _('Too many input arguments.'));
%=============================================================================
