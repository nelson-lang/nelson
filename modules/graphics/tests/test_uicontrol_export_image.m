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
hFig = figure('Position', [100, 100, 400, 300], 'Name', 'Simple GUI', 'NumberTitle', 'off');
surf(peaks(30));
% Create a slider
hSlider = uicontrol('Style', 'slider', ...
                        'Min', 0, 'Max', 100, ...
                        'Value', 50, ...
                        'Position', [50, 200, 300, 20], ...
                        'Callback', @slider_callback);

% Create a toggle button
hToggle = uicontrol('Style', 'togglebutton', ...
                        'String', 'Toggle', ...
                        'Position', [50, 150, 100, 40], ...
                        'Callback', @toggle_callback);

% Create a push button
hButton = uicontrol('Style', 'pushbutton', ...
                        'String', 'Press Me', ...
                        'Position', [200, 150, 100, 40], ...
                        'Callback', @button_callback);

% Create a list box
hListBox = uicontrol('Style', 'listbox', ...
                         'String', {'Option 1', 'Option 2', 'Option 3'}, ...
                         'Position', [50, 50, 100, 70], ...
                         'Callback', @listbox_callback);

% Create a static text
hText = uicontrol('Style', 'text', ...
                      'String', 'Hello, World!', ...
                      'Position', [200, 70, 150, 40]);

% Create a popup menu
hPopupMenu = uicontrol('Style', 'popupmenu', ...
                           'String', {'Choice 1', 'Choice 2', 'Choice 3'}, ...
                           'Position', [200, 110, 150, 30], ...
                           'Callback', @popupmenu_callback);


prefix = tempname();

name_1 = [prefix, '_1.png'];
saveas(hFig, name_1);
asserts.istrue(isfile(name_1));

name_2 = [prefix, '_2.pdf'];
saveas(hFig, name_2);
asserts.istrue(isfile(name_2));

name_3 = [prefix, '_3.svg'];
saveas(hFig, name_3);
asserts.istrue(isfile(name_3));
close(hFig);
%=============================================================================
% A figure whose only content is uicontrols must not export (or grab) as a
% blank canvas: the widgets have to be composited into the image. This guards
% the regression where uicontrols were missing from saveas/getframe output.
fWidgets = figure('Visible', 'off', 'Position', [100, 100, 300, 200]);
uicontrol(fWidgets, 'Style', 'pushbutton', 'String', 'Button', ...
                    'Position', [20, 150, 140, 30]);
uicontrol(fWidgets, 'Style', 'checkbox', 'String', 'Check', 'Value', 1, ...
                    'Position', [20, 100, 140, 30]);

name_widgets = [prefix, '_widgets.png'];
saveas(fWidgets, name_widgets);
asserts.istrue(isfile(name_widgets));
savedImage = imread(name_widgets);
savedBackground = reshape(double(squeeze(savedImage(1, 1, :))), 1, 1, 3);
savedDelta = abs(double(savedImage) - savedBackground);
asserts.istrue(sum(savedDelta(:) > 20) > 100);

frameStruct = getframe(fWidgets);
frameData = frameStruct.cdata;
frameBackground = reshape(double(squeeze(frameData(1, 1, :))), 1, 1, 3);
frameDelta = abs(double(frameData) - frameBackground);
asserts.istrue(sum(frameDelta(:) > 20) > 100);
close(fWidgets);
%=============================================================================
% A listbox must paint every visible entry stacked vertically, not just the
% first one, and highlight the selected entry. The listbox fills the figure so
% its rows map to the whole saved image.
fList = figure('Visible', 'off', 'Position', [100, 100, 200, 200]);
hList = uicontrol(fList, 'Style', 'listbox', ...
                  'String', {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'}, ...
                  'Position', [10, 10, 180, 180]);
set(hList, 'Value', 3);
name_list = [prefix, '_listbox.png'];
saveas(fList, name_list);
asserts.istrue(isfile(name_list));
listImage = imread(name_list);
% Dark pixels are the entry glyphs painted on the white box.
darkMask = all(listImage < 120, 3);
asserts.istrue(sum(darkMask(:)) > 20);
% Rows carrying glyphs: a single entry occupies about one glyph height, while
% eight stacked entries span many rows. A vertical extent several glyph heights
% tall proves more than one item's worth of content was drawn.
glyphRows = find(any(darkMask, 2));
textSpan = max(glyphRows) - min(glyphRows);
asserts.istrue(textSpan > 60);
% The selected entry sits on a neutral gray selection band (a wide run of
% mid-gray pixels distinct from the white box and the dark glyphs).
R = double(listImage(:, :, 1));
G = double(listImage(:, :, 2));
B = double(listImage(:, :, 3));
grayMask = (R > 180) & (R < 215) & (abs(R - G) < 10) & (abs(G - B) < 10);
asserts.istrue(sum(grayMask(:)) > 100);
close(fList);
rmfile(name_1);
rmfile(name_2);
rmfile(name_3);
rmfile(name_widgets);
rmfile(name_list);
