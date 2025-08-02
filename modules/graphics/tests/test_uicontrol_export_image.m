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


prefix = [tempdir(), 'test_uicontrol_export_image'];

name_1 = [prefix, '_1.png'];
saveas(hFig, name_1);
assert_istrue(isfile(name_1));

name_2 = [prefix, '_2.pdf'];
saveas(hFig, name_2);
assert_istrue(isfile(name_2));

name_3 = [prefix, '_3.svg'];
saveas(hFig, name_3);
assert_istrue(isfile(name_3));
