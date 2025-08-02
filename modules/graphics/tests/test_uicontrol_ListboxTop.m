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
f = figure('Position', [300, 300, 300, 200]);

% Create a list box with multiple items
items = {'Item 1', 'Item 2', 'Item 3', 'Item 4', 'Item 5', ...
         'Item 6', 'Item 7', 'Item 8', 'Item 9', 'Item 10'};
     
listbox = uicontrol('Style', 'listbox', ...
                    'Position', [50, 50, 200, 100], ...
                    'String', items);

for i = 10:-1:1
    listbox.ListboxTop = i;
    pause(1);
    assert_isequal(listbox.ListboxTop, i)
end
%=============================================================================
