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
f = figure();
ax = gca();
%=============================================================================
% set then get
setappdata(ax, 'ball', [1 2 3]);
assert_isequal(getappdata(ax, 'ball'), [1 2 3]);
%=============================================================================
% isappdata
assert_isequal(isappdata(ax, 'ball'), true);
assert_isequal(isappdata(ax, 'missing'), false);
%=============================================================================
% getappdata on a missing name returns []
assert_isequal(getappdata(ax, 'missing'), []);
%=============================================================================
% arbitrary value types (struct)
s.x = 1;
s.y = 'abc';
setappdata(ax, 'cfg', s);
assert_isequal(getappdata(ax, 'cfg'), s);
%=============================================================================
% getappdata(h) returns all entries as a struct
all = getappdata(ax);
assert_isequal(sort(fieldnames(all)), sort({'ball'; 'cfg'}));
assert_isequal(all.ball, [1 2 3]);
%=============================================================================
% overwrite
setappdata(ax, 'ball', 42);
assert_isequal(getappdata(ax, 'ball'), 42);
%=============================================================================
% rmappdata
rmappdata(ax, 'ball');
assert_isequal(isappdata(ax, 'ball'), false);
assert_checkerror('rmappdata(gca, ''ball'')', _('The application-defined data does not exist.'));
%=============================================================================
% data is per-object
f2 = figure();
ax2 = gca();
setappdata(ax2, 'ball', 7);
setappdata(ax, 'ball', 9);
assert_isequal(getappdata(ax2, 'ball'), 7);
assert_isequal(getappdata(ax, 'ball'), 9);
%=============================================================================
