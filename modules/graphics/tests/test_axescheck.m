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
close all
%=============================================================================
% a non-handle leading argument gives an empty axes and keeps all arguments
[ax, args, nargs] = axescheck(5);
assert_istrue(isempty(ax));
assert_isequal(nargs, 1);
assert_isequal(args{1}, 5);
%=============================================================================
% a leading axes handle is pulled off
f = figure('Visible', 'off');
a = axes('Parent', f);
[ax, args, nargs] = axescheck(a, 1, 2);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(nargs, 2);
assert_isequal(args, {1, 2});
%=============================================================================
% a 'Parent' name-value pair supplies the axes and is removed
[ax, args, nargs] = axescheck(1, 2, 'Parent', a);
assert_istrue(isgraphics(ax, 'axes'));
assert_isequal(nargs, 2);
assert_isequal(args, {1, 2});
%=============================================================================
% no axes anywhere leaves an empty handle and all arguments
[ax, args, nargs] = axescheck('foo', 3);
assert_istrue(isempty(ax));
assert_isequal(nargs, 2);
close(f);
%=============================================================================
