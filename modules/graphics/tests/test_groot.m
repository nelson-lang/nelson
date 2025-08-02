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
g = groot();
R =  properties(g);
REF = {'Children';
'CurrentFigure'; 
'Parent';
'PointerLocation';
'ScreenDepth';
'ScreenPixelsPerInch';
'ScreenSize';
'Tag';
'Type';
'Units';
'UserData'};
assert_isequal(R, REF);
%=============================================================================
assert_isequal(g.Type, 'root');
assert_isequal(g.Tag, '');
assert_isequal(g.UserData, []);
assert_isequal(g.Units, 'pixels');
%=============================================================================
