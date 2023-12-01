%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/809
% <-- Short Description -->
% add 'NumberTitle' property to figure graphics object.
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
assert_isequal(f.Name, '');
f.Name = 'Nelson';
assert_isequal(f.Name, 'Nelson');
assert_isequal(f.Visible, 'on');
f.Visible = 'off';
assert_isequal(f.Visible, 'off');
f.Visible = 'on';
assert_isequal(f.Visible, 'on');
assert_isequal(f.NumberTitle, 'on');
f.NumberTitle = 'off';
assert_isequal(f.NumberTitle, 'off');
f.NumberTitle = 'on';
assert_isequal(f.NumberTitle, 'on');
%=============================================================================
