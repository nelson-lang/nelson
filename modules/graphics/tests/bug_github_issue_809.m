%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
asserts.isequal(f.Name, '');
f.Name = 'Nelson';
asserts.isequal(f.Name, 'Nelson');
asserts.isequal(char(f.Visible), 'on');
f.Visible = 'off';
asserts.isequal(char(f.Visible), 'off');
f.Visible = 'on';
asserts.isequal(char(f.Visible), 'on');
asserts.isequal(char(f.NumberTitle), 'on');
f.NumberTitle = 'off';
asserts.isequal(char(f.NumberTitle), 'off');
f.NumberTitle = 'on';
asserts.isequal(char(f.NumberTitle), 'on');
%=============================================================================
