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
% A graphics object in first position selects set/get even when a later
% argument is a value class object.
f = figure('Visible', 'off');
lp = StackedLineProperties();
set(f, 'UserData', lp);
asserts.isequal(class(get(f, 'UserData')), 'StackedLineProperties');
%=============================================================================
s = __stackedplot__('AutoParent', 'off');
set(s, 'LineProperties', lp);
asserts.isequal(class(s.LineProperties), 'StackedLineProperties');
%=============================================================================
s = stackedplot(1:3, rand(3, 2));
asserts.istrue(isgraphics(s, 'StackedLineChart'));
close(f);
%=============================================================================
