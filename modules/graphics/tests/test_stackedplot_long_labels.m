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
s = stackedplot((1:5)', [(1:5)' (2:6)' (3:7)'], ...
  'DisplayLabels', {'Short', 'A very long stacked signal label', 'Third'});
% A long display label pushes the panel column to the right of the base
% left margin, and the labels stay in the left margin (negative x in axes
% coordinates, right aligned just left of the y-tick labels).
asserts.istrue(s.Children(1).Position(1) > 0.13);
asserts.istrue(s.Children(2).Children(end - 1).Position(1) < 0);
close all;
%=============================================================================
