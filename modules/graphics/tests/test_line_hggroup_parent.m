%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
figure();
hg = hggroup();
l = line(1, 1, 'Parent', hg);
asserts.isequal(l.Parent, hg);
asserts.isequal(hg.Children, l);
%=============================================================================
figure();
hg = hggroup();
l = line('XData', 1, 'YData', 1, 'Parent', hg);
asserts.isequal(l.Parent, hg);
asserts.isequal(hg.Children, l);
%=============================================================================
figure();
hg = hggroup();
l = line(1, 1, 1, 'Parent', hg);
asserts.isequal(l.Parent, hg);
asserts.isequal(hg.Children, l);
close all;
%=============================================================================
