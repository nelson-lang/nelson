%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all;
f = figure('Visible', 'off');
surf(peaks(20));
ax = gca();
%=============================================================================
p = campos();
asserts.isequal(size(p), [1 3]);
asserts.isequal(campos(ax), get(ax, 'CameraPosition'));
%=============================================================================
campos(ax, [10 20 30]);
asserts.isequal(campos(ax), [10 20 30]);
asserts.isequal(char(get(ax, 'CameraPositionMode')), 'manual');
%=============================================================================
campos(ax, 'auto');
asserts.isequal(char(campos(ax, 'mode')), 'auto');
campos(ax, 'manual');
asserts.isequal(char(campos(ax, 'mode')), 'manual');
%=============================================================================
R = campos(ax, [1 2 3]);
asserts.isequal(R, [1 2 3]);
%=============================================================================
asserts.checkerror('campos(gca(), ''bad'')', 'Value must be ''auto'', ''manual'' or ''mode''.');
%=============================================================================
close all;
%=============================================================================
