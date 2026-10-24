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
asserts.isequal(char(camproj(ax)), 'orthographic');
%=============================================================================
camproj(ax, 'perspective');
asserts.isequal(char(camproj(ax)), 'perspective');
asserts.isequal(char(get(ax, 'Projection')), 'perspective');
%=============================================================================
camproj(ax, 'orthographic');
asserts.isequal(char(camproj(ax)), 'orthographic');
%=============================================================================
% abbreviated projection values (shortest unambiguous leading substring)
camproj(ax, 'p');
asserts.isequal(char(camproj(ax)), 'perspective');
camproj(ax, 'o');
asserts.isequal(char(camproj(ax)), 'orthographic');
camproj(ax, 'pers');
asserts.isequal(char(camproj(ax)), 'perspective');
%=============================================================================
asserts.checkerror('camproj(gca(), ''bad'')', 'Projection must be ''orthographic'' or ''perspective''.');
asserts.checkerror('camproj(gca(), '''')', 'Projection must be ''orthographic'' or ''perspective''.');
%=============================================================================
close all;
%=============================================================================
