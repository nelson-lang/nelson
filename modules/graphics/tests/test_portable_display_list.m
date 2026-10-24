%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
figureHandle = figure('Visible', 'off');
cleanupFigure = onCleanup(@() close(figureHandle)); %#ok<NASGU>
surf(peaks(24));
title('Portable plot');
displayList = __web_display_list__(figureHandle);
document = jsondecode(displayList);

asserts.istrue(length(displayList) > 100);
asserts.istrue(isstruct(document));
%=============================================================================
