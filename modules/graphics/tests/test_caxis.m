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
f = figure('Visible', 'off');
imagesc([1 2; 3 4]);
asserts.isequal(char(gca().Box), 'on');
asserts.isequal(gca().ZLim, [-1 1]);
asserts.isequal(gca().ZTick, [-1 0 1]);
asserts.isapprox(caxis(), [1 4], 1e-12);
caxis([0 5]);
asserts.isequal(gca().CLim, [0 5]);
asserts.isequal(caxis('mode'), 'manual');
caxis('auto');
asserts.isequal(caxis('mode'), 'auto');
close(f);
%=============================================================================
