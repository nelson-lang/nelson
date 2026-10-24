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
close all
f = figure('Visible', 'off');
plot(1:10);
F = getframe(gca());
axesFrameSize = size(F.cdata);
F = getframe();
asserts.isequal(size(F.cdata), axesFrameSize);
asserts.isequal(axesFrameSize(3), 3);
close(f);
%=============================================================================
