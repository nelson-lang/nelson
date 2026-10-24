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
% Non-regression tests for true super/subscript positioning of the TeX
% interpreter (raised / lowered runs at a smaller font), verified through the
% text Extent so no browser or reference image is required.
%=============================================================================
% A superscript in a title / axis label goes through the same paint path.
f = figure();
plot(1:10, (1:10) .^ 2);
title('E = mc^2');
xlabel('t_0 (s)');
ylabel('v^2 (m/s)');
drawnow();
close(f);
%=============================================================================
