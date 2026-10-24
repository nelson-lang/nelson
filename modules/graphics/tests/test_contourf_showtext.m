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
figure();
[M, h] = contourf(peaks(20), 5, 'ShowText', 'on', 'TextList', [0 2], 'LabelFormat', @(x) sprintf('L%g', x));
asserts.isequal(M, h.ContourMatrix);
asserts.isequal(char(h.ShowText), 'on');
asserts.isequal(h.TextList, [0 2]);
asserts.isequal(class(h.LabelFormat), 'function_handle');
drawnow();
%=============================================================================
