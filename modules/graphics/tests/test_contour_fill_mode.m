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
% Fill is the reference-facing on/off view of FaceColor.
figure();
[M, h] = contour(peaks(20));
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
h.Fill = 'on';
asserts.isequal(char(h.Fill), 'on');
asserts.isequal(h.FaceColor, 'flat');
h.Fill = 'off';
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
% Setting FaceColor drives Fill in the other direction.
h.FaceColor = 'flat';
asserts.isequal(char(h.Fill), 'on');
h.FaceColor = 'none';
asserts.isequal(char(h.Fill), 'off');
%=============================================================================
