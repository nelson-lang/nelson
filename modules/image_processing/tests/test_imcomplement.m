%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
J = imcomplement(int16([-32768 0 32767]));
asserts.isequal(J, int16([32767 -1 -32768]));
I = peaks(16);
J = imcrop(I, [4 4 7 7]);
asserts.isequal(size(J), [8 8]);
