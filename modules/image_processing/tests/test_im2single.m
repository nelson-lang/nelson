%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
I = reshape(uint8(linspace(1, 255, 25)), [5 5]);
J = im2single(I);
REF = single(double(I) / 255);
asserts.isequal(J, REF);
