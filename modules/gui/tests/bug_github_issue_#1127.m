%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/121
% <-- Short Description -->
% Nelson could crash if an mxn characters is displayed in the variable browser.
%=============================================================================
% <--GUI MODE-->
A = 'g';
B = 'Test';
C = [B;B]
disp(C);