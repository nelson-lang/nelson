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
% https://github.com/nelson-lang/nelson/issues/226
% <-- Short Description -->
% tempdir() did not include a final slash
%=============================================================================
T = tempdir()
assert_istrue(T(end) == '/' ||  T(end) == '\')
%=============================================================================
U = userdir()
assert_istrue(U(end) == '/' ||  U(end) == '\')
%=============================================================================

