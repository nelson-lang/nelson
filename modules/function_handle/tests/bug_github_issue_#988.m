%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/988
% <-- Short Description -->
% anonymous function serialization '.^' and '^' were inversed.
%=============================================================================
F1 = @(f) f .^2
assert_isequal(func2str(F1), '@(f)f.^2')
%=============================================================================
F2 = @(f) f ^2
assert_isequal(func2str(F2), '@(f)f^2')
%=============================================================================
