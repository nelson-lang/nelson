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
% https://github.com/nelson-lang/nelson/issues/533
% <-- Short Description -->
% find does not consider complex number correctly
%=============================================================================
R = find([1 i]);
REF = [1 2];
assert_isequal(R, REF)
%=============================================================================
R = find(single([1 i]));
REF = [1 2];
assert_isequal(R, REF)
%=============================================================================
