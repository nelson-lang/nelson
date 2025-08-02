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
% https://github.com/nelson-lang/nelson/issues/39
% <-- Short Description -->
% inv([0 0;i() 3]) did not return [Inf, Inf; Inf, Inf] on ARM platform.
%=============================================================================
assert_isequal(inv([0 0;i() 3]), [Inf, Inf; Inf, Inf]);
assert_isequal(inv(single([0 0;i() 3])), single([Inf, Inf; Inf, Inf]));
%=============================================================================
