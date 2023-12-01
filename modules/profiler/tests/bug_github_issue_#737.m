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
% https://github.com/nelson-lang/nelson/issues/737
% <-- Short Description -->
% profiler failed with 'sind' example.
%=============================================================================
profile on
sind(5)
profile off
p = [tempdir(), 'profile_results_',  createGUID()];
profsave(profile('info'), p)
assert_istrue(isfile([p,'/index.html']))
%=============================================================================
 