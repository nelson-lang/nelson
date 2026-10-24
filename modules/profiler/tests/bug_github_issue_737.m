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
% https://github.com/nelson-lang/nelson/issues/737
% <-- Short Description -->
% profiler failed with 'sind' example.
%=============================================================================
profile on
sind(5)
profile off
p = tempname();
profsave(profile('info'), p)
asserts.istrue(isfile([p, '/index.html']))
rmdir(p, 's')
%=============================================================================
