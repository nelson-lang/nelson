%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('dynamic_link'), '/examples/call_fortran.m']);
run([modulepath('dynamic_link'), '/examples/call_c.m']);
run([modulepath('dynamic_link'), '/examples/dllibinfo_demo.m']);
%=============================================================================