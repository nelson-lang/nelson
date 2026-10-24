%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
run([modulepath('mex'), '/examples/mex_engine_demo_1.m'])
filename = generated_executable;
if ispc()
  filename = [filename, '.exe'];
end
asserts.istrue(isfile(filename));
%=============================================================================
run([modulepath('mex'), '/examples/mex_engine_demo_2.m']);
filename = generated_executable;
if ispc()
  filename = [filename, '.exe'];
end
asserts.istrue(isfile(filename));
%=============================================================================
