%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
run([modulepath('mex'), '/examples/mex_engine_demo_1.m'])
filename = [tempdir(), 'mex_engine_demo_1/mex_engine_demo_1'];
if ispc()
  filename = [filename, '.exe'];
end
assert_istrue(isfile(filename));
%=============================================================================
run([modulepath('mex'), '/examples/mex_engine_demo_2.m']);
filename = [tempdir(), 'mex_engine_demo_2/mex_engine_demo_2'];
if ispc()
  filename = [filename, '.exe'];
end
assert_istrue(isfile(filename));
%=============================================================================
