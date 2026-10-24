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
  configuremsvc()
end
%=============================================================================
[status, compiler] = loadcompilerconf();
if ispc()
  asserts.istrue(islogical(status));
  asserts.istrue(strcmp(compiler, '') || strcmp(compiler, 'mingw') || strcmp(compiler, 'msvc'));
else
  asserts.isfalse(status);
  asserts.isequal(compiler, 'unix');
end
%=============================================================================
