%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_c_cpp_compiler()
  persistent compiler_detected %#ok<NLS0005>
  if isempty(compiler_detected)
    compiler_detected = havecompiler();
  end
  r = compiler_detected;
end
