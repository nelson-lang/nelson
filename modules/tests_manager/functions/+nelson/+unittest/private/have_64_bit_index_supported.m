%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_64_bit_index_supported()
  persistent have_64_bit_index %#ok<NLS0005>
  if isempty(have_64_bit_index)
    compiler_info = version('-compiler');
    have_64_bit_index = strcmp(compiler_info{3}, '64');
  end
  r = have_64_bit_index;
end
