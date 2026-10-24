%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(identitySameName(42), 42);
asserts.isequal(readBeforeAssignSameName(13), 13);
asserts.isequal(cellStringSameName({'a', 'b'}), 'a\nb');
%=============================================================================
function value = identitySameName(value)
end
%=============================================================================
function value = readBeforeAssignSameName(value)
  if value ~= 13
    error('unexpected value');
  end
end
%=============================================================================
function value = cellStringSameName(value)
  if iscell(value)
    value = value(:)';
    joined = value{1};
    for k = 2:numel(value)
      joined = [joined, '\n', value{k}];
    end
    value = joined;
  end
end
%=============================================================================
