%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = __struct_isequal(a, b)
  r = false;
  if ~isstruct(a)
    return
  end
  if ~isstruct(b)
    return
  end
  fieldA = sort(fieldnames(a));
  fieldB = sort(fieldnames(b));
  if isequalto(fieldA, fieldB)
    if isequalto(size(a), size(b))
      r = true;
      for f = fieldA(:)'
        for l = 1:length(a)
          valueA = getfield(a(l), f{:});
          valueB = getfield(b(l), f{:});
          if ~isequal(valueA, valueB)
            r = false;
            break;
          end
        end
      end
    end
  end
end
%=============================================================================
